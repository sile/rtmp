(progn
  (require :sb-bsd-sockets)
  (use-package :sb-bsd-sockets)
  )

(defun make-server-socket (port)
  (let ((sock (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address sock) t)
    (socket-bind sock #(0 0 0 0) port)
    (socket-listen sock 64)
    sock))

(define-symbol-macro srv 
  (progn (when (boundp '*srv*) (socket-close *srv*))
	 (defparameter *srv* 
	   (make-server-socket 8080)
	   )
	 *srv*))

(defmacro with-accept ((client-io client server) &body body)
  `(let* ((,client (socket-accept ,server))
	  (,client-io (socket-make-stream ,client :input t :output t :element-type 'octet)))
     (unwind-protect
	 (locally ,@body)
       (socket-close ,client))))

(defun read-c0 (in)
  (let ((version (read-byte in)))
    version))

(defun read-c1 (in)
  (let ((timestamp (read-int4 in))
	(zero (read-int4 in))
	(random-bytes (read-bytes 1528 in)))
    (format t "~&; c1: zero=~a, timestamp=~a~%" zero timestamp)
    (values timestamp random-bytes)))

(defun read-c2 (in)
  (let ((ts1 (read-int4 in))
	(ts2 (read-int4 in))
	(bytes (read-bytes 1528 in)))
    (format t "~&; c2: timestamp1=~a, timestamp2=~a~%" ts1 ts2)
    (values ts1 ts2 bytes)))

(defun write-s0/s1 (out)
  (let ((write-version 3)
	(timestamp 7))
    (write-byte write-version out)

    (write-int4 timestamp out)
    (write-int4 0 out)
    (loop FOR i FROM 0 BELOW 1528 DO (write-byte (ldb (byte 8 0) i) out))
    (force-output out)
    
    timestamp))

(defun srv-handshake (io)
  (format t "~&; start: handshake~%")
  (let ((version (read-c0 io)))
    (format t "~&; c0=~a~%" version)
    
    (let ((send-timestamp (write-s0/s1 io)))
      (multiple-value-bind (timestamp random-bytes)
			   (read-c1 io)
        (write-s2 send-timestamp timestamp random-bytes io)
	(read-c2 io)
    ))))

(defun read-int (length in)
  (loop FOR i FROM (1- length) DOWNTO 0 
	SUM (ash (read-byte in) (* i 8))))

(defun srv-read-chunk-basic-header (in)
  (let* ((b1 (read-byte in))
	 (fmt (ldb (byte 2 6) b1)))
    (let ((chunk-stream-id
	   (case (ldb (byte 6 0) b1)
	     (0 (+ (read-byte in) 64))
	     (1 (+ (read-int 2 in) 64))
	     (otherwise (ldb (byte 6 0) b1)))))
      (values fmt chunk-stream-id))))

(defstruct chunk-msg
  chunk-stream-id
  timestamp
  msg-type-id
  msg-stream-id

  (timestamp-delta 0)
  total-msg-len
  (per-msg-len 128) ; XXX: default?
  
  payload)

(defun reverse-uint4 (n)
  (loop FOR i FROM 0 BELOW 4
	SUM (ash (ldb (byte 8 (* i 8)) n) (- 24 (* i 8)))))

(defun srv-read-chunk-msg-header (fmt chunk-msg in)
  (ecase fmt
    (0 (let ((timestamp (read-int 3 in))
	     (msg-len (read-int 3 in))
	     (msg-type-id (read-byte in))
	     (msg-stream-id (reverse-uint4 (read-int 4 in))))
	 (when (= msg-len #xFFFFFF)
	   (setf msg-len (read-int 4 in)))

	 (setf (chunk-msg-timestamp chunk-msg) timestamp
	       (chunk-msg-total-msg-len chunk-msg) msg-len
	       (chunk-msg-msg-type-id chunk-msg) msg-type-id
	       (chunk-msg-msg-stream-id chunk-msg) msg-stream-id)))
    (1 (let ((timestamp-delta (read-int 3 in))
	     (msg-len (read-int 3 in))
	     (msg-type-id (read-byte in)))
	 (setf (chunk-msg-timestamp-delta chunk-msg) timestamp-delta
	       (chunk-msg-total-msg-len chunk-msg) msg-len
	       (chunk-msg-msg-type-id chunk-msg) msg-type-id)))
    (2 (let ((timestamp-delta (read-int 3 in)))
	 (setf (chunk-msg-timestamp-delta chunk-msg) timestamp-delta)))
    (3 ))
  
  (incf (chunk-msg-timestamp chunk-msg) (chunk-msg-timestamp-delta chunk-msg))
  (let ((data (read-bytes (min (chunk-msg-per-msg-len chunk-msg) 
							   (- (chunk-msg-total-msg-len chunk-msg) (length (chunk-msg-payload chunk-msg))))
						  in)))
    (setf (chunk-msg-payload chunk-msg)
	  (to-flat-octets (chunk-msg-payload chunk-msg) data)))
  )

(defun srv-read-chunk (chunk-msg in)
  (multiple-value-bind (fmt chunk-stream-id)
		       (srv-read-chunk-basic-header in)
    (format t "~&; chunk:basic-header: fmt=~a, chunk-stream-id=~a~%" fmt chunk-stream-id)
    (setf (chunk-msg-chunk-stream-id chunk-msg) chunk-stream-id)

    (srv-read-chunk-msg-header fmt chunk-msg in)
    (format t "~&; chunk:msg-header: cs-id=~a, timestamp=~a, tid=~a, sid=~a, len=~a, total-len=~a, payload=~a~%"
	    (chunk-msg-chunk-stream-id chunk-msg)
	    (chunk-msg-timestamp chunk-msg)
	    (chunk-msg-msg-type-id chunk-msg)
	    (chunk-msg-msg-stream-id chunk-msg)
	    (chunk-msg-per-msg-len chunk-msg)
	    (chunk-msg-total-msg-len chunk-msg)
	    (length (chunk-msg-payload chunk-msg)))
    t))

(defun srv-read-message (chunk-msg in)
  ;; XXX: 一回で全て読み取れると仮定
  (srv-read-chunk chunk-msg in)
  (loop WHILE (/= (chunk-msg-total-msg-len chunk-msg)
				  (length (chunk-msg-payload chunk-msg)))
		DO
		(srv-read-chunk chunk-msg in))
  (let* ((chunk-payload (chunk-msg-payload chunk-msg)))
    (with-open-file (out "/tmp/chunk.tmp" :direction :output
			 :element-type 'octet
			 :if-exists :supersede)
      (write-sequence chunk-payload out)))
  t)

(define-symbol-macro lp
  (loop WITH server = srv
		WITH chunk-msg = (make-chunk-msg)
		DO
		(with-accept (io client server)
		  (srv-handshake io)
		  (srv-read-message chunk-msg io)
		  )))

#+C
(with-open-file (in "/tmp/chunk.tmp" :element-type 'octet)
  (list (amf0::decode in)
		(amf0::decode in)
		(amf0::decode in)
		(ignore-errors (amf0::decode in))
		))
