(progn
  (require :sb-bsd-sockets)
  (use-package :sb-bsd-sockets)
  )

(defun make-client-socket ()
  (let ((sock (make-instance 'inet-socket :type :stream :protocol :tcp)))
    sock))

(defun make-connected-socket (host port)
  (let ((sock (make-client-socket))
	(addr (host-ent-address (get-host-by-name host))))
    (socket-connect sock addr port)
    sock))

(defun write-int4 (int out)
  (loop FOR i FROM 3 DOWNTO 0
	DO (write-byte (ldb (byte 8 (* 8 i)) int) out))
  t)

(defun read-int4 (in)
  (loop FOR i FROM 3 DOWNTO 0 
	SUM (ash (read-byte in) (* i 8))))

(defun read-bytes (length in)
  (let ((bytes (make-array length :element-type 'octet)))
    (read-sequence bytes in)
    bytes))

(defun write-c0/c1 (io)
  (let ((write-version 3)
	(timestamp 0))
    (write-byte write-version io)
    (force-output io)

    (write-int4 timestamp io)
    (write-int4 0 io)
    (loop FOR i FROM 0 BELOW 1528 DO (write-byte (ldb (byte 8 0) i) io))
    (force-output io)

    timestamp))

(defun read-s0/s1 (io)
  (let ((version (read-byte io))
	(timestamp (read-int4 io))
	(zero (read-int4 io))
	(random-bytes (read-bytes 1528 io)))
    (format t "~&; version=~a, zero=~a, timestamp=~a~%" version zero timestamp)
    ;; fms は zero も使っているらしい (version? 値は0ではない)
    ;; (assert (zerop zero) () "'zero' must be 0: ~a" zero)
    (values timestamp random-bytes)))

(defun write-s2 (send-timestamp recv-timestamp recv-random-bytes out)
  (write-int4 recv-timestamp out)
  (write-int4 send-timestamp out)
  (write-sequence recv-random-bytes out)
  (force-output out)
  t)

(defun read-s2 (in)
  (values (read-int4 in)
	  (read-int4 in)
	  (read-bytes 1528 in)))

(defun rtmp-handshake (sock)
  (let ((io (socket-make-stream sock :input t :output t :element-type 'octet)))
    (let ((send-timestamp (write-c0/c1 io)))
      (multiple-value-bind (recv-timestamp random-bytes)
			   (read-s0/s1 io)
        (write-s2 send-timestamp recv-timestamp random-bytes io)
	(read-s2 io)
	t))))

(defstruct chunk
  basic-header
  chunk-msg-header
  extended-timestamp
  data)

(defun read-chunk-basic-header (sock)
  (let ((fmt (read-byte sock)))
    (print `(:fmt ,fmt))
    fmt))

(defun to-octets (list)
  (let ((octets (make-array (length list) :element-type 'octet)))
    (loop FOR i FROM 0
	  FOR x IN list
	  DO (setf (aref octets i) x))
    octets))

(defun build-chunk-basic-header (fmt stream-id)
  (declare ((integer 0 3) fmt)
	   ((integer 2 65599) stream-id))
  ;; format-type(fmt) + chunk-stream-id
  (to-octets
   (cond ((<= 2 stream-id 63) 
	  (list (+ (ash fmt 6) stream-id)))

	 ((<= 64 stream-id 319)
	  (list (+ (ash fmt 6) 0)
		#1=(- stream-id 64)))

	 ((<= 320 stream-id 65599)
	  (list (+ (ash fmt 6) 1)
		(ldb (byte 8 8) #1#)
		(ldb (byte 8 0) #1#))))
   ))

(defun to-bytes (size n)
  (loop FOR i FROM (1- size) DOWNTO 0
	COLLECT (ldb (byte 8 (* i 8)) n)))

(defun build-chunk-msg-header-fmt0 (timestamp msg-length msg-type-id msg-stream-id)
  (declare ((unsigned-byte 32) timestamp msg-stream-id)
	   ((unsigned-byte 24)  msg-length)
	   ((unsigned-byte 8) msg-type-id))
  (let ((timestamp (if (>= timestamp #xFFFFFF) #xFFFFFF timestamp)))
    (to-octets
     (append (to-bytes 3 timestamp)
	     (to-bytes 3 msg-length)
	     (list msg-type-id)
	     (reverse (to-bytes 4 msg-stream-id)) ; little endian
	     ))))

(defun build-chunk-msg-header-fmt1 (timestamp-delta msg-length msg-type-id) 
  (declare ((unsigned-byte 24) timestamp-delta msg-length)
	   ((unsigned-byte 8) msg-type-id))
  (to-octets
   (append (to-bytes 3 timestamp-delta)
	   (to-bytes 3 msg-length)
	   (list msg-type-id))))

(defun build-chunk-msg-header-fmt2 (timestamp-delta)
  (declare ((unsigned-byte 24) timestamp-delta))
  (to-octets
   (to-bytes 3 timestamp-delta)))

(defun build-chunk-msg-header-fmt3 ()
  (to-octets '()))

(defun build-chunk-extended-timestamp (timestamp)
  (declare ((unsigned-byte 32) timestamp))
  (if (< timestamp #xFFFFFF)
      (to-octets '())
    (to-octets (to-bytes 4 timestamp))))

(defun build-chunk (fmt chunk-stream-id data &key timestamp timestamp-delta 
						                          msg-type-id msg-stream-id
												  (msg-length (length data)))
						  
  (concatenate
   'simple-octets
   (build-chunk-basic-header fmt chunk-stream-id)
   (ecase fmt
     (0 (build-chunk-msg-header-fmt0 timestamp msg-length msg-type-id msg-stream-id))
     (1 (build-chunk-msg-header-fmt1 timestamp-delta msg-length msg-type-id))
     (2 (build-chunk-msg-header-fmt2 timestamp-delta))
     (3 (build-chunk-msg-header-fmt3)))
   (when (= 0 fmt) (build-chunk-extended-timestamp timestamp))
   (to-octets (coerce data 'list))))
  
(defun rtmp-test (sock)
  (rtmp-handshake sock)
  (format t "~&; handshacke done~%")

  (let ((io (socket-make-stream sock :input t :output t :element-type 'octet)))
    (read-chunk-basic-header io))
  t)

(defstruct msg-header
  msg-type        ; 1 byte
  payload-length  ; 3 bytes
  timestamp       ; 4 bytes
  stream-id)      ; 3 bytes

(defstruct message
  header
  payload)

(defun make-pcm-set-chunk-size (chunk-size &key (timestamp 0))
  (let ((msg-stream-id 0)  ; 0 is reserved for 'control stream'
	(chunk-stream-id 2); 2 is reserved for 'control stream'
	(pcm 1))
    (build-chunk 
     0 
     chunk-stream-id 
     (to-bytes 4 chunk-size)
     :msg-type-id pcm
     :timestamp timestamp
     :msg-stream-id msg-stream-id
     )))

(defun to-flat-octets (&rest values)
  (to-octets
   (loop FOR v IN values
	 APPEND
	 (etypecase v
	   (octet (list v))
	   (list  v)
	   (simple-octets (coerce v 'list))
	   ))))

(defun assoc-to-object (list)
  (amf0::make-object-type
   :value
   (loop FOR (key value) IN list
	 COLLECT (list key (etypecase value
	                     (string (amf0::make-string-type :value value))
			     (number (amf0::make-number-type :value (coerce value 'double-float)))
			     (boolean (amf0::make-boolean-type :value value)))))))

(defun rtmp-cmd-connect-bytes ()
  (let ((name "connect")
	(transaction-id 1)
	(params `(("app" "")
			  ("type" "nonprivate") ; ffmpeg
			  ("flashver" "FMLE/3.0 (compatible; Lavf53.24.2)")
			  ("tcUrl" "rtmp://192.168.137.95:8080/")))
	(opt-args '()))
    (to-flat-octets
     (amf0::encode (amf0::make-string-type :value name))
     (amf0::encode (amf0::make-number-type :value (coerce transaction-id 'double-float)))
     (amf0::encode (assoc-to-object params))
     (when opt-args
	   (amf0::encode (assoc-to-object opt-args)))
     )))

(defparameter *default-chunk-size* 128)

(defun build-chunks (chunk-stream-id msg-type-id msg-stream-id timestamp payload &aux (len (length payload)))
  (cons (build-chunk 0
					 chunk-stream-id
					 (subseq payload 0 (min len *default-chunk-size*))
					 :timestamp timestamp
					 :msg-type-id msg-type-id
					 :msg-stream-id msg-stream-id
					 :msg-length len)
		(loop FOR offset FROM *default-chunk-size* BELOW (length payload) BY *default-chunk-size*
			  WHILE (< offset len)
		  COLLECT
		  (build-chunk 3 chunk-stream-id (subseq payload offset (min len (+ offset *default-chunk-size*)))))
		))

(defun ary-to-int (length offset ary)
  (loop FOR i FROM 0 BELOW length
		FOR j FROM offset 
		SUM (ash (aref ary j) (* (- length i 1) 8))))

(defun parse-acknowledge-windows-size-msg (msg)
  (assert (= (chunk-msg-msg-type-id msg) 5) () "msg type must be 5: ~a" (chunk-msg-msg-type-id msg))
  ;; size
  (let ((size (ary-to-int 4 0 (chunk-msg-payload msg))))
	(format t "~&; [5] ack-win-size=~a~%" size)
	size))

(defun parse-see-peer-bandwidth-msg (msg)
  (assert (= (chunk-msg-msg-type-id msg) 6) () "msg type must be 6: ~a" (chunk-msg-msg-type-id msg))
  (let* ((ary (chunk-msg-payload msg))
		 (size (ary-to-int 4 0 ary))
		 (limit-type (aref ary 4)))
	(format t "~&; [6] ack-win-size=~a, limit-type=~a~%" size limit-type)
	(values size limit-type)))

(defun parse-stream-begin-msg (msg)
  (assert (= (chunk-msg-msg-type-id msg) 4) () "msg type must be 4: ~a" (chunk-msg-msg-type-id msg))  
  (let* ((ary (chunk-msg-payload msg))
		 (event-type (ary-to-int 2 0 ary))
		 (stream-id (ary-to-int 4 2 ary)))
	(assert (= event-type 0) () "event type must be 0: ~a" event-type)
	
	(format t "~&; [4] stream-begin: stream-id=~a~%" stream-id)
	stream-id))
  
(defun send-acknowledge-window-size (size out)
  (let ((msg-type-id 5)
		(msg-stream-id 0)  ; for control
		(chunk-stream-id 2) ; for control
		(timestamp 0)
		(payload (to-bytes 4 size)))
	
	(dolist (data (build-chunks chunk-stream-id msg-type-id msg-stream-id timestamp payload))
	  (write-sequence data out))
	(force-output out))
  t)

(defun rtmp-cmd-connect (sock)
  (let ((io (socket-make-stream sock :input t :output t :element-type 'octet))
	(msg-type-id 20)  ; for AMF0
	(msg-stream-id 33)
	(chunk-stream-id 44)
	(timestamp 100)
	(payload (rtmp-cmd-connect-bytes))

	(win-size 0)
	(begin-stream-id 0)
	)
	
	;; send
	(format t "~&; send 'connect'~%")
    (dolist (data (build-chunks chunk-stream-id msg-type-id msg-stream-id timestamp payload))
      (write-sequence data io))
	(force-output io)
  
	;; recv
	(format t "~&; recv 'connect' response~%")
	(let ((msg (make-chunk-msg)))
	  
	  ;; acknowledge window size
	  (srv-read-message msg io)
	  (setf win-size (parse-acknowledge-windows-size-msg msg))
	  (setf (chunk-msg-payload msg) '())

	  ;; see peer bandwidth
	  (srv-read-message msg io)
	  (parse-see-peer-bandwidth-msg msg)
	  (setf (chunk-msg-payload msg) '())	  

	  ;; send
	  (format t "~&; send ack-win-size: ~a~%" win-size)
	  (send-acknowledge-window-size win-size io)

	  ;; recv
	  ;; stream-begin
	  (srv-read-message msg io)
	  (setf begin-stream-id (parse-stream-begin-msg msg))
	  (setf (chunk-msg-payload msg) '())	  

	  ;; recv
	  ;; command-message
	  (srv-read-message msg io)	  
	  (prog1 
		  (parse-command-message msg)
		(setf (chunk-msg-payload msg) '()))
	  )
	)
  )

#|
command massage
 - message-type 20 for AMF0
 - message-type 17 for AMF3
|#
(define-symbol-macro cli 
  (progn (defparameter *cli* 
	   ;;(make-connected-socket "localhost" 1935)
	   ;;(make-connected-socket "172.16.250.131" 8080)
	   (make-connected-socket "192.168.136.112" 9090)
	   )
	 *cli*))
(define-symbol-macro cls
  (socket-close *cli*))
