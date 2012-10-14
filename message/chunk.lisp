(in-package :rtmp.message)

;;;; message
;; XXX: move
(defstruct message-base
  (type-id   0 :type (unsigned-byte 8))
  (stream-id 0 :type (unsigned-byte 32))
  (timestamp 0 :type (unsigned-byte 32)))

;;;
(defconstant +DEFAULT_CHUNK_SIZE+ 128)
(defconstant +MAX_CHUNK_SIZE+ 65536)

(defconstant +CHUNK_FMT_0+ 0)
(defconstant +CHUNK_FMT_1+ 1)
(defconstant +CHUNK_FMT_2+ 2)
(defconstant +CHUNK_FMT_3+ 3)

(defconstant +CHUNK_STREAM_ID_INDICATE_2BYTE+ 0)
(defconstant +CHUNK_STREAM_ID_INDICATE_3BYTE+ 1)

;;;; write
(defun write-chunk-basic-header (out fmt chunk-stream-id)
  (declare ((integer 0 3) fmt)
           ((integer 2 65599) chunk-stream-id))

  (let ((adjusted-fmt (ash fmt 6)))
    (cond ((<= 2 chunk-stream-id 63) 
           (write-uint 1 (+ adjusted-fmt chunk-stream-id) out))

          ((<= 64 chunk-stream-id 319)
           (write-uint 1 (+ adjusted-fmt +CHUNK_STREAM_ID_INDICATE_2BYTE+) out)
           (write-uint 1 (- chunk-stream-id 64) out))
          
          ((<= 320 chunk-stream-id 65599)
           (write-uint 1 (+ adjusted-fmt +CHUNK_STREAM_ID_INDICATE_3BYTE+) out)
           (write-uint 2 (- chunk-stream-id 64) out)))))
  
(defun write-fmt0-chunk (out chunk-stream-id message-type-id message-stream-id timestamp payload 
                             &key (start 0) (end (length payload)))
  (write-chunk-basic-header out +CHUNK_FMT_0+ chunk-stream-id)

  (let ((orig-timestamp timestamp)
        (timestamp (min timestamp #x00FFFFFF)))

    (write-uint 3 timestamp out)
    (write-uint 3 (length payload) out)
    (write-uint 1 message-type-id out)
    (write-uint 4 message-stream-id out :endian :little)
    
    (when (= timestamp #x00FFFFFF)
      (write-uint 4 orig-timestamp out)))

  (write-bytes payload out :start start :end end))

(defun write-fmt3-chunk (out chunk-stream-id payload &key (start 0) (end (length payload)))
  (write-chunk-basic-header out +CHUNK_FMT_3+ chunk-stream-id)
  (write-bytes payload out :start start :end end))

(defun write-chunks (out chunk-size chunk-stream-id message-type-id message-stream-id timestamp payload)
  (declare ((integer 1 65536) chunk-size)
           ((unsigned-byte 32) message-stream-id timestamp)
           ((integer 2 65599) chunk-stream-id)
           ((unsigned-byte  8) message-type-id)
           (octets payload))
  (let ((payload-len (length payload)))
    (write-fmt0-chunk out chunk-stream-id message-type-id message-stream-id timestamp payload 
                      :start 0 :end (min chunk-size payload-len))
    
    (loop FOR offset FROM chunk-size BELOW payload-len BY chunk-size
          FOR end = (min (+ offset chunk-size) payload-len)
          DO
          (write-fmt3-chunk out chunk-stream-id payload :start offset :end end)))
  (values))


;;;; read
(defstruct message-buffer
  (message-header t :type message-base)
  payloads)

(defstruct chunk-buffer
  stream-id

  prev-message-type-id
  prev-message-stream-id
  prev-timestamp
  (prev-timestamp-delta 0)

  payload-length
  (payload-read-length 0)
  
  message-buffers)
  
(defstruct state ; XXX: name ; TODO: chunk-stream-idごとに管理する必要がありそう
  chunk-size
  chunk-buffers
  (ack-win-size 1000) ; XXX:
  (next-ack-size 0) ; XXX:
  queue
  )
  
(defun make-initial-state (&key (chunk-size +DEFAULT_CHUNK_SIZE+))
  (make-state :chunk-size chunk-size))

(defun change-state-chunk-size (state new-chunk-size)
  (setf (state-chunk-size state) new-chunk-size)
  t)

(defun read-chunk-basic-header (in)
  (let* ((byte1 (read-uint 1 in))
         (fmt                  (ldb (byte 2 6) byte1))
         (base-chunk-stream-id (ldb (byte 6 0) byte1))
         (chunk-stream-id 
          (case base-chunk-stream-id
            (#. +CHUNK_STREAM_ID_INDICATE_2BYTE+ (+ (read-uint 1 in) 64))
            (#. +CHUNK_STREAM_ID_INDICATE_3BYTE+ (+ (read-uint 2 in) 64))
            (otherwise                           base-chunk-stream-id))))
    (values fmt chunk-stream-id)))

(defun read-chunk-message-header-fmt0 (io chunk)
  (let ((timestamp         (read-uint 3 io))
        (message-length    (read-uint 3 io))
        (message-type-id   (read-uint 1 io))
        (message-stream-id (read-uint 4 io :endian :little)))
    (show-log "msg-header-fmt0# timestamp=~d, length=~d, type-id=~d, stream-id=~d" 
              timestamp message-length message-type-id message-stream-id)

    (when (= timestamp #x00FFFFFF)
      (setf timestamp (read-uint 4 io)))
    
    (with-slots (prev-timestamp prev-message-stream-id prev-message-type-id 
                payload-read-length payload-length) chunk
      (setf prev-timestamp timestamp
            ;;prev-timestamp-delta 0
            prev-message-type-id message-type-id
            prev-message-stream-id message-stream-id
            payload-length message-length
            payload-read-length 0))
      
    (let* ((msg (make-message-base :type-id message-type-id
                                   :stream-id message-stream-id
                                   :timestamp timestamp))
           (buf (make-message-buffer :message-header msg
                                     :payloads '())))
      (push buf (chunk-buffer-message-buffers chunk))
      buf)))

(defun read-chunk-message-header-fmt1 (io chunk)
  (let ((timestamp-delta (read-uint 3 io))
        (message-length  (read-uint 3 io))
        (message-type-id (read-uint 1 io)))
    (show-log "msg-header-fmt1# timestamp-delta=~d, length=~d, type-id=~d" 
              timestamp-delta message-length message-type-id)

    (when (= timestamp-delta #x00FFFFFF)
      (setf timestamp-delta (read-uint 4 io)))
    
    (with-slots (prev-timestamp prev-timestamp-delta prev-message-stream-id prev-message-type-id 
                 payload-read-length payload-length) chunk
      (setf prev-timestamp-delta timestamp-delta
            prev-message-type-id message-type-id
            payload-length message-length
            payload-read-length 0)
      (incf prev-timestamp prev-timestamp-delta)

      ;; TODO: type-idも考慮すべき？
      (let ((msg-buf (find prev-message-stream-id (chunk-buffer-message-buffers chunk) 
                           :key (lambda (x) (message-base-stream-id (message-buffer-message-header x))))))
        (unless msg-buf
          ;; XXX: (not null ...)の場合の方が問題?
          (setf payload-read-length 0)
          (setf msg-buf (make-message-buffer 
                         :message-header (make-message-base :stream-id prev-message-stream-id
                                                            :timestamp prev-timestamp
                                                            :type-id prev-message-type-id)
                         :payloads '()))
          (push msg-buf (chunk-buffer-message-buffers chunk)))
        msg-buf))))

(defun read-chunk-message-header-fmt2 (io chunk)
  (let ((timestamp-delta (read-uint 3 io)))
    (show-log "msg-header-fmt2# timestamp-delta=~d" timestamp-delta)
    (when (= timestamp-delta #x00FFFFFF)
      (setf timestamp-delta (read-uint 4 io)))

    (with-slots (prev-timestamp prev-message-type-id 
                 prev-timestamp-delta prev-message-stream-id payload-read-length) chunk
      (setf prev-timestamp-delta timestamp-delta
            payload-read-length 0)
      (incf prev-timestamp prev-timestamp-delta)
            
      (let ((msg-buf (find prev-message-stream-id (chunk-buffer-message-buffers chunk) 
                           :key (lambda (x) (message-base-stream-id (message-buffer-message-header x))))))

        (unless msg-buf
          ;; XXX: (not null ...)の場合の方が問題?
          (setf payload-read-length 0)
          (setf msg-buf (make-message-buffer 
                         :message-header (make-message-base :stream-id prev-message-stream-id
                                                            :timestamp prev-timestamp
                                                            :type-id prev-message-type-id)
                         :payloads '()))
          (push msg-buf (chunk-buffer-message-buffers chunk)))

        msg-buf))))

(defun read-chunk-message-header-fmt3 (io chunk)
  (declare (ignore io))
  (show-log "msg-header-fmt3#")

  (with-slots (prev-message-stream-id prev-timestamp prev-timestamp-delta
               prev-message-type-id payload-read-length) chunk
    (let ((msg-buf (find prev-message-stream-id (chunk-buffer-message-buffers chunk) 
                         :key (lambda (x) (message-base-stream-id (message-buffer-message-header x))))))

      (unless msg-buf
        (incf prev-timestamp prev-timestamp-delta) ;; XXX: delta周りは怪しい (fmt1, fmt2も)

        (setf payload-read-length 0)
        (setf msg-buf (make-message-buffer 
                       :message-header (make-message-base :stream-id prev-message-stream-id
                                                          :timestamp prev-timestamp
                                                          :type-id prev-message-type-id)
                       :payloads '()))
        (push msg-buf (chunk-buffer-message-buffers chunk)))
      msg-buf)))

(defun read-chunk (io state)
  (with-log-section ("read-chunk")
    (multiple-value-bind (fmt chunk-stream-id)
                         (read-chunk-basic-header io)
      (show-log "basic-header# fmt=~d, chunk-stream-id=~d" fmt chunk-stream-id)
      
      (let ((chunk (find chunk-stream-id (state-chunk-buffers state) :key #'chunk-buffer-stream-id)))
        (unless chunk
          (setf chunk (make-chunk-buffer :stream-id chunk-stream-id))
          (push chunk (state-chunk-buffers state)))
        
        (let ((msg-buf (ecase fmt
                         (0 (read-chunk-message-header-fmt0 io chunk))
                         (1 (read-chunk-message-header-fmt1 io chunk))
                         (2 (read-chunk-message-header-fmt2 io chunk))
                         (3 (read-chunk-message-header-fmt3 io chunk)))))

          (with-slots (payload-read-length payload-length) chunk
            (let ((chunk-payload-length (min (state-chunk-size state) 
                                             (- payload-length payload-read-length))))
              (push (read-bytes chunk-payload-length io) (message-buffer-payloads msg-buf))
              (incf payload-read-length chunk-payload-length))
        
            (show-log "current message# payload-length=~d" payload-read-length)
            (assert (<= payload-read-length payload-length) () "TODO: 1")

            (= payload-length payload-read-length)))))))
    
(defun read-message-chunks (io state)
  (loop FOR end? = (read-chunk io state)
        UNTIL end?)

  (loop WITH acc = '()
        FOR chunk-buffer IN (state-chunk-buffers state)
        WHEN (= (chunk-buffer-payload-length chunk-buffer)
                (chunk-buffer-payload-read-length chunk-buffer))
        DO
        (loop FOR message-buffer IN (chunk-buffer-message-buffers chunk-buffer)
              DO
              (with-slots (message-header payloads) message-buffer
                (push (list message-header (apply #'concatenate 'octets (reverse payloads)))
                      acc)))
        (setf (chunk-buffer-message-buffers chunk-buffer) '())

        FINALLY
        (with-slots (queue) state
          (setf queue (nconc queue (nreverse acc)))
          (return (pop queue)))))
