(in-package :rtmp.message)

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
  stream-id
  type-id
  timestamp
  payloads)

(defstruct chunk-buffer
  stream-id

  prev-message-type-id
  prev-message-type-id
  prev-message-stream-id
  prev-timestamp
  (prev-timestamp-delta 0)

  payload-length
  (payload-read-length 0)
  
  message-buffers)
  
(defstruct state ; XXX: name ; TODO: chunk-stream-idごとに管理する必要がありそう
;; delete: begin
  chunk-stream-id
  message-type-id
  message-stream-id
  timestamp
  (timestamp-delta 0)
;; delete: end

  chunk-size

;; delete: begin
  (payload-length 0)
  payload-chunks
;; delete: end
  )
  
(defun state-payload-read-length (state)
  (loop FOR chunk IN (state-payload-chunks state)
        SUM (length chunk)))

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

(defun read-chunk-message-header-fmt0 (io state)
  (let ((timestamp         (read-uint 3 io))
        (message-length    (read-uint 3 io))
        (message-type-id   (read-uint 1 io))
        (message-stream-id (read-uint 4 io :endian :little)))
    (show-log "msg-header-fmt0# timestamp=~d, length=~d, type-id=~d, stream-id=~d" 
              timestamp message-length message-type-id message-stream-id)
    (setf (state-timestamp state) timestamp
          (state-timestamp-delta state) 0
          (state-payload-length state) message-length
          (state-message-type-id state) message-type-id
          (state-message-stream-id state) message-stream-id)
    
    (when (= timestamp #x00FFFFFF)
      (setf (state-timestamp state) (read-uint 4 io))))

  (values))

(defun read-chunk-message-header-fmt1 (io state)
  (let ((timestamp-delta (read-uint 3 io))
        (message-length  (read-uint 3 io))
        (message-type-id (read-uint 1 io)))
    (show-log "msg-header-fmt1# timestamp-delta=~d, length=~d, type-id=~d" 
              timestamp-delta message-length message-type-id)
    (setf (state-timestamp-delta state) timestamp-delta
          (state-payload-length state) message-length
          (state-message-type-id state) message-type-id)

    (when (= timestamp-delta #x00FFFFFF)
      (setf (state-timestamp-delta state) (read-uint 4 io))))
  (values))

(defun read-chunk-message-header-fmt2 (io state)
  (let ((timestamp-delta (read-uint 3 io)))
    (show-log "msg-header-fmt2# timestamp-delta=~d" timestamp-delta)
    (setf (state-timestamp-delta state) timestamp-delta)

    (when (= timestamp-delta #x00FFFFFF)
      (setf (state-timestamp-delta state) (read-uint 4 io))))
  (values))

(defun read-chunk-message-header-fmt3 (io state)
  (declare (ignore io state))
  (show-log "msg-header-fmt3#")
  (values))

(defun read-chunk (io state)
  (with-log-section ("read-chunk")
    (multiple-value-bind (fmt chunk-stream-id)
                         (read-chunk-basic-header io)
      (show-log "basic-header# fmt=~d, chunk-stream-id=~d" fmt chunk-stream-id)
      (setf (state-chunk-stream-id state) chunk-stream-id)

      (ecase fmt
        (0 (read-chunk-message-header-fmt0 io state))
        (1 (read-chunk-message-header-fmt1 io state))
        (2 (read-chunk-message-header-fmt2 io state))
        (3 (read-chunk-message-header-fmt3 io state)))

      (incf (state-timestamp state) (state-timestamp-delta state))

      (let* ((message-payload-length (state-payload-length state))
             (read-payload-length (state-payload-read-length state))
             (chunk-payload-length (min (state-chunk-size state)
                                        (- message-payload-length read-payload-length))))
        (push (read-bytes chunk-payload-length io) (state-payload-chunks state))
        
        (show-log "current message# timestamp=~d, payload-length=~d"
                  (state-timestamp state) (state-payload-read-length state))
        (assert (<= (state-payload-read-length state) message-payload-length) ; XXX: 毎回計算は無駄
                () "TODO: ")
        (= (state-payload-read-length state) message-payload-length)))))
    
(defun read-message-chunks (io state)
  (loop FOR end? = (read-chunk io state)
        UNTIL end?)
  (with-slots (message-type-id timestamp message-stream-id payload-chunks) state
    (let ((payload (apply #'concatenate 'octets (reverse payload-chunks))))
      (setf payload-chunks '())
      (values message-type-id timestamp message-stream-id payload))))
