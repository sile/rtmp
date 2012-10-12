(in-package :rtmp.message)

(defconstant +DEFAULT_CHUNK_SIZE+ 128)
(defconstant +MAX_CHUNK_SIZE+ 65536)

(defconstant +CHUNK_FMT_0+ 0)
(defconstant +CHUNK_FMT_1+ 1)
(defconstant +CHUNK_FMT_2+ 2)
(defconstant +CHUNK_FMT_3+ 3)

(defconstant +CHUNK_STREAM_ID_INDICATE_2BYTE+ 0)
(defconstant +CHUNK_STREAM_ID_INDICATE_3BYTE+ 1)

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
