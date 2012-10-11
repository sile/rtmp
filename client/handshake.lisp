(in-package :rtmp.client)

(defun generate-random-bytes (size)
  (let ((bytes (make-array size :element-type 'octet)))
    (dotimes (i size bytes)
      (setf (aref bytes i) (random #x100)))))

(defun send-c0/c1 (out version timestamp zero random-bytes)
  (write-handshake-0 out version)
  (write-handshake-1 out timestamp zero random-bytes)
  (force-output out)
  (show-log "send c0# version=~a" version)
  (show-log "send c1# timestamp=~a, zero=~a" timestamp zero))

(defun send-c2 (out s1-timestamp-echo c1-timestamp s1-random-bytes-echo)
  (write-handshake-2 out s1-timestamp-echo c1-timestamp s1-random-bytes-echo)
  (force-output out)
  (show-log "send c2# timestamp1=~a, timestamp2=~a" s1-timestamp-echo c1-timestamp))
          
(defun recv-s0/s1 (in sent-version)
  (let ((recv-version (read-handshake-0 in)))
    (show-log "recv s0# version=~a" recv-version)
    (assert (= sent-version recv-version) ()
            "versions are mismatched: client=~a, server=~a" sent-version recv-version))

  (multiple-value-bind (recv-timestamp recv-zero recv-random-bytes)
                       (read-handshake-1 in)
    (show-log "recv s1# timestamp=~a, zero=~a" recv-timestamp recv-zero)
    (values recv-timestamp recv-zero recv-random-bytes)))

(defun recv-s2 (in c1-timestamp c1-random-bytes)
  (multiple-value-bind (c1-timestamp-echo s2-timestamp c1-random-bytes-echo)
                       (read-handshake-2 in)
    (show-log "recv s2# timestamp1=~a, timestamp2=~a" c1-timestamp-echo s2-timestamp)

    (assert (= c1-timestamp c1-timestamp-echo) ()
            "incorrect S2's timestamp: c1=~a, s2=~a" c1-timestamp c1-timestamp-echo)
    (assert (not (mismatch c1-random-bytes c1-random-bytes-echo)) ()
            "incorrect S2's random-bytes")
    
    (values)))

(defun handshake (io &key (version +RTMP_VERSION+)
                          (timestamp (get-internal-real-time))
                          (zero 0)
                          (random-bytes (generate-random-bytes 1528)))
  (with-log-section ("handshake")
    (send-c0/c1 io version timestamp zero random-bytes)
    
    (multiple-value-bind (recv-timestamp recv-zero recv-random-bytes)
                         (recv-s0/s1 io version)

      (send-c2 io recv-timestamp timestamp recv-random-bytes)
      (recv-s2 io timestamp random-bytes)
      
      (values recv-timestamp recv-zero recv-random-bytes))))
