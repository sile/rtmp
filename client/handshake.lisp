(in-package :rtmp.client)

(defun generate-random-bytes (size)
  (let ((bytes (make-array size :element-type 'octet)))
    (dotimes (i size bytes)
      (setf (aref bytes i) (random #x100)))))

(defun handshake (io &key (version +RTMP_VERSION+)
                          (timestamp 0)
                          (zero 0)
                          (random-bytes (generate-random-bytes 1528)))
  (with-log-section ("handshake")
    ;; send: c0/c1
    (write-handshake-0 io version)
    (write-handshake-1 io timestamp zero random-bytes)
    (force-output io)
    (show-log "send c0# version=~a" version)
    (show-log "send c1# timestamp=~a, zero=~a" timestamp zero)
    
    ;; recv: s0/s1
    (let ((recv-version (read-handshake-0 io)))
      (show-log "recv s0# versoin=~a" recv-version)
      (assert (= version recv-version) ()
              "versions are mismatched: client=~a, server=~a" version recv-version)
    
      (multiple-value-bind (recv-timestamp recv-zero recv-random-bytes)
                           (read-handshake-1 io)
        (show-log "recv s1# timestamp=~a, zero=~a" recv-timestamp recv-zero)

        ;; send: c2
        (write-handshake-2 io recv-timestamp timestamp recv-random-bytes)
        (show-log "send c2# timestamp1=~a, timestamp2=~a" recv-timestamp timestamp)

        ;; recv: s2
        (multiple-value-bind (send-timestamp recv-timestamp2 send-random-bytes)
                             (read-handshake-2 io)
          (show-log "recv s2# timestamp1=~a, timestamp2=~a" send-timestamp recv-timestamp2)

          (assert (= timestamp send-timestamp) ()
                  "incorrect S2's timestamp: c1=~a, s2=~a" timestamp send-timestamp)
          (assert (not (mismatch random-bytes send-random-bytes)) ()
                  "incorrect S2's random-bytes"))

        ;; all ok
        (values recv-timestamp recv-zero recv-random-bytes)))))
