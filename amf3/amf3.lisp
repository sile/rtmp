(in-package :rtmp.amf3)


(defun decode (in)
  (decode-impl in))

(defun decode-bytes (bytes)
  (declare (octets bytes))
  (flexi-streams:with-input-from-sequence (in bytes)
    (decode in)))
