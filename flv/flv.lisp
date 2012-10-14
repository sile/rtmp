(in-package :rtmp.flv)

(defun decode (in)
  (decode-impl in))

(defun decode-bytes (bytes)
  (with-input-from-bytes (in bytes)
    (decode-impl in)))
