(in-package :rtmp.amf0)

(defun encode (value out)
  (encode-impl value out))

(defun decode (in)
  (decode-impl in))

