(in-package :rtmp.amf0)

(defun encode (value out)
  (encode-impl value out))

(defun encode-to-bytes (value)
  (flexi-streams:with-output-to-sequence (out :element-type 'octet)
    (encode value out)))

(defun decode (in)
  (decode-impl in))

(defun decode-bytes (bytes)
  (declare (octets bytes))
  (flexi-streams:with-input-from-sequence (in bytes)
    (decode in)))

