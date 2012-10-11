(defpackage rtmp.amf0
  (:use :common-lisp :rtmp.utils)
  (:export encode
           encode-to-bytes

	   decode
           decode-bytes))
(in-package :rtmp.amf0)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))
