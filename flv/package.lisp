(defpackage rtmp.flv
  (:use :common-lisp :rtmp.utils)
  (:export decode))
(in-package :rtmp.flv)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))
