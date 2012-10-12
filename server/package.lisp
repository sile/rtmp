(defpackage rtmp.server
  (:use :common-lisp :rtmp.utils :rtmp.const)
  (:export start
           handshake))
(in-package :rtmp.server)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))

