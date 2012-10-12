(defpackage rtmp.client
  (:use :common-lisp :rtmp.utils :rtmp.const)
  (:export handshake
           connect
           create-stream
           publish
           play
           ))
(in-package :rtmp.client)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))


  