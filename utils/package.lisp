(defpackage rtmp.utils
  (:use :common-lisp :rtmp.const)
  (:export decode-double-float
           encode-double-float
           
           write-uint
           write-bytes
           read-uint read-uint-from-bytes
           read-int
           read-bytes

           with-output-to-bytes
           with-input-from-bytes

           write-handshake-0
           write-handshake-1
           write-handshake-2
           read-handshake-0
           read-handshake-1
           read-handshake-2

           *show-log*
           show-log
           with-log-section
           ))
(in-package :rtmp.utils)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))
