(defpackage rtmp.amf3
  (:use :common-lisp :rtmp.utils)
  (:export encode
           encode-to-bytes

           decode
           decode-bytes

           object-type))
(in-package :rtmp.amf3)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))

(defun object-type-p (x)
  (and (consp x) 
       (eq (first x) :map) (listp (second x))
       (null (cddr x))))
       
(deftype object-type () '(satisfies object-type-p))
