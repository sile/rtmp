(defpackage rtmp.amf0
  (:use :common-lisp :rtmp.utils)
  (:export encode
	   decode))
(in-package :rtmp.amf0)

(deftype octet () '(unsigned-byte 8))
