(defpackage rtmp.utils
  (:use :common-lisp)
  (:export decode-double-float
		   encode-double-float
		   
		   write-uint
		   write-bytes
		   read-uint
		   read-bytes))
(in-package :rtmp.utils)
