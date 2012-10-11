(in-package :rtmp.amf0)

;; XXX: name: decoder

(defun decode-number (in)
  (decode-double-float (read-uint 8 in)))

(defun encode-number (value out)
  (declare (number value))
  (let ((code (encode-double-float (coerce value 'double-float))))
	(write-uint 8 code out)))

(defun decode-boolean (in)
  (if (/= (read-uint 1 in) 0) :true :false))

(defun encode-boolean (value out)
  (declare ((member :true :false) value))
  (write-uint 1 (if (eq value :true) 1 0) out))

(defun parse-impl (in)
  (let ((marker (read-uint 1 in)))
	marker))

#|
(defun encode-impl (value out)
  )

(defun decode-impl (in)
  )
|#