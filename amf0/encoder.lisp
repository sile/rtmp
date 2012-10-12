(in-package :rtmp.amf0)

(defun encode-number (value out)
  (declare (number value))
  (let ((code (encode-double-float (coerce value 'double-float))))
    (write-uint 1 +NUMBER_MARKER+ out)
    (write-uint 8 code out)))

(defun encode-boolean (value out)
  (declare ((member :true :false) value))
  (write-uint 1 +BOOLEAN_MARKER+ out)
  (write-uint 1 (if (eq value :true) 1 0) out))

(defun encode-string-no-marker (value out)
  (let* ((bytes (creole:string-to-octets value))
	 (len (length bytes)))
    (write-uint 2 len out)
    (write-bytes bytes out)))

(defun encode-string (value out)
  (declare (string value))
  (let* ((bytes (creole:string-to-octets value))
	 (len (length bytes)))
    (if (< len #x10000)
	(progn (write-uint 1 +STRING_MARKER+ out)
	       (write-uint 2 len out))
      (progn (write-uint 1 +LONG_STRING_MARKER+ out)
	     (write-uint 4 len out)))
    (write-bytes bytes out)))

(defun encode-xml-document (value out)
  (declare (string value))
  (let* ((bytes (creole:string-to-octets value))
	 (len (length bytes)))
    (write-uint 1 +XML_DOCUMENT_MARKER+ out)
    (write-uint 4 len out)
    (write-bytes bytes out)))

(defun encode-object (list out)
  (write-uint 1 +OBJECT_MARKER+ out)
  (loop FOR (key value) IN list
    DO 
    (encode-string-no-marker key out)
    (encode-impl value out))
  (encode-string-no-marker "" out)
  (write-uint 1 +OBJECT_END_MARKER+ out))

(defun encode-typed-object (data out)
  (destructuring-bind (class list) data
    (write-uint 1 +TYPED_OBJECT_MARKER+ out)
    (encode-string-no-marker class out)
    (loop FOR (key value) IN list
      DO 
      (encode-string-no-marker key out)
      (encode-impl value out))
    (encode-string-no-marker "" out)
    (write-uint 1 +OBJECT_END_MARKER+ out)))

(defun encode-null (out) 
  (write-uint 1 +NULL_MARKER+ out))
(defun encode-undefined (out) 
  (write-uint 1 +UNDEFINED_MARKER+ out))
(defun encode-unsupported (out) 
  (write-uint 1 +UNSUPPORTED_MARKER+ out))

(defun encode-referance (value out)
  (declare ((unsigned-byte 16) value))
  (write-uint 1 +REFERENCE_MARKER+ out)
  (write-uint 2 value out))

(defun encode-ecma-array (list out)
  (write-uint 1 +ECMA_ARRAY_MARKER+ out)
  (write-uint 4 (length list) out)
  (loop FOR (key value) IN list
    DO
    (encode-string-no-marker key out)
    (encode-impl value out)))

(defun encode-strict-array (list out)
  (write-uint 1 +STRICT_ARRAY_MARKER+ out)
  (write-uint 4 (length list) out)
  (loop FOR value IN list
	DO (encode-impl value out)))

(defun encode-date (value out)
  (destructuring-bind (timestamp timezone) value
    (write-uint 1 +DATE_MARKER+ out)
    (write-uint 8 (encode-double-float (coerce timestamp 'double-float)) out)
    (write-uint 2 timezone out)))
    
(defun encode-impl (value out)
  (etypecase value
    (number (encode-number value out))
    (string (encode-string value out))
    (keyword (ecase value
               ((:true :false) (encode-boolean value out))
	       ((:null)        (encode-null out))
	       ((:undefined)   (encode-undefined out))
	       ((:unsupported) (encode-unsupported out))))
    (list (if (not (keywordp (car value)))
	      (encode-strict-array value out)
	    (destructuring-bind (tag tagged-value) value
	      (ecase tag
	        (:xml   (encode-xml-document tagged-value out))
		(:map   (encode-object tagged-value out))
		(:class (encode-typed-object tagged-value out))
		(:date  (encode-date tagged-value out))
		(:reference (encode-referance tagged-value out))
		))))))
