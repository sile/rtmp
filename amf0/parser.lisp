(in-package :rtmp.amf0)

;; XXX: name: decoder

(defun decode-number (in)
  (decode-double-float (read-uint 8 in)))

(defun encode-number (value out)
  (declare (number value))
  (let ((code (encode-double-float (coerce value 'double-float))))
    (write-uint 1 +NUMBER_MARKER+ out)
    (write-uint 8 code out)))

(defun decode-boolean (in)
  (if (/= (read-uint 1 in) 0) :true :false))

(defun encode-boolean (value out)
  (declare ((member :true :false) value))
  (write-uint 1 +BOOLEAN_MARKER+ out)
  (write-uint 1 (if (eq value :true) 1 0) out))

(defun decode-string (in)
  (let ((length (read-uint 2 in)))
    (creole:octets-to-string (read-bytes length in))))

(defun decode-long-string (in)
  (let ((length (read-uint 4 in)))
    (creole:octets-to-string (read-bytes length in))))

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

(defun decode-xml-document (in)
  (decode-long-string in))

(defun encode-xml-document (value out)
  (declare (string value))
  (let* ((bytes (creole:string-to-octets value))
	 (len (length bytes)))
    (write-uint 1 +XML_DOCUMENT_MARKER+ out)
    (write-uint 4 len out)
    (write-bytes bytes out)))

(defun decode-object (in)
  (loop FOR key   = (decode-string in)
	FOR value = (decode-impl in)
	UNTIL (eq value :object-end)
    COLLECT (cons key value) INTO list
    FINALLY (return `(:map ,list))))

(defun encode-object (list out)
  (write-uint 1 +OBJECT_MARKER+ out)
  (loop FOR (key . value) IN list
    DO 
    (encode-string-no-marker key out)
    (encode-impl value out))
  (encode-string-no-marker "" out)
  (write-uint 1 +OBJECT_END_MARKER+ out))

(defun decode-typed-object (in)
  (loop WITH class = (decode-string in)
	FOR key    = (decode-string in)
	FOR value  = (decode-impl in)
	UNTIL (eq value :object-end)
    COLLECT (cons key value) INTO list
    FINALLY (return `(:class (,class ,list)))))

(defun encode-typed-object (data out)
  (destructuring-bind (class list) data
    (write-uint 1 +TYPED_OBJECT_MARKER+ out)
    (encode-string-no-marker class out)
    (loop FOR (key . value) IN list
      DO 
      (encode-string-no-marker key out)
      (encode-impl value out))
    (encode-string-no-marker "" out)
    (write-uint 1 +OBJECT_END_MARKER+ out)))

(defun decode-null (in) (declare (ignore in)) :null)
(defun decode-undefined (in) (declare (ignore in)) :undefined)
(defun decode-unsupported (in) (declare (ignore in)) :unsupported)
(defun decode-object-end (in) (declare (ignore in)) :object-end)

(defun encode-null (out) 
  (write-uint 1 +NULL_MARKER+ out))
(defun encode-undefined (out) 
  (write-uint 1 +UNDEFINED_MARKER+ out))
(defun encode-unsupported (out) 
  (write-uint 1 +UNSUPPORTED_MARKER+ out))

(defun decode-reference (in)
  `(:reference ,(read-uint 2 in))) ; TODO: 

(defun encode-referance (value out)
  (declare ((unsigned-byte 16) value))
  (write-uint 1 +REFERENCE_MARKER+ out)
  (write-uint 2 value out))

(defun decode-ecma-array (in)
  (let ((size (read-uint 4 in)))
    (loop REPEAT size
	  FOR key   = (decode-string in)
	  FOR value = (decode-impl in)
      COLLECT (cons key value) INTO list
      FINALLY (return `(:map ,list)))))

(defun encode-ecma-array (list out)
  (write-uint 1 +ECMA_ARRAY_MARKER+ out)
  (write-uint 4 (length list) out)
  (loop FOR (key . value) IN list
    DO
    (encode-string-no-marker key out)
    (encode-impl value out)))

(defun decode-strict-array (in)
  (let ((size (read-uint 4 in)))
    (loop REPEAT size 
	  COLLECT (decode-impl in))))

(defun encode-strict-array (list out)
  (write-uint 1 +STRICT_ARRAY_MARKER+ out)
  (write-uint 4 (length list) out)
  (loop FOR value IN list
	DO (encode-impl value out)))

(defun decode-date (in)
  (let ((timestamp (decode-double-float (read-uint 8 in)))
	(timezone (read-uint 2 in)))
    (multiple-value-bind (timestamp xxx) (floor timestamp)
      (assert (zerop xxx) () "TODO:")
      `(:date (,timestamp ,timezone)))))

(defun encode-date (value out)
  (destructuring-bind (timestamp timezone) value
    (write-uint 1 +DATE_MARKER+ out)
    (write-uint 8 (encode-double-float (coerce timestamp 'double-float)) out)
    (write-uint 2 timezone out)))
    
(defun decode-impl (in)
  (let ((marker-code (read-uint 1 in)))
    (ecase marker-code
      (#. +NUMBER_MARKER+       (decode-number in))
      (#. +BOOLEAN_MARKER+      (decode-boolean in))
      (#. +STRING_MARKER+       (decode-string in))
      (#. +LONG_STRING_MARKER+  (decode-long-string in))
      (#. +XML_DOCUMENT_MARKER+ (decode-xml-document in))
      (#. +OBJECT_MARKER+       (decode-object in))
      (#. +TYPED_OBJECT_MARKER+ (decode-typed-object in))
      (#. +OBJECT_END_MARKER+   (decode-object-end in))
      (#. +NULL_MARKER+         (decode-null in))
      (#. +UNDEFINED_MARKER+    (decode-undefined in))
      (#. +UNSUPPORTED_MARKER+  (decode-unsupported in))
      (#. +REFERENCE_MARKER+    (decode-reference in))
      (#. +ECMA_ARRAY_MARKER+   (decode-ecma-array in))
      (#. +STRICT_ARRAY_MARKER+ (decode-strict-array in))
      (#. +DATE_MARKER+         (decode-date in))
      )))

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
	      
