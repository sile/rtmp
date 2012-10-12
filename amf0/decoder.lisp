(in-package :rtmp.amf0)

(defun decode-number (in)
  (decode-double-float (read-uint 8 in)))

(defun decode-boolean (in)
  (if (/= (read-uint 1 in) 0) :true :false))

(defun decode-string (in)
  (let ((length (read-uint 2 in)))
    (creole:octets-to-string (read-bytes length in))))

(defun decode-long-string (in)
  (let ((length (read-uint 4 in)))
    (creole:octets-to-string (read-bytes length in))))

(defun decode-xml-document (in)
  (decode-long-string in))

(defun decode-object (in)
  (loop FOR key   = (decode-string in)
	FOR value = (decode-impl in)
	UNTIL (eq value :object-end)
    COLLECT (list key value) INTO list
    FINALLY (return `(:map ,list))))

(defun decode-typed-object (in)
  (loop WITH class = (decode-string in)
	FOR key    = (decode-string in)
	FOR value  = (decode-impl in)
	UNTIL (eq value :object-end)
    COLLECT (list key value) INTO list
    FINALLY (return `(:class (,class ,list)))))

(defun decode-null (in) (declare (ignore in)) :null)
(defun decode-undefined (in) (declare (ignore in)) :undefined)
(defun decode-unsupported (in) (declare (ignore in)) :unsupported)
(defun decode-object-end (in) (declare (ignore in)) :object-end)

(defun decode-reference (in)
  `(:reference ,(read-uint 2 in))) ; TODO: 

(defun decode-ecma-array (in)
  (let ((size (read-uint 4 in)))
    (loop REPEAT size
	  FOR key   = (decode-string in)
	  FOR value = (decode-impl in)
      COLLECT (list key value) INTO list
      FINALLY (return `(:map ,list)))))

(defun decode-strict-array (in)
  (let ((size (read-uint 4 in)))
    (loop REPEAT size 
	  COLLECT (decode-impl in))))

(defun decode-date (in)
  (let ((timestamp (decode-double-float (read-uint 8 in)))
	(timezone (read-uint 2 in)))
    (multiple-value-bind (timestamp xxx) (floor timestamp)
      (assert (zerop xxx) () "TODO:")
      `(:date (,timestamp ,timezone)))))
    
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
      (#. +AVMPLUS_OBJECT_MARKER+ (error "not implemented: AVMPLUS_OBJECT_MARKER"))
      )))
