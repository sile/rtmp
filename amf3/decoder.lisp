(in-package :rtmp.amf3)

(defun decode-undefined (in)
  (declare (ignore in))
  :undefined)

(defun decode-null (in)
  (declare (ignore in))
  :null)

(defun decode-false (in)
  (declare (ignore in))
  :false)

(defun decode-true (in)
  (declare (ignore in))
  :true)

(defun decode-integer (in)
  (loop WITH sum = 0
        FOR b = (read-uint 1 in)
        FOR i FROM 0 BELOW 4
    DO
    (cond ((= i 3)    (return (+ (ash sum 8) 8)))
          ((< b #x80) (return (+ (ash sum 7) b)))
          (t          (setf sum (+ (ash sum 7) (ldb (byte 7 0) b)))))))

(defun decode-double (in)
  (decode-double-float (read-uint 8 in)))

(defun decode-string (in)
  (let ((byte (read-uint 1 in)))
    (assert (= 1 (ldb (byte 1 0) byte)) () "string reference is unsupported")
    
    (let ((length (ldb (byte 28 1) byte)))
      (creole:octets-to-string (read-bytes length in)))))
    
(defun decode-xml-doc (in)
  (declare (ignore in))
  (error "not implemented: XML_DOC_MARKER"))

(defun decode-date (in)
  (declare (ignore in))
  (error "not implemented: DATE_MARKER"))

(defun decode-array (in)
  (declare (ignore in))
  (error "not implemented: ARRAY_MARKER"))

(defun decode-object (in)
  (let ((flags (read-uint 1 in)))
    (assert (ldb-test (byte 1 0) flags) () "unsupported(1)")
    (assert (ldb-test (byte 1 1) flags) () "unsupported(2)")
    (assert (not (ldb-test (byte 1 2) flags)) () "unsupported(3)")
    (assert (ldb-test (byte 1 3) flags) () "unsupported(4)") ; dynamic
    (assert (= 0 (ldb (byte 25 4) flags)) () "unsupported(5)")
    
    (let ((class-name (decode-string in)))
      (assert (string= class-name "") () "unsupported: named object(~s)" class-name)
      
      (loop FOR key = (decode-string in)
            UNTIL (string= key "")
            FOR value = (decode-impl in)
        COLLECT (list key value) INTO list
        FINALLY (return `(:map ,list))))))

(defun decode-xml (in)
  (declare (ignore in))
  (error "not implemented: XML_MARKER"))

(defun decode-byte-array (in)
  (declare (ignore in))
  (error "not implemented: BYTE_ARRAY_MARKER"))

(defun decode-impl (in)
  (let ((marker-code (read-uint 1 in)))
    (ecase marker-code
      (#. +UNDEFINED_MARKER+ (decode-undefined in))
      (#. +NULL_MARKER+ (decode-null in))
      (#. +FALSE_MARKER+ (decode-false in))
      (#. +TRUE_MARKER+ (decode-true in))
      (#. +INTEGER_MARKER+ (decode-integer in))
      (#. +DOUBLE_MARKER+ (decode-double in))
      (#. +STRING_MARKER+ (decode-string in))
      (#. +XML_DOC_MARKER+ (decode-xml-doc in))
      (#. +DATE_MARKER+ (decode-date in))
      (#. +ARRAY_MARKER+ (decode-array in))
      (#. +OBJECT_MARKER+ (decode-object in))
      (#. +XML_MARKER+ (decode-xml in))
      (#. +BYTE_ARRAY_MARKER+ (decode-byte-array in)))))
