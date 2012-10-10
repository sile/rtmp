(defpackage amf0
  (:use :common-lisp)
  )
(in-package :amf0)

(deftype octet () '(unsigned-byte 8))
(deftype octets (&optional size) 
  (if size
      `(vector octets ,size)
    '(vector octet)))
(deftype simple-octets (&optional size) 
  (if size
      `(simple-array octet (,size))
    '(simple-array octet)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +NUMBER_MARKER+ #x00)
(defconstant +BOOLEAN_MARKER+ #x01)
(defconstant +STRING_MARKER+ #x02)
(defconstant +OBJECT_MARKER+ #x03)
(defconstant +MOVIECLIP_MARKER+ #x04)  ; reserved, not supported
(defconstant +NULL_MARKER+ #x05)
(defconstant +UNDEFINED_MARKER+ #x06)
(defconstant +REFERENCE_MARKER+ #x07)
(defconstant +ECMA_ARRAY_MARKER+ #x08)
(defconstant +OBJECT_END_MARKER+ #x09)
(defconstant +STRICT_ARRAY_MARKER+ #x0A)
(defconstant +DATE_MARKER+ #x0B)
(defconstant +LONG_STRING_MARKER+ #x0C)
(defconstant +UNSUPPORTED_MARKER+ #x0D)
(defconstant +RECORDSET_MARKER+ #x0E) ; reserved, not supported
(defconstant +XML_DOCUMENT_MARKER+ #x0F)
(defconstant +TYPED_OBJECT_MARKER+ #x10)
(defconstant +AVMPLUS_OBJECT_MARKER+ #x11) ; indicate to switch to AMF3
)

(defun to-octets (list)
  (let ((octets (make-array (length list) :element-type '(unsigned-byte 8))))
    (loop FOR i FROM 0
	  FOR x IN list
	  DO (setf (aref octets i) x))
    octets))

(defun to-bytes (size n)
  (loop FOR i FROM (1- size) DOWNTO 0
	COLLECT (ldb (byte 8 (* i 8)) n)))

(defun read-int (length in)
  (loop FOR i FROM (1- length) DOWNTO 0 
	SUM (ash (read-byte in) (* i 8))))

(defun read-bytes (length in)
  (let ((bytes (make-array length :element-type 'octet)))
    (read-sequence bytes in)
    bytes))

(defun to-flat-octets (&rest values)
  (to-octets
   (loop FOR v IN values
	 APPEND
	 (etypecase v
	   (octet (list v))
	   (list (if (typep (car v) 'simple-octets)
		     (coerce (apply #'to-flat-octets v) 'list)
		   v))
	   (simple-octets (coerce v 'list))
	   ))))

(defstruct value-type)

;; number
(defstruct (number-type (:include value-type))
  (value t :type double-float))

(defun decode-double (code)
  (let ((sign     (ldb (byte  1 63) code))
	(exponent (ldb (byte 11 52) code))
	(fraction (ldb (byte 52  0) code)))
    (assert (not (and (= exponent #b11111111111) (= fraction 0)))) ; infinity
    (assert (not (and (= exponent #b11111111111) (/= fraction 0)))); NaN
    
    (coerce 
     (* (if (zerop sign) 1 -1)
	(scale-float (+ 1.0d0 (* fraction (expt 2 -52)))
		     (- exponent 1023)))
     'double-float)))
  
(defun decode-number (in)
  (let ((code (read-int 8 in)))
    (make-number-type :value (decode-double code))))

(defun encode-double (double)
  (multiple-value-bind (fraction exponent sign) 
		       (integer-decode-float double)
    (let ((code 0))
      (setf (ldb (byte  1 63) code) (if (plusp sign) 0 1)
	    (ldb (byte 11 52) code) (+ exponent 52 1023)
	    (ldb (byte 52  0) code) fraction)
      (to-bytes 8 code))))
  
(defmethod encode ((v number-type))
  (with-slots (value) v
    (to-flat-octets (to-bytes 2 +NUMBER_MARKER+) (encode-double value))))

;; boolean
(defstruct (boolean-type (:include value-type))
  (value t :type boolean))

(defun decode-boolean (in)
  (make-boolean-type :value (/= (read-byte in) 0)))

(defmethod encode ((v boolean-type))
  (to-flat-octets (to-bytes 2 +BOOLEAN_MARKER+) (if (boolean-type-value v) 1 0)))

;; string
(defstruct (string-type (:include value-type))
  (value t :type string))

(defun decode-string (in)
  (let ((length (read-int 2 in)))
    (make-string-type :value (creole:octets-to-string (read-bytes length in)))))

(defun encode-string (s)
  (let* ((bytes (creole:string-to-octets s))
	 (len (length bytes)))
    (to-flat-octets
     (to-bytes (if (< len #x10000) 2 4) len)
     bytes)))

(defmethod encode ((v string-type))
  (to-flat-octets
   (to-bytes 2 +STRING_MARKER+)
   (encode-string (string-type-value v))))

;; long string
(defstruct (long-string-type (:include value-type))
  (value t :type string))

(defun decode-long-string (in)
  (let ((length (read-int 4 in)))
    (make-long-string-type :value (creole:octets-to-string (read-bytes length in)))))

(defmethod encode ((v long-string-type))
  (to-flat-octets
   (to-bytes 4 +LONG_STRING_MARKER+)
   (encode-string (long-string-type-value v))))

;; XML document
(defstruct (xml-document-type (:include value-type))
  (value t :type string))

(defun decode-xml-document (in)
  (let ((length (read-int 4 in)))
    (make-xml-document-type :value (creole:octets-to-string (read-bytes length in)))))

(defmethod encode ((v xml-document-type))
  (to-flat-octets
   (to-bytes 4 +XML_DOCUMENT_MARKER+)
   (encode-string (xml-document-type-value v))))

;; object
(defstruct (object-type (:include value-type))
  (value t :type list))  ; (key value)

(defstruct (object-end-type (:include value-type)))

(defun decode-object (in)
  (loop FOR (key v) = (list (print (decode-string in)) (decode in))
	UNTIL (object-end-type-p v)
	COLLECT (print (list key v)) INTO list
	FINALLY (return (make-object-type :value list))))

(defmethod encode ((v object-type))
  (to-flat-octets
   (to-bytes 2 +OBJECT_MARKER+)
   (loop FOR (key x) IN (object-type-value v)
	 COLLECT (to-flat-octets (encode-string key) (encode x)))
   (to-bytes 2 +OBJECT_END_MARKER+)))

;; typed object
(defstruct (typed-object-type (:include value-type))
  (class-name t :type string)
  (value t :type list)) ; (key value)

(defun decode-typed-object (in)
  (let ((class-name (decode-string in)))
    (loop FOR (key v) = (list (decode-string in) (decode in))
	  UNTIL (object-end-type-p v)
	  COLLECT (list key v) INTO list
	  FINALLY (return (make-typed-object-type
			   :class-name class-name
			   :value list)))))

(defmethod encode ((v typed-object-type))
  (to-flat-octets
   (to-bytes 2 +TYPED_OBJECT_MARKER+)
   (encode-string (typed-object-type-class-name v))
   (loop FOR (key x) IN (typed-object-type-value v)
	 COLLECT (to-flat-octets (encode-string key) (encode x)))
   (to-bytes 2 +OBJECT_END_MARKER+)))


;; null
(defstruct (null-type (:include value-type)))

(defmethod encode ((v null-type))
  (to-flat-octets (to-bytes 2 +NULL_MARKER+)))

;; undefined
(defstruct (undefined-type (:include value-type)))

(defmethod encode ((v undefined-type))
  (to-flat-octets (to-bytes 2 +UNDEFINED_MARKER+)))

;; unsupported
(defstruct (unsupported-type (:include value-type)))

(defmethod encode ((v unsupported-type))
  (to-flat-octets (to-bytes 2 +UNSUPPORTED_MARKER+)))

;; reference
(defstruct (reference-type (:include value-type))
  (value t :type (unsigned-byte 16)))

(defun decode-reference (in)
  (make-reference-type :value (read-int 2 in)))

(defmethod encode ((v reference-type))
  (to-flat-octets 
   (to-bytes 2 +REFERENCE_MARKER+)
   (to-bytes 2 (reference-type-value v))))

;; ECMA Aarray
(defstruct (ecma-array-type (:include value-type))
  (value t :type list)) ; (key value)

(defun decode-ecma-array (in)
  (let ((size (read-int 4 in)))
    (loop REPEAT size
	  FOR (key v) = (list (decode-string in) (decode in))
	  COLLECT (list key v) INTO list
	  FINALLY (return (make-ecma-array-type :value list)))))

(defmethod encode ((v ecma-array-type))
  (to-flat-octets
   (to-bytes 2 +ECMA_ARRAY_MARKER+)
   (to-bytes 4 (ecma-array-type-value v))
   (loop FOR (key x) IN (ecma-array-type-value v)
	 APPEND (append (encode-string key) (encode x)))))

;; strict array
(defstruct (array-type (:include value-type))
  (value t :type list))

(defun decode-array (in)
  (let ((size (read-int 4 in)))
    (loop REPEAT size
	  FOR v = (decode in)
	  COLLECT v INTO list
	  FINALLY (return (make-array-type :value list)))))

(defmethod encode ((v array-type))
  (to-flat-octets
   (to-bytes 2 +STRICT_ARRAY_MARKER+)
   (to-bytes 4 (array-type-value v))
   (loop FOR x IN (array-type-value v)
	 APPEND (encode x))))

;; date
(defstruct (date-type (:include value-type))
  (value t :type double-float)
  (timezone 0 :type (unsigned-byte 16))) ; shuld be 0

(defun decode-date (in)
  (let ((code (read-int 8 in)))
    (make-date-type :value (decode-double code)
		    :timezone (read-int 2 in))))

(defmethod encode ((v date-type))
  (to-flat-octets
   (to-bytes 2 +DATE_MARKER+)
   (encode-double (date-type-value v))
   (to-bytes 2 (date-type-timezone v))))

;; 
(defun decode (in)
  (let ((marker (read-byte in)))
    (ecase marker
      (#.+NUMBER_MARKER+ (decode-number in))
      (#.+BOOLEAN_MARKER+ (decode-boolean in))
      (#.+STRING_MARKER+ (decode-string in))
      (#.+LONG_STRING_MARKER+ (decode-long-string in))
      (#.+XML_DOCUMENT_MARKER+ (decode-xml-document in))
      (#.+OBJECT_MARKER+ (decode-object in))
      (#.+TYPED_OBJECT_MARKER+ (decode-typed-object in))
      (#.+OBJECT_END_MARKER+ (make-object-end-type))
      (#.+NULL_MARKER+ (make-null-type))
      (#.+UNDEFINED_MARKER+ (make-undefined-type))
      (#.+UNSUPPORTED_MARKER+ (make-unsupported-type))
      (#.+REFERENCE_MARKER+ (decode-reference in))
      (#.+ECMA_ARRAY_MARKER+ (decode-ecma-array in))
      (#.+STRICT_ARRAY_MARKER+ (decode-array in))
      (#.+DATE_MARKER+ (decode-date in))
      )))

;; amf packet
(defstruct header
  (name t :type string)
  (must-understand t :type boolean)
  (length 0 :type (unsigned-byte 32))  ; (U32)-1 if unknown
  (value t :type value-type))

(defstruct message
  (target-uri t :type string)
  (response-uri t :type string)
  (length 0 :type (unsigned-byte 32))  ; (U32)-1 if unknown
  (value t :type value-type))

(defstruct packet
  (version 0 :type (unsigned-byte 16)) ; value must be 0 for AMF 0
;  (header-count 0 :type (unsigned-byte 16))
  (headers t :type list)
;  (message-count 0 :type (unsigned-byte 16))
  (messages t :type list))

(defun decode-header (in)
  (let ((name (decode-string in))
	(must-understand (read-byte in))
	(length (read-int 4 in))
	(value (decode in)))
    (make-header :name name
		 :must-understand (/= must-understand 0)
		 :length length
		 :value value)))

(defun decode-message (in)
  (let ((target-uri (decode-string in))
	(response-uri (decode-string in))
	(length (read-int 4 in))
	(value (decode in)))
    (make-message :target-uri target-uri
		  :response-uri response-uri
		  :length length
		  :value value)))

(defmethod encode ((v header))
  (with-slots (name must-understand length value) v
    (to-flat-octets
     (encode-string name)
     (if must-understand 1 0)
     (to-bytes 4 length)
     (encode value))))

(defmethod encode ((v message))
  (with-slots (target-uri response-uri length value) v
    (to-flat-octets
     (encode-string target-uri)
     (encode-string response-uri)
     (to-bytes 4 length)
     (encode value))))

(defmethod encode ((v packet))
  (with-slots (version headers messages) v
    (to-flat-octets
     (to-bytes 2 version) ; must be 0
     (to-bytes 2 (length headers))
     (apply #'to-flat-octets (loop FOR x IN headers COLLECT (encode x)))
     (to-bytes 2 (length messages))
     (apply #'to-flat-octets (loop FOR x IN messages COLLECT (encode x)))
     )))

(defun decode-packet (in)
  (let ((version (read-int 2 in)))
    (assert (= version 0) () "version must be 0: ~a" version)

    (let* ((header-count (read-int 2 in))
	   (headers (loop REPEAT header-count COLLECT (decode-header in)))
	   (message-count (read-int 2 in))
	   (messages (loop REPEAT message-count COLLECT (decode-message in)))
	   )
      (make-packet :version version
		   :headers headers
		   :messages messages)
      )))
