(in-package :rtmp.amf0)

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

