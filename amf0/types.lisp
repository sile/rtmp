(in-package :rtmp.amf0)

;; TODO: types => value ?
;; TODO: tab => space

;; TODO: doc: mapping-rule

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct marker
	(name       t :type keyword :read-only t)
	(code       t :type octet   :read-only t)
	(supported? t :type boolean :read-only t)
	(special?   t :type boolean :read-only t))

  (defun marker (name code supported? special?)
	(make-marker :name name 
				 :code code
				 :supported? supported?
				 :special? special?))

  (defparameter *markers*
	(list (marker :number         #x00 t nil)
		  (marker :boolean        #x01 t nil)
		  (marker :string         #x02 t nil)
		  (marker :object         #x03 t nil)
		  (marker :movieclip      #x04 nil nil)
		  (marker :null           #x05 t nil)
		  (marker :undefined      #x06 t nil)
		  (marker :reference      #x07 t nil)
		  (marker :ecma_array     #x08 t nil)
		  (marker :object_end     #x09 t t)
		  (marker :strict-array   #x0A t nil)
		  (marker :date           #x0B t nil)
		  (marker :long-string    #x0C t nil)
		  (marker :unsupported    #x0D t t)
		  (marker :recordset      #x0E nil nil)
		  (marker :xml-document   #x0F t nil)
		  (marker :typed-object   #x10 t nil)
		  (marker :avmplus-object #x11 t t))))
