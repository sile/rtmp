(in-package :rtmp.utils)

(defun encode-double-float (double)
  (declare (double-float double))

  (multiple-value-bind (fraction exponent sign) 
	                   (integer-decode-float double)
    (let ((code 0))
	  (declare ((unsigned-byte 64) code))
      (setf (ldb (byte  1 63) code) (if (plusp sign) 0 1)
			(ldb (byte 11 52) code) (+ exponent 52 1023)
			(ldb (byte 52  0) code) fraction)

	  code)))

(defun decode-double-float (code)
  (declare ((unsigned-byte 64) code))
  (let ((sign     (ldb (byte  1 63) code))
		(exponent (ldb (byte 11 52) code))
		(fraction (ldb (byte 52  0) code)))
    (assert (not (and (= exponent #b11111111111) (= fraction 0)))) ; infinity
    (assert (not (and (= exponent #b11111111111) (/= fraction 0)))); NaN
  
	(* (if (zerop sign) 1 -1)
	   (scale-float (+ 1.0d0 (* fraction (expt 2 -52)))
					(- exponent 1023)))))

(defmacro read-uint (byte-width in &key (endian :big))
  (declare ((integer 1 8) byte-width)
		   ((member :big :little) endian))
  `(the (unsigned-byte ,(* byte-width 8))
	 (+ ,@(loop FOR i FROM 0 BELOW byte-width
				FOR offset = (if (eq endian :little) 
								 (* i 8)
							   (* (- byte-width i 1) 8))
				COLLECT `(ash (read-byte ,in) ,offset)))))


(defun read-bytes (length in)
  (let ((buffer (make-array length :element-type '(unsigned-byte 8)))) ; TODO: octet
    ;; TODO: read length check
    (read-sequence buffer in)
    buffer))
    
(defmacro write-uint (byte-width value out &key (endian :big))
  (declare ((integer 1 8) byte-width)
		   ((member :big :little) endian))
  (let ((v (gensym)))
	`(let ((,v ,value))
	   (declare ((unsigned-byte ,(* byte-width 8)) ,v))
	   ,@(loop FOR i FROM 0 BELOW byte-width
			   FOR offset = (if (eq endian :little) 
								(* i 8)
							  (* (- byte-width i 1) 8))
			   COLLECT `(write-byte (ldb (byte 8 ,offset) ,v) ,out))
	   (values))))

(defun write-bytes (bytes out &key (start 0) (end (length bytes)))
  (declare (octets bytes))
  ;; TODO: wrote length check
  (write-sequence bytes out :start start :end end)
  (values))

(defmacro with-output-to-bytes ((out) &body body)
  `(flexi-streams:with-output-to-sequence (,out :element-type 'octet)
     ,@body))

(defmacro with-input-from-bytes ((in bytes) &body body)
  `(flexi-streams:with-input-from-sequence (,in ,bytes)
     ,@body))

(defmacro read-uint-from-bytes (byte-width bytes &key (endian :big))
  `(with-input-from-bytes (in ,bytes)
     (read-uint ,byte-width in :endian ,endian)))

(defparameter *show-log* t)
(defparameter *log-nest* 0)

(defun show-log (fmt &rest args)
  (when *show-log*
    (format *error-output* "~&;~v@t~?~%" (* 2 *log-nest*) fmt args))
  (values))

(defmacro with-log-section ((name) &body body)
  `(when *show-log*
     (show-log "")
     (show-log "[~a]" ,name)
     (let ((*log-nest* (1+ *log-nest*)))
	   ,@body)))


