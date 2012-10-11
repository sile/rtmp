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
