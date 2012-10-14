(in-package :rtmp.socket)

(defclass wrapped-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))
     
(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (stream-of stream)))
     
(defmethod close ((stream wrapped-stream) &key abort)
  (close (stream-of stream) :abort abort))
     
(defclass wrapped-binary-stream
  (wrapped-stream fundamental-binary-input-stream fundamental-binary-output-stream)
  ())
     
(defmethod stream-read-byte ((stream wrapped-binary-stream))
  (read-byte (stream-of stream) nil :eof))

(defmethod stream-write-byte ((stream wrapped-binary-stream) byte)
  (write-byte byte (stream-of stream))
  byte)

(defmethod stream-write-sequence ((stream wrapped-binary-stream) seq &optional start end)
  (write-sequence seq (stream-of stream) :start start :end end))

(defmethod stream-read-sequence ((stream wrapped-binary-stream) seq &optional start end)
  (read-sequence seq (stream-of stream) :start start :end end))

(defmethod stream-force-output ((stream wrapped-binary-stream))
  (force-output (stream-of stream)))

(defclass counting-binary-stream
  (wrapped-binary-stream)
  ((read-byte :initform 0 :accessor read-count-of)))

(defmethod stream-read-byte ((stream counting-binary-stream))
  (let ((byte (call-next-method)))
    (if (eq byte :eof)
        :eof
      (progn (incf (read-count-of stream))
             byte))))

(defmethod stream-read-sequence ((stream counting-binary-stream) seq &optional start end)
  ;; XXX:
  (let ((start (or start 0))
        (end (or end (length seq))))
    (incf (read-count-of stream) (- end start)))
  (call-next-method))

(defun read-byte-count (in)
  (declare (counting-binary-stream in))
  (read-count-of in))

(defmacro with-client-socket-stream ((stream server-host server-port) &body body)
  (let ((sock (gensym))
        (addr (gensym)))
    `(let ((,sock (make-instance 'inet-socket :type :stream :protocol :tcp)))
       (unwind-protect
           (let ((,addr (host-ent-address (get-host-by-name ,server-host))))
             (socket-connect ,sock ,addr ,server-port)
             (let* ((,stream 
                     (socket-make-stream ,sock :input t :output t :element-type '(unsigned-byte 8)))
                    (,stream (make-instance 'counting-binary-stream :stream ,stream)))
               ,@body))
         (socket-close ,sock)))))

(defmacro with-accepted-socket-stream ((stream port) &body body)
  (let ((sock (gensym))
        (client-sock (gensym))
        (addr (gensym)))
    `(let ((,sock (make-instance 'inet-socket :type :stream :protocol :tcp)))
       (unwind-protect
           (let ((,addr #(0 0 0 0)))
             (setf (sockopt-reuse-address ,sock) t)
             (socket-bind ,sock ,addr ,port)
             (socket-listen ,sock 256)
             (let* ((,client-sock (socket-accept ,sock)))
               (unwind-protect
                   (let* ((,stream (socket-make-stream ,client-sock
                                          :input t :output t :element-type '(unsigned-byte 8)))
                          (,stream (make-instance 'counting-binary-stream :stream ,stream)))
                     ,@body)
                 (socket-close ,client-sock))))
         (socket-close ,sock)))))
