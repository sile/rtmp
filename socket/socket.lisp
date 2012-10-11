(in-package :rtmp.socket)

(defmacro with-client-socket-stream ((stream server-host server-port) &body body)
  (let ((sock (gensym))
        (addr (gensym)))
    `(let ((,sock (make-instance 'inet-socket :type :stream :protocol :tcp)))
       (unwind-protect
           (let ((,addr (host-ent-address (get-host-by-name ,server-host))))
             (socket-connect ,sock ,addr ,server-port)
             (let ((,stream (socket-make-stream ,sock :input t :output t :element-type '(unsigned-byte 8))))
               ,@body))
         (socket-close ,sock)))))
       