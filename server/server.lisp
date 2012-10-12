(in-package :rtmp.server)

(defparameter *default-ack-win-size* 2500000)

(defun handle-connect (io msg state)
  (with-log-section ("handle-connect")
    (rtmp.message:write io (rtmp.message:ack-win-size *default-ack-win-size*))
    (rtmp.message:write io (rtmp.message:set-peer-bandwidth *default-ack-win-size* 2))
    (rtmp.message:write io (rtmp.message:stream-begin 0)) ;; XXX: 必要? (and 適切?)

    (let ((props '())
          (infos '()))
      (rtmp.message:write io (rtmp.message:_result 1 props infos)))
    
    (force-output io)
    ))

(defun start (io)
  (handshake io)
  
  (with-log-section ("read-loop")
    (loop WITH state = (rtmp.message:make-initial-state)
          FOR msg = (rtmp.message:read io state)
      DO
      (etypecase msg
        (rtmp.message:connect (handle-connect io msg state))
        (rtmp.message:ack-win-size :ignore) ; TODO:
        )))
  
  (values))
