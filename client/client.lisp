(in-package :rtmp.client)

(defun connect (io connect-params)
  (with-log-section ("net-connection.connect")
    (rtmp.message:write io (rtmp.message:connect connect-params))
    (force-output io)

    (loop WITH msg-state = (rtmp.message:make-initial-state)
          FOR msg = (rtmp.message:read io msg-state)
      DO
      (etypecase msg
        (rtmp.message:ack-win-size :todo)
        (rtmp.message:set-peer-bandwidth :todo)
        (rtmp.message:stream-begin :todo)
        (rtmp.message:_result :todo)
        ))))
