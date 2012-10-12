(in-package :rtmp.client)

(defun connect (io connect-params)
  (with-log-section ("net-connection.connect")
    (rtmp.message:write io (rtmp.message:connect connect-params))
    (force-output io)

    (let ((msg-state (rtmp.message:make-initial-state)))
      (loop FOR msg = (rtmp.message:read io msg-state)
        DO
        (show-log "recv message# ~a" (rtmp.message:show msg))
        (return)))
    ))
