(in-package :rtmp.client)

(defun connect-loop (io msg-state)
  (loop WITH win-size = -1  ; TODO: ちゃんと処理する
        WITH target-stream=id = -1 ; TODO: ちゃんと処理する
        FOR msg = (rtmp.message:read io msg-state)
    DO
    (etypecase msg
      (rtmp.message:ack-win-size 
       (setf win-size (rtmp.message::ack-win-size-size msg)))
        
      (rtmp.message:set-chunk-size
       (setf (rtmp.message::state-chunk-size msg-state) (rtmp.message::set-chunk-size-size msg)))

      (rtmp.message:set-peer-bandwidth 
       (setf win-size (rtmp.message::set-peer-bandwidth-size msg))
       (rtmp.message:write io (rtmp.message:set-peer-bandwidth win-size 2))
       (force-output io))
        
      (rtmp.message:stream-begin 
       (setf target-stream=id (rtmp.message::stream-begin-target-stream-id msg)))
        
      (rtmp.message:_result 
       (return (values msg win-size target-stream=id)))
        
      (rtmp.message:message-base
       (show-log "receve unknown message# ~a" (rtmp.message:show msg)))
      )))
  
(defun connect (io connect-params &key (state (rtmp.message:make-initial-state)))
  (with-log-section ("net-connection.connect")
    (rtmp.message:write io (rtmp.message:connect connect-params))
    (force-output io)

    (let* ((result (connect-loop io state))
           (name (intern (string-upcase (rtmp.message::_result-name result)) :keyword))) ; XXX: gc
      (ecase name
        (:_result (values t
                          (rtmp.message::_result-properties result)
                          (rtmp.message::_result-information result)))
        ))))

