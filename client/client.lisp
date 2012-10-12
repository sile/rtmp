(in-package :rtmp.client)

(defun connect-loop (io state)
  (loop WITH win-size = -1  ; TODO: ちゃんと処理する
        WITH target-stream=id = -1 ; TODO: ちゃんと処理する
        FOR msg = (rtmp.message:read io state)
    DO
    (etypecase msg
      (rtmp.message:ack-win-size 
       (setf win-size (rtmp.message::ack-win-size-size msg)))
        
      (rtmp.message:set-chunk-size
       (setf (rtmp.message::state-chunk-size state) (rtmp.message::set-chunk-size-size msg)))

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

(defun create-stream-loop (io state)
  (loop WITH win-size = -1  ; TODO: ちゃんと処理する
        WITH target-stream=id = -1 ; TODO: ちゃんと処理する
        FOR msg = (rtmp.message:read io state)
    DO
    (etypecase msg
      (rtmp.message:ack-win-size 
       (setf win-size (rtmp.message::ack-win-size-size msg)))
        
      (rtmp.message:set-chunk-size
       (setf (rtmp.message::state-chunk-size state) (rtmp.message::set-chunk-size-size msg)))

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
  
(defun convert-result (result)
  (let ((name (intern (string-upcase (rtmp.message::_result-name result)) :keyword))) ; XXX: gc
    (ecase name
      (:_result (values t
                        (rtmp.message::_result-properties result)
                        (rtmp.message::_result-information result)))
      )))

(defun connect (io connect-params &key (state (rtmp.message:make-initial-state)))
  (with-log-section ("net-connection.connect")
    (rtmp.message:write io (rtmp.message:connect connect-params))
    (force-output io)

    (let ((result (connect-loop io state)))
      (convert-result result))))

(defun create-stream (io &key state &aux (transaction-id 4)) ; XXX: 適当
  (with-log-section ("net-connection.create-stream")
    (rtmp.message:write io (rtmp.message:create-stream transaction-id :null))
    (force-output io)
    
    (let ((result (create-stream-loop io state)))
      (convert-result result))))

