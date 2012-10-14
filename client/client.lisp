(in-package :rtmp.client)

(defun receive-loop (io state)
  (loop WITH win-size = -1  ; TODO: ちゃんと処理する
        WITH target-stream-id = -1 ; TODO: ちゃんと処理する
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
       (setf target-stream-id (rtmp.message::stream-begin-target-stream-id msg)))
        
      (rtmp.message:_result 
       (return (values msg win-size target-stream-id)))
        
      (rtmp.message:on-status
       (return (values msg win-size target-stream-id)))
      
      (rtmp.message:message-base
       (show-log "receve unknown message# ~a" (rtmp.message:show msg)))
      )))

(defun convert-result (result)
  (let ((name (intern (string-upcase (rtmp.message::command-base-name result)) :keyword))) ; XXX: gc
    (ecase name
      (:_result (values t
                        (rtmp.message::_result-properties result)
                        (rtmp.message::_result-information result)))
      (:onStatus (values t
                         (rtmp.message::on-status-field1 result)
                         (rtmp.message::on-status-field2 result)))
      )))

(defun connect (io connect-params &key (state (rtmp.message:make-initial-state)))
  (with-log-section ("net-connection.connect")
    (rtmp.message:write io (rtmp.message:connect connect-params))
    (force-output io)

    (let ((result (receive-loop io state)))
      (convert-result result))))

(defun create-stream (io &key state &aux (transaction-id 4)) ; XXX: 適当
  (with-log-section ("net-connection.create-stream")
    (rtmp.message:write io (rtmp.message:create-stream transaction-id :null))
    (force-output io)
    
    (let ((result (receive-loop io state)))
      (convert-result result))))

(defun publish (io stream-id publishing-name publishing-type &key state &aux (transaction-id 0))
  (with-log-section ("net-stream.publish")
    (rtmp.message:write io (rtmp.message:publish transaction-id publishing-name publishing-type
                                                 :stream-id stream-id))
    (force-output io)
    
    (let ((result (receive-loop io state)))
      (convert-result result))))


(defun play (io stream-id stream-name start duration reset &key state)
  (with-log-section ("net-stream.play")
    (rtmp.message:write io (rtmp.message:play stream-name start duration reset 
                                              :stream-id stream-id
                                              :timestamp 0))

    #+C
    (let ((buffer-length 256)) ; XXX:
      (rtmp.message:write io (rtmp.message:set-buffer-length stream-id buffer-length
                                                             :timestamp 0)))
    (force-output io)

    (loop FOR (ok? null info) = (multiple-value-list (convert-result (receive-loop io state)))
          WHILE (and ok?
                     (not (equal "NetStream.Play.Start"
                                 (second (assoc "code" (second info) :test #'string=)))))
          FINALLY
          (return info))))

(defun receive (io &key state)
  (receive-loop io state))
