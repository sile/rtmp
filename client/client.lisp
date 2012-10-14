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
      
      (rtmp.message:video 
       (return msg)
       #+IGNORE
       (ignore-errors
         (let ((pkt (rtmp.flv:decode-video-packet-bytes 
                     (rtmp.message::video-data msg))))
           (show-log "recv video# ~a" pkt)
           (return pkt))))
    
      (rtmp.message:audio
       (return msg)
       #+IGNORE
       (ignore-errors
         (let ((pkt (rtmp.flv:decode-audio-packet-bytes 
                     (rtmp.message::audio-data msg))))
           (show-log "recv audio# ~a" pkt)
           (return pkt))))
      
      (rtmp.message:data-base
       (return msg))
      
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

    (loop FOR msg = (receive-loop io state)
          UNTIL (typep msg 'rtmp.message::command-base)
          DO (show-log "drop# ~a" (rtmp.message:show msg))
          FINALLY (return (convert-result msg)))))

(defun create-stream (io &key state &aux (transaction-id 4)) ; XXX: 適当
  (with-log-section ("net-connection.create-stream")
    (rtmp.message:write io (rtmp.message:create-stream transaction-id :null))
    (force-output io)
    
    (loop FOR msg = (receive-loop io state)
          UNTIL (typep msg 'rtmp.message::command-base)
          DO (show-log "drop# ~a" (rtmp.message:show msg))
          FINALLY (return (convert-result msg)))))

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

(defun rtmpdump-receive-metadata (io state)
  (loop WITH pendings = '()
        FOR rlt = (rtmp.client:receive io :state state)
        UNTIL (and (typep rlt 'rtmp.message:data-base)
                   (some (lambda (x)
                           (equal "onMetaData" x))
                         (rtmp.message::data-base-data rlt)))
        DO
        (typecase rlt
          ((or rtmp.message:video rtmp.message:audio) (push rlt pendings))
          (otherwise
           (show-log "drop# ~a" (rtmp.message:show rlt))))
        FINALLY
        (return (values (rtmp.message::data-base-data rlt)
                        pendings))))

(defun rtmpdump-play (io target-stream-id stream-name start duration reset state output)
  (let ((play-result 
         (rtmp.client:play io target-stream-id stream-name start duration reset
                           :state state)))
    (assert (equal (second (assoc "code" (second play-result) :test #'string=))
                   "NetStream.Play.Start")
            () "play failed")
    
    ;; header
    (rtmp.flv:header-write output)

    ;; first tag
    (multiple-value-bind (metadata pendings) 
                         (rtmpdump-receive-metadata io state)
      (let* ((data (with-output-to-bytes (out)
                     (dolist (x metadata)
                       (rtmp.amf0:encode x out))))
             (prev-tag-size
              (rtmp.flv:tag-write output 
                                  data
                                  :prev-tag-size 0
                                  :tag-type :script-data
                                  :stream-id target-stream-id
                                  :timestamp 0)))
        
        (flet ((dump-msg (msg)
                 (typecase msg
                   (rtmp.message:video 
                    (let ((data (rtmp.message::video-data msg))
                          (timestamp (rtmp.message::video-timestamp msg)))
                      (setf prev-tag-size
                            (rtmp.flv:tag-write output
                                                data
                                                :prev-tag-size prev-tag-size
                                                :tag-type :video
                                                :stream-id target-stream-id
                                                :timestamp timestamp))))
                   
                   (rtmp.message:audio 
                    (let ((data (rtmp.message::audio-data msg))
                          (timestamp (rtmp.message::audio-timestamp msg)))
                      (setf prev-tag-size
                            (rtmp.flv:tag-write output
                                                data
                                                :prev-tag-size prev-tag-size
                                                :tag-type :audio
                                                :stream-id target-stream-id
                                                :timestamp timestamp))))
                   
                   (otherwise
                    (show-log "drop# ~a" (rtmp.message:show msg))))))
          
          ;; pending tags
          (loop FOR msg IN pendings
                DO (dump-msg msg))

          ;; rest tags
          (loop FOR msg = (rtmp.client:receive io :state state)
                DO (dump-msg msg)))))))
  
(defun rtmpdump-impl (io &key url app stream-name output)
  (declare (string url app stream-name)
           (stream output))

   (rtmp.client:handshake io)
   
   (let ((state (rtmp.message:make-initial-state))
         (connect-params `(("app" ,app)
                           ("tcUrl" ,(format nil "~a/~a" (string-right-trim "/" url) app))
                           ("flashVer" "FMLE/3.0 (compatible; FMSc/1.0)")
                           ("type" "nonprivate")
                           ("audioCodecs" #x0FFF)
                           ("videoCodecs" #x00FF))))
    (rtmp.client:connect io connect-params :state state)
    (multiple-value-bind (_ __ target-stream-id)
                         (rtmp.client:create-stream io :state state)
      (declare (ignore _ __))

      (let ((start -2)
            (duration -1)
            (reset :false)
            (target-stream-id (round target-stream-id)))
        (unwind-protect
            (rtmpdump-play io target-stream-id stream-name start duration reset state output)
          (progn
            (rtmp.message:write io (rtmp.message:close-stream 0 :field1 target-stream-id))
            (rtmp.message:write io (rtmp.message:delete-stream 0 target-stream-id))))))))

(defun rtmpdump (rtmp-host rtmp-port app stream-name output-file)
  (rtmp.socket:with-client-socket-stream (io rtmp-host rtmp-port)
    (with-open-file (out output-file :direction :output
                                     :if-exists :supersede
                                     :element-type 'octet)
      (unwind-protect
          (rtmpdump-impl io
                         :url (format nil "rtmp://~a:~d/" rtmp-host rtmp-port)
                         :app app
                         :stream-name stream-name
                         :output out)
        (close out)))))
