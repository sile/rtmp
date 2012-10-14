;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(define-symbol-macro s
  (rtmp.socket:with-accepted-socket-stream (io 8080)
    (rtmp.server:start io)
    ))

(defparameter *connect-params*
  '(("app" "live")
    ("tcUrl" "rtmp://192.168.100.103/live")
    ("type" "nonprivate")
    ("flashVer" "FMLE/3.0 (compatible; FMSc/1.0)") 
    ("swfUrl" "rtmp://192.168.100.103/live")
    ("audioCodecs" #x0FFF)
    ("videoCodecs" #x00FF)
    ))

;;; publish
(rtmp.socket:with-client-socket-stream (io "localhost" 1935)
  (rtmp.client:handshake io)

  (let ((state (rtmp.message:make-initial-state)))
    (rtmp.client:connect io *connect-params* :state state)
    (multiple-value-bind (_ __ target-stream-id)
                         (rtmp.client:create-stream io :state state)
      (declare (ignore _ __))
      
      (let ((publishing-name "livestream")
            (publishing-type "live"))
        (rtmp.client:publish io (floor target-stream-id)
                             publishing-name publishing-type
                             :state state)
        
        ))))

;    message: ("play" "18:1234:0" 0 (livestream -2.0d0 -1.0d0 FALSE))

;;; play
(rtmp.socket:with-client-socket-stream (io "localhost" 1935)
  (rtmp.client:handshake io)

  (let ((state (rtmp.message:make-initial-state)))
    (rtmp.client:connect io *connect-params* :state state)
    (multiple-value-bind (_ __ target-stream-id)
                         (rtmp.client:create-stream io :state state)
      (declare (ignore _ __))

      (let ((stream-name "livestream")
            (start -2)
            (duration -1)
            (reset :false)
            (target-stream-id (round target-stream-id)))

        (rtmp.client:play io target-stream-id stream-name start duration reset
                          :state state)
        
        (rtmp.client:receive io :state state)
        
        ))))


;; TODO: ack-win-sizeごとのメッセージ送信

  

; "livestream?videoKeyframeFrequency=5&totalDatarate=596"

;  receve unknown message# ("onStatus" "20:1:0" 1.1125369292536007d-308 (NULL (MAP ((level status) (code NetStream.Publish.Start) (description livestream is now published.) (clientid oAArAAAA)))))

'("@setDataFrame" "onMetaData"
 (:MAP
  (("author" "")
   ("copyright" "") 
   ("description" "")
   ("keywords" "")
   ("rating" "") 
   ("title" "") 
   ("presetname" "Custom")
   ("creationdate" "Sat Oct 13 03:46:58 2012")
   ("videodevice" "ManyCam Virtual Webcam")
   ("framerate" 15.0d0)
   ("width" 320.0d0) 
   ("height" 240.0d0) 
   ("videocodecid" "avc1")
   ("videodatarate" 500.0d0) 
   ("avclevel" 31.0d0)
   ("avcprofile" 66.0d0)
   ("videokeyframe_frequency" 5.0d0)
   ("audiodevice" "ManyCam Virtual Microphone")
   ("audiosamplerate" 44100.0d0)
   ("audiochannels" 2.0d0) 
   ("audioinputvolume" 75.0d0) 
   ("audiocodecid" ".mp3")
   ("audiodatarate" 96.0d0))))


(:MAP (("author" "")
       ("copyright" "") 
       ("description" "")
       ("keywords" "")
       ("rating" "")
       ("title" "")
       ("presetname" "Custom")
       ("creationdate" "Sun Oct 14 20:54:16 2012")
       ("videodevice" "ManyCam Virtual Webcam")
       ("framerate" 15.0d0)
       ("width" 320.0d0)
       ("height" 240.0d0)
       ("videocodecid" "avc1")
       ("videodatarate" 500.0d0)
       ("avclevel" 31.0d0)
       ("avcprofile" 66.0d0)
       ("videokeyframe_frequency" 5.0d0)
       ("audiodevice" "ManyCam Virtual Microphone")
       ("audiosamplerate" 44100.0d0)
       ("audiochannels" 2.0d0)
       ("audioinputvolume" 75.0d0)
       ("audiocodecid" ".mp3")
       ("audiodatarate" 96.0d0)))
