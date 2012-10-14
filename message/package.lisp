(defpackage rtmp.message
  (:use :common-lisp :rtmp.utils)
  (:shadow write read)
  (:export ;;; command
           connect
           _result
           _error
           create-stream
           delete-stream
           close-stream
           publish
           play
           
           on-bandwidth-done
           release-stream
           fcpublish
           FCUnpublish
           on-status

           ;;; data
           data-base

           ;;; 
           video 
           audio

           ;;; protocol control
           set-chunk-size
           ack-win-size
           set-peer-bandwidth

           ;;; user control
           stream-begin
           set-buffer-length
           stream-is-recorded
           ping-request
           ping-response

           buffer-ready
           buffer-empty
           
           unknown-ucm

           ;;;
           message-base

           show
           make-initial-state
           change-state-chunk-size
           read
           write))
(in-package :rtmp.message)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))
