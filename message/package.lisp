(defpackage rtmp.message
  (:use :common-lisp :rtmp.utils)
  (:shadow write read)
  (:export ;;; command
           connect
           _result

           ;;; protocol control
           ack-win-size
           set-peer-bandwidth

           ;;; user control
           stream-begin

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
