(defpackage rtmp.flv
  (:use :common-lisp :rtmp.utils)
  (:export decode
           decode-bytes
           
           decode-audio-packet
           decode-audio-packet-bytes

           decode-video-packet
           decode-video-packet-bytes

           packet-header
           packet-data
           packet-write

           header-write
           tag-write
           ))
(in-package :rtmp.flv)

(deftype octet () '(unsigned-byte 8))

(deftype octets (&optional (size nil))
  (if size
      `(vector octet ,size)
    '(vector octet)))
