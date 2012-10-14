(defpackage rtmp.socket
  (:use :common-lisp :sb-bsd-sockets :sb-gray)
  (:export with-client-socket-stream
           with-accepted-socket-stream
           read-byte-count))
(in-package :rtmp.socket)
