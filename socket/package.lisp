(defpackage rtmp.socket
  (:use :common-lisp :sb-bsd-sockets)
  (:export with-client-socket-stream))
(in-package :rtmp.socket)
