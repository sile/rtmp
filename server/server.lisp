(in-package :rtmp.server)

(defun start (io)
  (handshake io)
  t)
