(in-package :asdf)

(defsystem rtmp
  :name "rtmp"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "TODO:"

  :serial t
  :depends-on (:creole :flexi-streams :sb-bsd-sockets)
  :components ((:file "package")

               (:file "const/package")
               (:file "const/rtmp")

               (:file "utils/package")
               (:file "utils/utils")
               (:file "utils/handshake")
               
               (:file "amf0/package")
               (:file "amf0/marker")
               (:file "amf0/decoder")
               (:file "amf0/encoder")
               (:file "amf0/amf0")

               (:file "socket/package")
               (:file "socket/socket")

               (:file "message/package")
               (:file "message/chunk")
               (:file "message/message")

               (:file "client/package")
               (:file "client/handshake")
               (:file "client/client")
               
               (:file "server/package")
               (:file "server/server")

               (:file "rtmp")))
