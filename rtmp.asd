(in-package :asdf)

(defsystem rtmp
  :name "rtmp"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "TODO:"

  :serial t
  :depends-on (:creole)
  :components ((:file "package")

			   (:file "utils/package")
			   (:file "utils/utils")

			   (:file "amf0/package")
			   (:file "amf0/types")
			   (:file "amf0/parser")
			   (:file "amf0/amf0")

			   (:file "rtmp")))
