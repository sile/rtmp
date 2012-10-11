(in-package :rtmp.utils)

(defun write-handshake-0 (out version)
  (declare (octet version))
  (write-uint 1 version out))

(defun write-handshake-1 (out timestamp zero random-bytes)
  (declare ((unsigned-byte 32) timestamp zero)
           ((octets 1528) random-bytes))
  (write-uint 4 timestamp out)
  (write-uint 4 zero out)
  (write-bytes random-bytes out))

(defun write-handshake-2 (out send-timestamp recv-timestamp recv-random-bytes)
  (declare ((unsigned-byte 32) send-timestamp recv-timestamp)
           ((octets 1528) recv-random-bytes))
  (write-uint 4 send-timestamp out)
  (write-uint 4 recv-timestamp out)
  (write-bytes recv-random-bytes out))

(defun read-handshake-0 (in)
  (read-uint 1 in))

(defun read-handshake-1 (in)
  (let ((timestamp (read-uint 4 in))
        (zero      (read-uint 4 in))
        (random-bytes (read-bytes 1528 in)))
    (values timestamp zero random-bytes)))

(defun read-handshake-2 (in)
  (let ((my-timestamp (read-uint 4 in))
        (other-timestamp (read-uint 4 in))
        (my-random-bytes (read-bytes 1528 in)))
    (values my-timestamp other-timestamp my-random-bytes)))
