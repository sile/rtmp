(in-package :rtmp.message)

(defun list-map-p (list)
  (and (listp list)
       (every (lambda (x) (and (consp x) (stringp (car x)))) list)))

(deftype list-map () '(satisfies list-map-p))

(defparameter *chunk-stream-id* 3)
(defparameter *message-stream-id* 1)

(defun next-chunk-stream-id ()
  (when (> *chunk-stream-id* 65599)
    (setf *chunk-stream-id* 3))
  (prog1 *chunk-stream-id*
    (incf *chunk-stream-id*)))

(defun next-message-stream-id ()
  (when (> *message-stream-id* #xFFFFFF)
    (setf *message-stream-id* 0))
  (prog1 *message-stream-id*
    (incf *message-stream-id*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +MESSAGE_TYPE_ID_COMMAND_AMF0+ 20)
  (defconstant +MESSAGE_TYPE_ID_COMMAND_AMF3+ 17)

  (defconstant +MESSAGE_TYPE_ID_DATA_AMF0+ 18)
  (defconstant +MESSAGE_TYPE_ID_DATA_AMF3+ 15)

  (defconstant +MESSAGE_TYPE_ID_VIDEO+ 9)
  (defconstant +MESSAGE_TYPE_ID_AUDIO+ 8)
  )


;;;; message
;;(defstruct message-base
;;  (type-id   0 :type (unsigned-byte 8))
;;  (stream-id 0 :type (unsigned-byte 32))
;;  (timestamp 0 :type (unsigned-byte 32)))

;;;; video
(defstruct (video (:include message-base))
  data)

(defun video (data &key (timestamp (get-internal-real-time)) (stream-id (next-message-stream-id)))
  (make-video :data data
              :type-id +MESSAGE_TYPE_ID_VIDEO+
              :timestamp timestamp
              :stream-id stream-id))

(defun parse-video (payload stream-id timestamp)
  (video payload :stream-id stream-id :timestamp timestamp))

(defmethod show ((m video))
  (with-slots (type-id stream-id timestamp data) m
    (format nil "(~s \"~d:~d:~d\" ~d)" 
            "video" type-id stream-id timestamp (length data))))

(defmethod write (out (m video) &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                     (chunk-stream-id +CHUNK_STREAM_ID_PCM+))
  (write-impl (out m :chunk-size chunk-size
                     :chunk-stream-id chunk-stream-id)
    (with-slots (data) m
      (write-bytes data out))))

;;;; audio
(defstruct (audio (:include message-base))
  data)

(defun audio (data &key (timestamp (get-internal-real-time)) (stream-id (next-message-stream-id)))
  (make-audio :data data
              :type-id +MESSAGE_TYPE_ID_AUDIO+
              :timestamp timestamp
              :stream-id stream-id))

(defun parse-audio (payload stream-id timestamp)
  (audio payload :stream-id stream-id :timestamp timestamp))

(defmethod show ((m audio))
  (with-slots (type-id stream-id timestamp data) m
    (format nil "(~s \"~d:~d:~d\" ~d)" 
            "audio" type-id stream-id timestamp (length data))))

(defmethod write (out (m audio) &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                     (chunk-stream-id +CHUNK_STREAM_ID_PCM+))
  (write-impl (out m :chunk-size chunk-size
                     :chunk-stream-id chunk-stream-id)
    (with-slots (data) m
      (write-bytes data out))))

;;;; data
(defstruct (data-base (:include message-base))
  data)

(defun data-base (data &key (timestamp (get-internal-real-time)) (stream-id (next-message-stream-id)))
  (make-data-base :data data
                  :type-id +MESSAGE_TYPE_ID_DATA_AMF0+ ; XXX:
                  :timestamp timestamp
                  :stream-id stream-id))
  
;; TODO: 正確なフォーマットを確認
(defun parse-data (payload stream-id timestamp amf-version)
  (declare (ignore amf-version))

  (with-input-from-bytes (in payload)
    (let ((data (loop WHILE (listen in) COLLECT (rtmp.amf0:decode in))))
      (data-base data 
                 :timestamp timestamp
                 :stream-id stream-id))))

(defmethod show ((m data-base))
  (with-slots (type-id stream-id timestamp data) m
    (let ((*print-pretty* nil))
      (format nil "(~s \"~d:~d:~d\" ~s)"
              "data" type-id stream-id timestamp data))))

(defmethod write (out (m data-base) &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                         (chunk-stream-id +CHUNK_STREAM_ID_PCM+))
  (write-impl (out m :chunk-size chunk-size
                     :chunk-stream-id chunk-stream-id)
    (with-slots (data) m
      (dolist (x data)
        (rtmp.amf0:encode x out)))))

;;;; command
(defconstant +CONNECT_TRANSACIONT_ID+ 1)

(defstruct (command-base (:include message-base))
  (name           t :type string)
  (transaction-id t :type number))

(defstruct (publish (:include command-base))
  (command-object  t :type (member :null))
  (publishing-name t :type string)
  (publishing-type t :type string)) ; "live" or "record" or "append"

(defun publish (transaction-id publishing-name publishing-type &key (timestamp 0)
                                                                    (stream-id (next-message-stream-id))
                                                                    (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-publish :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                :stream-id stream-id
                :timestamp timestamp
                :name "publish"
                :transaction-id transaction-id  ; must be 0
                :command-object :null
                :publishing-name publishing-name
                :publishing-type publishing-type))

(defmethod write-command (out (m publish))
  (with-slots (command-object publishing-name publishing-type) m
    (rtmp.amf0:encode command-object out)
    (rtmp.amf0:encode publishing-name out)
    (rtmp.amf0:encode publishing-type out)))

(defun parse-command-publish (in transaction-id stream-id timestamp)
  (let ((obj (rtmp.amf0:decode in))
        (publishing-name (rtmp.amf0:decode in))
        (publishing-type (rtmp.amf0:decode in)))
    (assert (not (listen in)) () "stream is n't consumed")
    (assert (eq obj :null) () "command-object must be NULL")
    
    (publish transaction-id 
             publishing-name
             publishing-type
             :stream-id stream-id
             :timestamp timestamp)))

(defmethod show-fields ((m publish))
  (with-slots (publishing-name publishing-type) m
    (format nil "name=~s type=~s" publishing-name publishing-type)))

(defstruct (delete-stream (:include command-base))
  (command-object   t :type (member :null))
  (target-stream-id t :type number))

(defun delete-stream (transaction-id target-stream-id &key (timestamp (get-internal-real-time))
                                                           (stream-id (next-message-stream-id))
                                                           (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-delete-stream :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                      :stream-id stream-id
                      :timestamp timestamp
                      :name "deleteStream"
                      :transaction-id transaction-id
                      :command-object :null
                      :target-stream-id target-stream-id))

(defmethod write-command (out (m delete-stream))
  (with-slots (command-object target-stream-id) m
    (rtmp.amf0:encode command-object out)
    (rtmp.amf0:encode target-stream-id out)))

(defun parse-command-delete-stream (in transaction-id stream-id timestamp)
  (let ((obj (rtmp.amf0:decode in))
        (target-stream-id (rtmp.amf0:decode in)))
    (assert (not (listen in)) () "stream is n't consumed")
    (assert (eq obj :null) () "command-object must be NULL")
    
    (delete-stream transaction-id 
                   target-stream-id
                   :stream-id stream-id
                   :timestamp timestamp)))

(defmethod show-fields ((m delete-stream))
  (with-slots (target-stream-id) m
    (format nil "stream-id=~s" target-stream-id)))

(defstruct (create-stream (:include command-base))
  (command-object t :type (or list-map (member :null))))

(defun create-stream (transaction-id command-object &key (timestamp (get-internal-real-time))
                                                         (stream-id 0) ; use default channel
                                                         (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-create-stream :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                      :stream-id stream-id
                      :timestamp timestamp
                      :name "createStream"
                      :transaction-id transaction-id
                      :command-object command-object))

(defmethod write-command (out (m create-stream))
  (with-slots (command-object) m
    (rtmp.amf0:encode command-object out)))

(defun parse-command-create-stream (in transaction-id stream-id timestamp)
  (let ((obj (rtmp.amf0:decode in)))
    (assert (not (listen in)) () "stream is n't consumed")
    (create-stream transaction-id 
                   obj
                   :stream-id stream-id
                   :timestamp timestamp)))

(defmethod show-fields ((m create-stream))
  (with-slots (command-object) m
    (princ-to-string command-object)))

;; TODO: 仕様 or 実装 を探す
(defstruct (on-status (:include command-base))
  field1 
  field2)

(defun on-status (transaction-id &key (field1 :null)
                                      (field2 :null)
                                      (timestamp (get-internal-real-time))
                                      (stream-id (next-message-stream-id))
                                      (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-on-status :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                  :stream-id stream-id
                  :timestamp timestamp
                  :name "onStatus"
                  :transaction-id transaction-id
                  :field1 field1
                  :field2 field2))

(defmethod write-command (out (m on-status))
  (with-slots (field1 field2) m
    (rtmp.amf0:encode field1 out)
    (rtmp.amf0:encode field2 out)))

(defun parse-command-on-status (in transaction-id stream-id timestamp)
  (let ((obj1 (rtmp.amf0:decode in))
        (obj2 (rtmp.amf0:decode in)))
    (assert (not (listen in)) () "stream is n't consumed")
    (on-status transaction-id 
               :stream-id stream-id
               :timestamp timestamp
               :field1 obj1
               :field2 obj2)))

(defmethod show-fields ((m on-status))
  (with-slots (field1 field2) m
    (princ-to-string (list field1 field2))))

;; TODO: 仕様 or 実装 を探す
(defstruct (fcpublish (:include command-base))
  field1 
  field2)

(defun fcpublish (transaction-id &key (field1 :null)
                                      (field2 :null)
                                      (timestamp (get-internal-real-time))
                                      (stream-id (next-message-stream-id))
                                      (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-fcpublish :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                  :stream-id stream-id
                  :timestamp timestamp
                  :name "FCPublish"
                  :transaction-id transaction-id
                  :field1 field1
                  :field2 field2))

(defmethod write-command (out (m fcpublish))
  (with-slots (field1 field2) m
    (rtmp.amf0:encode field1 out)
    (rtmp.amf0:encode field2 out)))

(defun parse-command-fcpublish (in transaction-id stream-id timestamp)
  (let ((obj1 (rtmp.amf0:decode in))
        (obj2 (rtmp.amf0:decode in)))
    (assert (not (listen in)) () "stream is n't consumed")
    (fcpublish transaction-id 
               :stream-id stream-id
               :timestamp timestamp
               :field1 obj1
               :field2 obj2)))

;; TODO: 仕様 or 実装 を探す
(defstruct (FCUnpublish (:include command-base))
  field1 
  field2)

(defun fcunpublish (transaction-id &key (field1 :null)
                                      (field2 :null)
                                      (timestamp (get-internal-real-time))
                                      (stream-id (next-message-stream-id))
                                      (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-fcunpublish :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                  :stream-id stream-id
                  :timestamp timestamp
                  :name "FCUnpublish"
                  :transaction-id transaction-id
                  :field1 field1
                  :field2 field2))

(defmethod write-command (out (m fcunpublish))
  (with-slots (field1 field2) m
    (rtmp.amf0:encode field1 out)
    (rtmp.amf0:encode field2 out)))

(defun parse-command-fcunpublish (in transaction-id stream-id timestamp)
  (let ((obj1 (rtmp.amf0:decode in))
        (obj2 (rtmp.amf0:decode in)))
    (assert (not (listen in)) () "stream is n't consumed")
    (fcunpublish transaction-id 
                 :stream-id stream-id
                 :timestamp timestamp
                 :field1 obj1
                 :field2 obj2)))


;; TODO: 仕様 or 実装 を探す
(defstruct (release-stream (:include command-base))
  field1 ; XXX:
  field2 
  )

(defun release-stream (transaction-id &key (field1 :null)
                                           (field2 "livestream")
                                           (timestamp (get-internal-real-time))
                                           (stream-id (next-message-stream-id))
                                           (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-release-stream :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                       :stream-id stream-id
                       :timestamp timestamp
                       :name "releaseStream"
                       :transaction-id transaction-id
                       :field1 field1
                       :field2 field2))


(defmethod show-fields ((m command-base))
  (declare (ignore m))
  "")

(defmethod show ((m command-base))
  (with-slots (type-id stream-id timestamp name transaction-id) m
    (let ((*print-pretty* nil))
      (format nil "(~s \"~d:~d:~d\" ~d ~a)"
              name type-id stream-id timestamp transaction-id (show-fields m)))))

(defmethod write-command (out (m release-stream))
  (with-slots (field1 field2) m
    (rtmp.amf0:encode field1 out)
    (rtmp.amf0:encode field2 out)))

(defun parse-command-release-stream (in transaction-id stream-id timestamp)
  (let ((obj1 (rtmp.amf0:decode in))
        (obj2 (rtmp.amf0:decode in)))
    (release-stream transaction-id 
                    :stream-id stream-id
                    :timestamp timestamp
                    :field1 obj1
                    :field2 obj2)))

;; TODO: 仕様 or 実装 を探す
(defstruct (on-bandwidth-done (:include command-base))
  field1 ; XXX:
  )

(defun on-bandwidth-done (transaction-id &key (field1 :null)
                                              (timestamp (get-internal-real-time))
                                              (stream-id (next-message-stream-id))
                                              (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-on-bandwidth-done :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                          :stream-id stream-id
                          :timestamp timestamp
                          :name "onBWDone" 
                          :transaction-id transaction-id
                          :field1 field1))

(defmethod write-command (out (m on-bandwidth-done))
  (with-slots (field1) m
    (rtmp.amf0:encode field1 out)))

(defun parse-command-on-bandwidth-done (in transaction-id stream-id timestamp)
  (let ((obj (rtmp.amf0:decode in)))
    (on-bandwidth-done transaction-id 
                       :stream-id stream-id
                       :timestamp timestamp
                       :field1 obj)))

(defstruct (_result (:include command-base))
  (properties  t :type (or list-map (member :null)))
  (information t :type t))

(defun _result (transaction-id properties information 
                &key (timestamp (get-internal-real-time))
                     (stream-id (next-message-stream-id))
                     (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-_result :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                :stream-id stream-id
                :timestamp timestamp
                :name "_result"
                :transaction-id transaction-id
                :properties properties
                :information information))

(defmethod show ((m _result))
  (with-slots (type-id stream-id timestamp name transaction-id properties information) m
    (let ((*print-pretty* nil))
      (format nil "(~s \"~d:~d:~d\" ~d (:PROPS ~s) (:INFO ~s))" 
              name type-id stream-id timestamp 
              transaction-id properties information))))

(defmethod write-command (out (m _result))
  (with-slots (properties information) m
    (rtmp.amf0:encode (if (listp properties) `(:map ,properties) properties) out) ; XXX:
    (rtmp.amf0:encode (if (listp information) `(:map ,information) information) out)))

(defun parse-command-_result (in transaction-id stream-id timestamp)
  (let ((properties (rtmp.amf0:decode in))
        (information (rtmp.amf0:decode in)))

    (_result transaction-id
             (if (listp properties) (second properties) properties)
             (if (listp information) (second information) information) ; XXX:
             :stream-id stream-id
             :timestamp timestamp)))

(defstruct (connect (:include command-base))
  (command-object t :type list-map)
  (optional-args  t :type list-map))

(defun connect (command-object &key (timestamp (get-internal-real-time))
                                    (stream-id 0) ; use default channel (net.connection)
                                    optional-args 
                                    (amf-version 0))
  (declare ((member 0 3) amf-version))
  (assert (= amf-version 0) () "unsupported AMF version: ~a" amf-version)

  (make-connect :type-id +MESSAGE_TYPE_ID_COMMAND_AMF0+
                :stream-id stream-id
                :timestamp timestamp
                :name "connect"
                :transaction-id +CONNECT_TRANSACIONT_ID+
                :command-object command-object
                :optional-args  optional-args))

(defmethod show ((m connect))
  (with-slots (type-id stream-id timestamp name transaction-id command-object optional-args) m
    (let ((*print-pretty* nil))
      (format nil "(~s \"~d:~d:~d\" ~d (:PARAMS ~s) (:OPTS ~s))" 
              name type-id stream-id timestamp 
              transaction-id command-object optional-args))))

(defmacro write-impl ((out message &key chunk-size chunk-stream-id) &body body)
  (let ((m (gensym)))
    `(let ((,m ,message))
       (declare ((integer 2 65599) ,chunk-stream-id))
       (show-log "write message# ~a" (show ,m))

       (let ((payload (with-output-to-bytes (,out)
                        ,@body)))
         (with-slots (type-id timestamp stream-id) ,m
           ;; TODO: chunkはchunk-streamっぽいものを使って、使いまわすようにする (fmt0の数が減る)
           (write-chunks ,out ,chunk-size ,chunk-stream-id type-id stream-id timestamp payload))))))

(defmethod write-command (out (m connect))
  (with-slots (command-object optional-args) m
    (rtmp.amf0:encode `(:map ,command-object) out)
    (when optional-args
      (rtmp.amf0:encode `(:map ,optional-args) out))))
     
(defmethod write (out (m command-base) &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                            (chunk-stream-id (next-chunk-stream-id)))
  (write-impl (out m :chunk-size chunk-size
                     :chunk-stream-id chunk-stream-id)
    (with-slots (name transaction-id) m
      (rtmp.amf0:encode name out) ; TODO: amf3にも対応
      (rtmp.amf0:encode transaction-id out)
      (write-command out m))))

(defun parse-command-connect (in transaction-id stream-id timestamp)
  (declare (ignore transaction-id))
  (let ((command-object (rtmp.amf0:decode in))
        (optional-args  (when (listen in) (rtmp.amf0:decode in))))
    (declare (rtmp.amf0:object-type command-object optional-args))

    (connect (second command-object )
             :stream-id stream-id
             :timestamp timestamp
             :optional-args (second optional-args))))

(defun parse-command (payload stream-id timestamp amf-version)
  (declare (ignore amf-version))

  (with-input-from-bytes (in payload)
    (let* ((command-name   (rtmp.amf0:decode in))
           (transaction-id (rtmp.amf0:decode in))
           (command (intern (string-upcase command-name) :keyword))) ; XXX: gc
      (show-log "command-name# ~s" command-name)
      (ecase command
        (:connect (parse-command-connect in transaction-id stream-id timestamp))
        (:createStream (parse-command-create-stream in transaction-id stream-id timestamp))
        (:deleteStream (parse-command-delete-stream in transaction-id stream-id timestamp))
        (:publish (parse-command-publish in transaction-id stream-id timestamp))
        (:_result (parse-command-_result in transaction-id stream-id timestamp))
        (:onBWDone (parse-command-on-bandwidth-done in transaction-id stream-id timestamp))
        (:releaseStream (parse-command-release-stream in transaction-id stream-id timestamp))
        (:FCPublish (parse-command-fcpublish in transaction-id stream-id timestamp))
        (:FCUnpublish (parse-command-fcunpublish in transaction-id stream-id timestamp))
        (:onStatus (parse-command-on-status in transaction-id stream-id timestamp))
        ))))

;;; user control
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +MESSAGE_TYPE_ID_UCM+ 4)
  
  (defconstant +UCM_EVENT_STREAM_BEGIN+ 0)
  )

(defstruct (user-control-base (:include message-base))
  (event-type 0 :type (unsigned-byte 16)))

(defstruct (stream-begin (:include user-control-base))
  (target-stream-id 0 :type (unsigned-byte 32)))

(defun stream-begin (target-stream-id &key (timestamp (get-internal-real-time)))
  (make-stream-begin :type-id +MESSAGE_TYPE_ID_UCM+
                     :stream-id +MESSAGE_STREAM_ID_PCM+
                     :event-type +UCM_EVENT_STREAM_BEGIN+
                     :timestamp timestamp
                     :target-stream-id target-stream-id))

(defmethod show ((m stream-begin))
  (with-slots (type-id event-type target-stream-id) m
    (format nil "(~s \"~d:~d\" stream-id=~d)" "stream-begin" type-id event-type target-stream-id))) 

(defmethod write (out (m user-control-base) &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                                 (chunk-stream-id +CHUNK_STREAM_ID_PCM+))
  (write-impl (out m :chunk-size chunk-size :chunk-stream-id chunk-stream-id)
    (with-slots (event-type target-stream-id) m
      (write-uint 2 event-type out)
      (write-ucm out m))))

(defmethod write-ucm (out (m stream-begin))
  (write-uint 4 (stream-begin-target-stream-id m) out))

(defun parse-stream-begin (payload timestamp)
  (let ((target-stream-id (read-uint-from-bytes 4 payload :start 2)))
    (stream-begin target-stream-id :timestamp timestamp)))

(defun parse-user-control (payload stream-id timestamp)
  (declare (ignore stream-id))
  (let ((event-type (read-uint-from-bytes 2 payload)))
    (ecase event-type
      (#. +UCM_EVENT_STREAM_BEGIN+ (parse-stream-begin payload timestamp))
      )))

;;; program control
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +MESSAGE_TYPE_ID_SET_CHUNK_SIZE+ 1)
  (defconstant +MESSAGE_TYPE_ID_ACK_WINDOW_SIZE+ 5)
  (defconstant +MESSAGE_TYPE_ID_SET_PEER_BANDWIDTH+ 6)
  )

(defconstant +CHUNK_STREAM_ID_PCM+ 2)
(defconstant +MESSAGE_STREAM_ID_PCM+ 0)

(defstruct (protocol-control-base (:include message-base)))

(defstruct (set-chunk-size (:include protocol-control-base))
  (size 0 :type (unsigned-byte 32)))

(defun set-chunk-size (size &key (timestamp (get-internal-real-time)))
  (make-set-chunk-size :type-id +MESSAGE_TYPE_ID_SET_CHUNK_SIZE+
                       :stream-id +MESSAGE_STREAM_ID_PCM+
                       :timestamp timestamp
                       :size size))

(defmethod write (out (m set-chunk-size)  &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                               (chunk-stream-id +CHUNK_STREAM_ID_PCM+))
  (write-impl (out m :chunk-size chunk-size
                     :chunk-stream-id chunk-stream-id)
    (write-uint 4 (set-chunk-size-size m) out)))

(defun parse-set-chunk-size (payload stream-id timestamp)
  (declare (ignore stream-id))
  (let ((size (read-uint-from-bytes 4 payload)))
    (set-chunk-size size :timestamp timestamp)))

(defmethod show ((m set-chunk-size))
  (with-slots (type-id size) m
    (format nil "(~s \"~d\" size=~d)" "set-chunk-size" type-id size)))
  
(defstruct (ack-win-size (:include protocol-control-base))
  (size 0 :type (unsigned-byte 32)))

(defun ack-win-size (size &key (timestamp (get-internal-real-time)))
  (make-ack-win-size :type-id +MESSAGE_TYPE_ID_ACK_WINDOW_SIZE+
                     :stream-id +MESSAGE_STREAM_ID_PCM+
                     :timestamp timestamp
                     :size size))

(defmethod show ((m ack-win-size))
  (with-slots (type-id size) m
    (format nil "(~s \"~d\" size=~d)" "ack-win-size" type-id size)))

(defmethod write (out (m ack-win-size)  &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                             (chunk-stream-id +CHUNK_STREAM_ID_PCM+))
  (write-impl (out m :chunk-size chunk-size
                     :chunk-stream-id chunk-stream-id)
    (write-uint 4 (ack-win-size-size m) out)))

(defun parse-ack-win-size (payload stream-id timestamp)
  (declare (ignore stream-id))
  (let ((size (read-uint-from-bytes 4 payload)))
    (ack-win-size size :timestamp timestamp)))

;; 応答は ack-win-size で行う (送られてきた set-peer-bandwidth とおりに設定したかどうか)
(defstruct (set-peer-bandwidth (:include protocol-control-base))
  (size       0 :type (unsigned-byte 32))
  (limit-type 0 :type (unsigned-byte 8)))

(defun set-peer-bandwidth (ack-win-size limit-type &key (timestamp (get-internal-real-time)))
  (make-set-peer-bandwidth :type-id +MESSAGE_TYPE_ID_SET_PEER_BANDWIDTH+
                           :stream-id +MESSAGE_STREAM_ID_PCM+
                           :timestamp timestamp
                           :size ack-win-size
                           :limit-type limit-type))

(defmethod show ((m set-peer-bandwidth))
  (with-slots (type-id size limit-type) m
    (format nil "(~s \"~d\" size=~d limit-type=~d)" "set-peer-bandwidth" type-id size limit-type)))

(defmethod write (out (m set-peer-bandwidth) &key (chunk-size +DEFAULT_CHUNK_SIZE+)
                                                  (chunk-stream-id +CHUNK_STREAM_ID_PCM+))
  (write-impl (out m :chunk-size chunk-size
                     :chunk-stream-id chunk-stream-id)
    (with-slots (size limit-type) m
      (write-uint 4 size out)
      (write-uint 1 limit-type out))))

(defun parse-set-peer-bandwidth (payload stream-id timestamp)
  (declare (ignore stream-id))
  (let ((ack-win-size (read-uint-from-bytes 4 payload))
        (limit-type   (read-uint-from-bytes 1 payload :start 4)))
    (set-peer-bandwidth ack-win-size limit-type :timestamp timestamp)))

;;; other
(defun read (io state)
  (with-log-section ("read-message")
    (destructuring-bind (message-base payload) (read-message-chunks io state)
      (with-slots (type-id timestamp stream-id) message-base
        (let ((msg 
                (ecase type-id
                  (#. +MESSAGE_TYPE_ID_SET_CHUNK_SIZE+ (parse-set-chunk-size payload stream-id timestamp))
                  (#. +MESSAGE_TYPE_ID_ACK_WINDOW_SIZE+ (parse-ack-win-size payload stream-id timestamp))
                  (#. +MESSAGE_TYPE_ID_SET_PEER_BANDWIDTH+ (parse-set-peer-bandwidth payload stream-id timestamp))
                  (#. +MESSAGE_TYPE_ID_UCM+ (parse-user-control payload stream-id timestamp))
                  (#. +MESSAGE_TYPE_ID_COMMAND_AMF0+ (parse-command payload stream-id timestamp 0))
                  (#. +MESSAGE_TYPE_ID_COMMAND_AMF3+ (error "unsupported message-type: ~a" type-id))
                  (#. +MESSAGE_TYPE_ID_DATA_AMF0+ (parse-data payload stream-id timestamp 0))
                  (#. +MESSAGE_TYPE_ID_DATA_AMF3+ (error "unsupported message-type: ~a" type-id))
                  (#. +MESSAGE_TYPE_ID_VIDEO+ (parse-video payload stream-id timestamp))
                  (#. +MESSAGE_TYPE_ID_AUDIO+ (parse-audio payload stream-id timestamp))
                  )))
          (show-log "message: ~a" (show msg))
          msg)))))
