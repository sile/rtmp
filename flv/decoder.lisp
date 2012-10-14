(in-package :rtmp.flv)

(defstruct header
  (signature   t :type string) ; must be "FLV" 
  (version     t :type (unsigned-byte 8))
  (reserved1   t :type (unsigned-byte 5)) ; shall be 0
  (audio?      t :type (unsigned-byte 1)) ; 1=true
  (reserved2   t :type (unsigned-byte 1)) ; shall be 0
  (video?      t :type (unsigned-byte 1)) ; 1=true
  (data-offset t :type (unsigned-byte 32))) ; The length of this header in bytes

(defstruct audio-tag-header
  (sound-format t :type (unsigned-byte 4))
  (sound-rate   t :type (unsigned-byte 2))
  (sound-size   t :type (unsigned-byte 1))
  (sound-type   t :type (unsigned-byte 1))
  (aac-packet-type t :type (or null (unsigned-byte 8)))) ; if sound-format == 10

(defstruct video-tag-header
  (frame-type t :type (unsigned-byte 4))
  (codec-id   t :type (unsigned-byte 4))
  (avc-packet-type t :type (or null (unsigned-byte 8))) ; if codec-id == 7
  (composition-time t :type (or null (signed-byte 24))) ; if codec-id == 7
  )

(defstruct encryption-tag-header) ; TODO: implement
(defstruct filter-params)         ; TODO: implement

(defstruct encrypted-body
  body) ; TODO: implement

(defstruct audio-tag-body
  body) ; TODO: 

(defstruct video-tag-body
  body) ; TODO: codec-idごとのデータ

(defstruct script-tag-body
  body)

(defstruct audio-data
  (body t :type (or audio-tag-body encrypted-body)))

(defstruct video-data
  (body t :type (or video-tag-body encrypted-body)))

(defstruct script-data
  (body t :type (or script-tag-body encrypted-body)))

(defstruct tag
  (reserved1 t :type (unsigned-byte 2)) ; Reserved for FMS, should be 0
  (filter    t :type (unsigned-byte 1))
  (tag-type  t :type (unsigned-byte 5))  ; 8=audio, 9=video, 18=script-data
  (data-size t :type (unsigned-byte 24))
  (timestamp t :type (unsigned-byte 24))
  (timestamp-extended t :type (unsigned-byte 8))
  (stream-id t :type (unsigned-byte 3)) ; Always 0
  (audio-tag-header t :type (or null audio-tag-header))  ; if tag-type == 8
  (video-tag-header t :type (or null video-tag-header))  ; if tag-type == 9
  (encryption-tag-header t :type (or null encryption-tag-header)) ; if filter == 1
  (filter-params t :type (or null filter-params)) ; if filter == 1
  (data t :type (or audio-data video-data script-data)))

(defun decode-header (in)
  (let ((signature (creole:octets-to-string (read-bytes 3 in)))
        (version (read-uint 1 in))
        (flags (read-uint 1 in))
        (data-offset (read-uint 4 in)))
    (make-header :signature signature
                 :version version
                 :reserved1 (ldb (byte 5 3) flags)
                 :audio?    (ldb (byte 1 2) flags)
                 :reserved2 (ldb (byte 1 1) flags)
                 :video?    (ldb (byte 1 0) flags)
                 :data-offset data-offset)))
  
(defun decode-audio-tag-header (in)
  (let* ((b1 (read-uint 1 in))
         (sound-format (ldb (byte 4 4) b1))
         (sound-rate   (ldb (byte 2 2) b1))
         (sound-size   (ldb (byte 1 1) b1))
         (sound-type   (ldb (byte 1 0) b1))
         (aac-packet-type (when (= sound-format 10)
                            (read-uint 1 in))))
    (make-audio-tag-header
     :sound-format sound-format
     :sound-rate sound-rate
     :sound-size sound-size
     :sound-type sound-type
     :aac-packet-type aac-packet-type)))

(defun decode-audio-data (header in)
  (declare (ignore header in))
  (make-audio-data :body (make-audio-tag-body :body :todo)))

(defun decode-video-tag-header (in)
  (let* ((b1 (read-uint 1 in))
         (frame-type (ldb (byte 4 4) b1))
         (codec-id   (ldb (byte 4 0) b1))
         (avc-packet-type (when (= codec-id 7)
                            (read-uint 1 in)))
         (composition-time (when (= codec-id 7)
                             (read-int 3 in))))
    (make-video-tag-header 
     :frame-type frame-type
     :codec-id codec-id
     :avc-packet-type avc-packet-type
     :composition-time composition-time)))

(defun decode-video-data (header in)
  (declare (ignore header in))
  (make-video-data :body (make-video-tag-body :body :todo)))

(defun decode-script-tag-body (in)
  (make-script-tag-body 
   :body
   (loop FOR rlt = (ignore-errors (rtmp.amf0:decode in))
         WHILE rlt
         COLLECT rlt)))

(defun decode-script-data (in)
  ;; TODO: if encrypted
  (make-script-data :body (decode-script-tag-body in))
  )

(defun decode-tag (in)
  (let* ((b1 (read-uint 1 in))
         (reserved1 (ldb (byte 2 6) b1))
         (filter    (ldb (byte 1 5) b1))
         (tag-type  (ldb (byte 5 0) b1))
         (data-size (read-uint 3 in))
         (timestamp (read-uint 3 in))
         (timestamp-ext (read-uint 1 in))
         (stream-id (read-uint 3 in))
         
         (rest-bytes (read-bytes data-size in))

         (audio-tag-header nil)
         (video-tag-header nil)
         )
    
    (with-input-from-bytes (in2 rest-bytes)
      (ecase tag-type
        (8 (setf audio-tag-header (decode-audio-tag-header in2)))
        (9 (setf video-tag-header (decode-video-tag-header in2)))
        (18 :ignore))
          
      (make-tag :reserved1 reserved1
                :filter filter
                :tag-type tag-type
                :data-size data-size
                :timestamp timestamp
                :timestamp-extended timestamp-ext
                :stream-id stream-id
                
                ;; TODO:
                :audio-tag-header audio-tag-header
                :video-tag-header video-tag-header
                :encryption-tag-header nil
                :filter-params nil
                :data (ecase tag-type
                        (8 (decode-audio-data audio-tag-header in2))
                        (9 (decode-video-data video-tag-header in2))
                        (18 (decode-script-data in2)))))))

(defun decode-body (in)
  (loop WHILE (listen in)
        FOR (prev-tag-size tag) = (ignore-errors ; XXX:
                                    (list (read-uint 4 in)
                                          (decode-tag in)))
        WHEN (and prev-tag-size tag)
        COLLECT (list prev-tag-size tag)))
  
(defun decode-impl (in)
  (let ((header (decode-header in))
        (body   (decode-body in)))
    (list header body)))
