(in-package :cl-user)
(defpackage japanese-streams
  (:use :cl
	:babel
	:trivial-gray-streams)
  (:export :japanese-stream-buffer
	   :japanese-stream-position))
(in-package :japanese-streams)
(cl-syntax:use-syntax :annot)
(deftype octets () '(unsigned-byte 8))

@export
(defun vector->char (vec &key (encoding :utf-8))
  (let* ((lst (loop for i across vec collect i))
	 (array (make-array (length lst) :element-type 'octets :initial-contents lst)))
  (ignore-errors (babel:octets-to-string array :encoding encoding))))

@export
(defun string->usb8-stream (string &key (encoding :utf-8))
  "Generate octets stream form string"
  (let ((octets (babel:string-to-octets string :encoding encoding)))
    (flexi-streams:make-flexi-stream 
     (flexi-streams:make-in-memory-input-stream octets))))

@export
(defun read-japanese-char (stream &key (encoding :utf-8))
  (let ((temp (make-array 1 :element-type 'octets :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil :eof)
       do
	 (if (eq byte :eof) (return nil))
	 (vector-push-extend byte temp)
	 (let ((char (vector->char temp :encoding encoding)))
	   (if char (return (elt char 0)))))))

(defun usb8-stream->japanese-stream-buffer (usb8-stream &key (encoding :utf-8))
  (let ((array (make-array 0
			   :adjustable t
			   :fill-pointer 0)))
    (loop for char = (read-japanese-char usb8-stream :encoding encoding)
	 until (eq char nil)
	 do
	 (vector-push-extend char array))
    array))

@export
(defun make-japanese-stream-buffer (strings &key (encoding :utf-8))
  (let ((jp-st (string->usb8-stream strings :encoding encoding)))
    (usb8-stream->japanese-stream-buffer jp-st)))
	
@export
(defclass japanese-input-stream (fundamental-character-input-stream)
  ((stream :initarg :stream
	   :initform nil
	   :reader japanese-stream-stream)
   (buffer :initarg :buffer 
	   :initform (make-japanese-stream-buffer)
	   :reader japanese-stream-buffer)
   (position :initform 0
	     :accessor japanese-stream-position)))


(defmethod stream-read-char ((this japanese-input-stream))
  (with-slots (buffer position) this
    (if (< position (length buffer))
	(prog1 
	    (aref buffer position)
	    (incf position))
	(prog1 
	    :eof))))

	  
;; STREAM-UNREAD-CHAR
;; STREAM-READ-CHAR-NO-HANG  stream
;; STREAM-PEEK-CHAR  stream
;; STREAM-LISTEN  stream
;; STREAM-READ-LINE  stream
;; STREAM-CLEAR-INPUT  stream

@export
(defun make-japanese-input-stream (&optional (strings nil) &key (encoding :utf-8) buffer)
  (make-instance 'japanese-input-stream 
		 :buffer (if (null buffer) (make-japanese-stream-buffer strings :encoding encoding) 
			     buffer)))

@export
(defmacro with-open-multibyte-file ((stream filespec &rest options &key (encoding :utf-8)) &body body)
  `(with-open-stream (,stream (open ,filespec :element-type 'octet ,@options))
     (setq ,stream (make-japanese-input-stream nil :buffer (usb8-stream->japanese-stream-buffer ,stream :encoding ,encoding)
				 :encoding ,encoding))
     ,@body))
