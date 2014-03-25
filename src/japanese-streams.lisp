(in-package :cl-user)
(defpackage japanese-streams
  (:use :cl
	:jp
	:trivial-gray-streams)
  (:export :japanese-stream-buffer
	   :japanese-stream-position))
(in-package :japanese-streams)
(cl-syntax:use-syntax :annot)

(deftype octet () '(unsigned-byte 8))

(defun vector->char (vec &key (external-format :utf-8))
  (ignore-errors (jp:decode vec external-format)))

(defun string->usb8-stream (string &key (external-format :utf-8))
  "Generate octets stream form string"
  (let ((octets (jp:encode string external-format)))
    (flexi-streams:make-flexi-stream 
     (flexi-streams:make-in-memory-input-stream octets))))

(defun read-japanese-char (stream &key (external-format :utf-8))
  (let ((temp (make-array 1 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil :eof)
       do
	 (if (eq byte :eof) (return nil))
	 (vector-push-extend byte temp)
	 (let ((char (vector->char temp :external-format external-format)))
	   (if char (return (elt char 0)))))))

(defun usb8-stream->japanese-stream-buffer (usb8-stream &key (external-format :utf-8))
  (let ((array (make-array 0
			   :adjustable t
			   :fill-pointer 0)))
    (loop for char = (read-japanese-char usb8-stream :external-format external-format)
	 until (eq char nil)
	 do
	 (vector-push-extend char array))
    array))

(defun make-japanese-stream-buffer (strings &key (external-format :utf-8))
  (let ((jp-st (string->usb8-stream strings :external-format external-format)))
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
(defun make-japanese-input-stream (&optional (strings nil) &key (external-format :utf-8) buffer)
  (make-instance 'japanese-input-stream 
		 :buffer (if (null buffer) (make-japanese-stream-buffer strings :external-format external-format) 
			     buffer)))

@export
(defmacro with-open-multibyte-file ((stream filespec &rest options &key (external-format :utf-8)) &body body)
  `(with-open-stream (,stream (open ,filespec :element-type 'octet ,@options))
     (setq ,stream (make-japanese-input-stream nil :buffer (usb8-stream->japanese-stream-buffer ,stream :external-format ,external-format)
				 :external-format ,external-format))
     ,@body))
