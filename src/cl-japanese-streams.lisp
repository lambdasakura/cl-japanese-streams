(in-package :cl-user)
(defpackage cl-japanese-streams
  (:use :cl
	:babel
	:trivial-gray-streams)
  (:export :japanese-stream-buffer
	   :japanese-stream-position))
(in-package :cl-japanese-streams)
(cl-syntax:use-syntax :annot)

(deftype octets () '(unsigned-byte 8))

@export
(defun vector->char (vec &key (encoding :utf-8))
  "convert unsinged-byte vector to japanese character."
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
  "read one japanese character from usb8 vector"
  (let ((temp (make-array 1 :element-type 'octets :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil :eof)
       do
	 (if (eq byte :eof) (return nil))
	 (vector-push-extend byte temp)
	 (let ((char (vector->char temp :encoding encoding)))
	   (if char (return (elt char 0)))))))

@export
(defclass japanese-input-stream (fundamental-character-input-stream)
  ((stream :initarg :stream
	   :initform nil
	   :reader japanese-stream-stream)
   (buffer :initarg :buffer 
	   :initform (make-japanese-stream-buffer)
	   :reader japanese-stream-buffer)
   (position :initform 0
	     :accessor japanese-stream-position)
   (encoding :initform :UTF-8
	     :initarg :encoding
	     :accessor encoding)))

@export
(defun make-japanese-stream-buffer ()
  "create Japanese character stream from Japanese string"
  (make-array 0 :adjustable t :fill-pointer 0))

@export
(defun make-japanese-stream-stream (string &key (encoding :utf-8))
  (string->usb8-stream string :encoding encoding))


(defmethod stream-read-char ((this japanese-input-stream))
  (with-slots (stream buffer position encoding) this
    (let ((char (read-japanese-char stream :encoding encoding)))
      (if (eq char nil) :eof
	  (prog2 
	      (vector-push-extend char buffer)
	      (aref buffer position)
	    (incf position))))))

@export
(defun make-japanese-input-stream (&optional (string nil) &key (encoding :utf-8) stream)
  (make-instance 'japanese-input-stream 
		 :encoding encoding
		 :stream (if stream stream (make-japanese-stream-stream string :encoding encoding))
		 :buffer (make-japanese-stream-buffer)))

@export
(defmacro with-open-japanese-file ((stream filespec &key (encoding :utf-8)) &body body)
  `(with-open-stream (,stream (open ,filespec :element-type 'octets ))
     (let ((,stream (make-japanese-input-stream
		    nil 
		    :stream ,stream
		    :encoding ,encoding)))
     ,@body)))
