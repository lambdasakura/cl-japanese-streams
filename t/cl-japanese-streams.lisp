(in-package :cl-user)
(defpackage cl-japanese-streams-test
  (:use :cl
	:cl-japanese-streams
	:cl-test-more))
(in-package :cl-japanese-streams-test)

(plan 8)

(is #\あ (read-char (make-japanese-input-stream "あ")))

(defparameter *japanese-string* 
"複数行の
日本語の文字も
正しく読めます")

(defparameter *japanese-stream* (make-japanese-input-stream *japanese-string*))

(is #\複 (read-char *japanese-stream*))
(is #\数 (read-char *japanese-stream*))
(is #\行 (read-char *japanese-stream*))
(is #\の (read-char *japanese-stream*))
(is #\Newline (read-char *japanese-stream*))

(is "日本語の文字も" (read-line *japanese-stream*))
(is "正しく読めます" (read-line *japanese-stream*))

(finalize)
