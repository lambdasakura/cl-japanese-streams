(in-package :cl-user)
(defpackage cl-japanese-streams-test
  (:use :cl
	:cl-japanese-streams
	:cl-test-more))
(in-package :cl-japanese-streams-test)

(plan 14)

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

(with-open-japanese-file (s "japanese-test-file.txt" :encoding :utf-8)
  (is "日本語" (read-line s))
  (is "複数行" (read-line s)))

(with-open-japanese-file (s "japanese-test-file-euc-jp.txt" :encoding :EUCJP)
  (is "日本語" (read-line s))
  (is "複数行" (read-line s)))

(with-open-japanese-file (s "japanese-test-file-cp932.txt" :encoding :CP932)
   (is "日本語" (read-line s))
   (is "複数行" (read-line s)))

(finalize)
