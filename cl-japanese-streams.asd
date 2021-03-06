(in-package :cl-user)
(defpackage cl-japanese-streams-asd
  (:use :cl :asdf))
(in-package :cl-japanese-streams-asd)

(defsystem cl-japanese-streams
  :version "0.1"
  :author "lambda_sakura"
  :license "MIT"
  :depends-on (:cl-annot
               :cl-syntax-annot
	       :babel
	       :flexi-streams
               :trivial-gray-streams)
  :components ((:module "src"
                :components
                ((:file "cl-japanese-streams"))))
  :description "Japanese strings streams for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op japanese-streams-test))))
