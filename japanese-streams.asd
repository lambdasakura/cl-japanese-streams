(in-package :cl-user)
(defpackage japanese-streams-asd
  (:use :cl :asdf))
(in-package :japanese-streams-asd)

(defsystem japanese-streams
  :version "0.1"
  :author "lambda_sakura"
  :license "MIT"
  :depends-on (:cl-annot
               :cl-syntax-annot
	       :jp
	       :flexi-streams
               :trivial-gray-streams)
  :components ((:module "src"
                :components
                ((:file "japanese-streams"))))
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
  :in-order-to ((test-op (load-op circular-streams-test))))
