(in-package :cl-user)
(defpackage cl-japanese-streams-test-asd
  (:use :cl :asdf))
(in-package :cl-japanese-streams-test-asd)

(defsystem cl-japanese-streams-test 
  :author "lambda_sakura"
  :license "MIT"
  :depends-on (:cl-japanese-streams
	       :cl-test-more)
  :components ((:module "t"
		:components
		((:file "cl-japanese-streams"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
			
