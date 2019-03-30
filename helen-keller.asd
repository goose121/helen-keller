
(defsystem "helen-keller"
  :description "A voting bot written in Common Lisp."
  :version "0.0.1"
  :author "goose121 (https://github.com/goose121)"
  :license "GPLv3-or-later"
  :depends-on (#:lispcord
			   #:anaphora)
  :components ((:file "package")
			   (:file "error-handling" :depends-on ("package"))
			   (:file "vote-bot" :depends-on ("package"
								              "error-handling"))))
