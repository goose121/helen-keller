
(defsystem "helen-keller"
  :description "A voting bot written in Common Lisp."
  :version "0.0.1"
  :author "goose121 (https://github.com/goose121)"
  :license "GPLv3-or-later"
  :depends-on (#:lispcord
               #:anaphora
               #:alexandria
               #:serapeum
               #:exit-hooks
               #:cl-ppcre
               #:cl-conspack
               #:fast-io)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "save-info" :depends-on ("package"))
               (:file "error-handling" :depends-on ("package"))
               (:file "perms" :depends-on ("package"
                                           "error-handling"))
               (:file "vote" :depends-on ("package"
                                          "util"
                                          "perms"
                                          "save-info"
                                          "error-handling"))
               (:file "vote-bot" :depends-on ("package"
                                              "error-handling"
                                              "vote"))))
