(in-package :helen-keller)

(define-condition user-error (error) ((message :initarg :message))
  (:documentation "Superclass for errors which should be shown to the
  user in Discord chat. Subclassed to error to prevent forgetting to
  handle and display it."))

(defmethod print-object ((obj user-error) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (write (slot-value obj 'message) :stream stream)))
