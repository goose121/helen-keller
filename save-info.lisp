(in-package :helen-keller)

(defvar *saved-vars* nil)

(defmacro define-saved-var (var &optional val doc)
  `(prog1
       (defvar ,var ,val ,doc)
     (push ',var *saved-vars*)))

(defun encode-vote-data (stream)
  (cpk:encode
   (loop :for var :in *saved-vars*
         :collect (cons var (symbol-value var)))
   :stream stream))

(defun decode-vote-data (stream)
  (cpk:decode-stream stream))

(defparameter *vote-file*
  (asdf:system-relative-pathname
   :helen-keller
   "votes.cpk"))

(defun save-vote-data (&optional (file *vote-file*))
  (with-open-file (stream file
                          :direction :output
                          :if-exists :rename
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (encode-vote-data stream))
  nil)

(defun restore-vote-data (&optional (file *vote-file*))
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (loop :for (var . val) :in (decode-vote-data stream)
          :do (setf (symbol-value var) val))))

(defun setup-exit-hook (&optional (file *vote-file*))
  "Sets an exit hook which saves the bot's data to FILE."
  (exit-hooks:add-exit-hook (lambda () (save-vote-data file))))
