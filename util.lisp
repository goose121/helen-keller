(in-package :helen-keller)

(defun create-msg (msg chan-id)
  "Creates a message in the channel with the given ID, without
fetching that channel's information via REST."
  (create msg (make-instance 'lc:channel :id chan-id)))
