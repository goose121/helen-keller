
(in-package :helen-keller)

(defclass guild-perms ()
  ((cast-ballot-criteria :initform nil
                         :initarg :cast-criteria
                         :accessor cast-ballot-criteria)
   (make-vote-criteria :initform nil
                       :initarg :make-criteria
                       :accessor make-vote-criteria)))

(cpk:defencoding guild-perms
  cast-ballot-criteria make-vote-criteria)

(define-saved-var *votes* (make-hash-table)
  "Maps vote IDs to vote objects.")
(define-saved-var *vote-channels* (make-hash-table)
  "Maps guild IDs to the IDs of their corresponding vote
  channels.")
(define-saved-var *guild-perms* (make-hash-table)
  "Maps guild IDs to the permissions of the guilds.")

(defun vote-channel (guild-id)
  (gethash guild-id *vote-channels*))

(defun (setf vote-channel) (channel guild-id)
  (setf (gethash guild-id *vote-channels*) channel))

(defclass bare-message ()
  ((id :initarg :id :accessor lc:id)
   (channel-id :initarg :channel-id :accessor channel-id))
  (:documentation "The bare minimum required to represent a message;
  that is, its ID and its channel ID"))

(cpk:defencoding bare-message
  id channel-id)

(defun strip-message (msg)
  "Converts MSG to a BARE-MESSAGE."
  (make-instance 'bare-message
                 :id (lc:id msg)
                 :channel-id (lc:channel-id msg)))

;; Go from a bare message to a full message
(defmethod from-id ((msg bare-message) (c (eql :message))
                    &optional (bot *client*))
  (from-id (lc:id msg) (from-id (channel-id msg) :channel bot) bot))

(defclass vote ()
  ((guild-id :initarg :guild-id :reader guild-id)
   (topic :initarg :topic :reader topic
          :documentation "The topic of the vote; what is actually
          being voted for or against?")
   (description :initarg :description :accessor description
                :documentation "A short description of the vote's
                topic")
   (choices :initarg :choices :accessor choices
            :documentation "The list of possible choices to vote for")
   (announcement :initarg :announcement :reader announcement
                 :documentation "The message which announces/describes
                 this vote")
   ;; Note: the separation of ballot IDs from ballot objects is
   ;; because a) it's useful to create a new ballot for a user which
   ;; then has a new ID and b) it's much harder for an accidental
   ;; glance at the representation of the vote to instantly reveal
   ;; compromising information due to the indirection
   (ballot-objects :initform (make-hash-table) :accessor ballot-objects
                   :documentation "Mapping from ballot IDs to ballot
                   objects")
   (ballot-ids :initform (make-hash-table) :accessor ballot-ids
               :documentation "Mapping from user IDs to their current
               ballot ID"))
  (:documentation "This base class provides a simple vote where people
  can select a ranked list from a set of alternatives."))

(cpk:defencoding vote
  guild-id topic description choices announcement ballot-objects ballot-ids)

(defun find-member (username discrim guild)
  "Finds a member with the given username and discriminator in GUILD."
  (find-if (lambda (m)
             (and (string= username (lc:name (lc:user m)))
                  (string= discrim (lc:discrim (lc:user m)))))
           (lc:members guild)))

(defun maybe-parse-integer (string &key (start 0) end (radix 10))
  "Attempts to parse a base-10 integer from STRING; returns nil if
STRING contains anything besides an integer."
  (loop :with total = 0
        :for char :across (nsubseq string start end)
        :for char-value = (or (digit-char-p char radix)
                              (return nil))
        :do (setf total (+ (* total radix) char-value)) 
        :finally (return total)))

(defun parse-ballot-choice (choice vote-guild)
  "Parses a single choice from a ballot message. If the choice ends
with #NNNN, where all of NNNN are digit-chars, it will try to find the
specified user in VOTE-GUILD and if found the user-id of that user
will be returned. If it's a ping, then the function will check that
the user mentioned is in VOTE-GUILD, and if so will return the user's
ID. In all other cases, the choice will be returned as a string."
  (ppcre:register-groups-bind
      (username discrim (#'maybe-parse-integer ping-id))
      ("^(?:([^#]{2,32})#(\\d{4})|<@(\\d+)>|.*)$"
       (trim-whitespace choice)
       :sharedp t)
    (format t "~A~%" ping-id)
    (or
      (and username discrim
           (lc:id (lc:user (find-member username discrim vote-guild))))
      (when (and ping-id
                 (find ping-id (lc:members vote-guild)
                       :key (alexandria:compose #'lc:id #'lc:user)))
        ping-id)
      (trim-whitespace choice))))

(defun princ-ballot-choice (choice)
  "Prints the ballot choice CHOICE for display in a Discord message."
  (etypecase choice
    (lispcord.util:snowflake (format nil "<@~A>" choice))
    (string choice)))

(defun make-id (map)
  "Creates a random ID which is not present in MAP."
  (loop
    :for random-id = (random (expt 36 20))
    :when (multiple-value-bind (v present-p)
              (gethash random-id map)
            (declare (ignore v))
            (not present-p))
      :do (return random-id)))


(defun parse-vote-msg (msg)
  (destructuring-bind (cmd-name topic description choices)
      (ppcre:split "\\n\\n" (lc:content msg))
    (unless choices
      (error
       'user-error
       :message
       (format nil
               "Not enough fields were found in the ~
                message. Usage: ```~@
                ~A~@
                ~@
                TITLE~@
                ~@
                DESCRIPTION~@
                ~@
                CHOICE1~@
                CHOICE2~@
                ...```"
               cmd-name)))
    (make-instance 'vote
                   :guild-id (lc:id (lc:guild msg))
                   :topic topic
                   :description description
                   :choices (mapcar
                             (lambda (c)
                               (parse-ballot-choice c (lc:guild msg)))
                             (ppcre:split "\\n" choices)))))

(defparameter *id-format* (formatter "~36,20,0R"))

(defun announce-vote (vote vote-id
                      &optional (channel
                                 (vote-channel (guild-id vote))))
  "Sends a message in CHANNEL (default is the vote's guild's specified
vote channel) announcing the vote and its possible choices."
  (setf
   ;; This is set here because it makes for more elegant control flow,
   ;; but the slot is defined without an accessor because it isn't
   ;; normally meant to change.
   (slot-value vote 'announcement)
   (strip-message
    (create-msg
     (lc:make-message
      ""
      :embed
      (lc:make-embed
       :title (format nil "Vote: ~A" (topic vote))
       :description (description vote)
       :fields (vector
                (make-instance
                 'lc:embed-field
                 :name "Vote ID"
                 :value (format nil *id-format* vote-id)
                 :inline :false)
                (make-instance
                 'lc:embed-field
                 :name "Choices"
                 :value (format nil "~{~A~^~%~}"
                                (mapcar #'princ-ballot-choice
                                        (choices vote)))
                 :inline :false))
       :type "rich"))
     channel)))
  vote)

(defun publicize-vote (vote creator)
  "Puts VOTE in *VOTES* with a random ID and announces it in the
appropriate channel if the creator has permission to create a vote."
  (unless (every (lambda (c) (satisfies-p creator c))
                 (make-vote-criteria
                  (gethash (guild-id vote) *guild-perms*)))
    (error 'user-error
           :message (format nil "You do not have permission to ~
                                 create votes in this server.")))
  (let ((vote-id (make-id *votes*)))
    (setf (gethash vote-id *votes*) vote)
    (announce-vote vote vote-id)))

(defun process-vote-msg (msg)
  (publicize-vote (parse-vote-msg msg) (lc:author msg)))

(defclass ballot ()
  ((message :initarg :message :reader message
            :documentation "The message which originally contained the
            user's choices for the vote")
   (choices :initarg :choices :reader choices
            :documentation "The choices voted for on this ballot, in
            the order that they appeared in the original message")
   (announcement :initarg :announcement :reader announcement
                 :documentation "The message containing the
                 announcement of this ballot"))
  (:documentation "Represents a ballot cast in a vote."))

(cpk:defencoding ballot
  message choices announcement)

(defun parse-ballot-msg (contents)
  "Parses the contents of a message containing a ballot, and returns
three values: the choices specified in the ballot, the vote
corresponding to the vote ID in the ballot, and the vote ID itself."
  (let* ((lines
           (remove 0
                   (ppcre:split "\\n" (trim-whitespace contents))
                   :key #'length))
         (vote-id
           (let ((id-line (string-right-trim whitespace (pop lines))))
             (or (maybe-parse-integer id-line :radix 36)
                 (error 'user-error
                        :message
                        (format nil
                                "`~A` is not a valid vote ID."
                                id-line)))))
         (vote
           (or (gethash vote-id *votes*)
               (error 'user-error
                      :message
                      (format nil
                              "Could not find vote with ID ~A."
                              vote-id))))
         bad-lines)
    (multiple-value-prog1
        (values
         (loop :for line :in lines
               :for choice = (parse-ballot-choice
                              (trim-whitespace line)
                              (from-id (guild-id vote) :guild))
               :unless (and choice (find choice (choices vote)
                                         :test #'equal))
                 :do (push line bad-lines)
               :collect choice)
         vote
         vote-id)
     (when bad-lines
       (error 'user-error
              :message
              (apply
               #'format nil
               "~#[ EMPTY LIST~; ~S isn't an available choice ~
               ~; ~S and ~S aren't available choices ~:;
               ~@{~#[~; and~] ~S~^ ,~} aren't available choices ~]~
               for this vote. ~
               Please check the description of the vote ~
               and try again." 
               bad-lines))))))

(defun announce-ballot (ballot ballot-id vote vote-id
                        &optional (channel
                                   (vote-channel (guild-id vote))))
  "Sends a message in CHANNEL (default is the vote's guild's specified
vote channel) announcing the ballot, its corresponding vote's ID, and
its specified choices."
  (setf
   (slot-value ballot 'announcement)
   (strip-message
    (create-msg
     (lc:make-message
      ""
      :embed
      (lc:make-embed
       :title (format nil "Ballot cast for vote \"~A\"" (topic vote))
       :description (description vote)
       :fields (vector
                (make-instance
                 'lc:embed-field
                 :name "Vote ID"
                 :value (format nil *id-format* vote-id)
                 :inline :false)
                (make-instance
                 'lc:embed-field
                 :name "Ballot ID"
                 :value (format nil *id-format* ballot-id)
                 :inilne :false)
                (make-instance
                 'lc:embed-field
                 :name "Choices selected"
                 :value (format nil "~{~A~^~%~}"
                                (mapcar #'princ-ballot-choice
                                        (choices ballot)))
                 :inline :false))
       :type "rich"))
     channel))))

(defun publicize-ballot (ballot vote vote-id user)
  (unless (every (lambda (c) (satisfies-p user c))
                 (cast-ballot-criteria
                  (gethash (guild-id vote) *guild-perms*)))
    (error 'user-error
           :message (format nil "You do not have permission to ~
                                 cast ballots in this vote.")))
  (let ((ballot-id (make-id (ballot-objects vote))))
    (announce-ballot ballot ballot-id vote vote-id)
    (create
     (format nil "Cast ballot with ID ~@?" *id-format* ballot-id)
     user)
    (with-accessors ((ballot-ids ballot-ids)
                     (ballot-objects ballot-objects))
        vote
      (awhen (gethash (lc:id user) ballot-ids)
        (erase
         (from-id (announcement (gethash it (ballot-objects vote)))
                  :message))
        (remhash it ballot-objects))
      (setf (gethash (lc:id user) ballot-ids) ballot-id
            (gethash ballot-id ballot-objects) ballot))))

(defun process-ballot-msg (msg)
  (multiple-value-bind (choices vote vote-id)
      (parse-ballot-msg (lc:content msg))
    (publicize-ballot
     (make-instance 'ballot :choices choices :message (strip-message msg))
     vote
     vote-id
     (lc:author msg))))
