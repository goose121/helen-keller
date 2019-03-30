;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :helen-keller)

(defun establish-handler (evt handler)
  "Establish an event handler which, when called, establishes a
restart which allows aborting the current event."
  (add-event-handler evt
   (lambda (&rest args)
     (with-simple-restart (abort-handler "Aborts handling the current event.")
       (apply handler args)))))

(defparameter *vote-guild* 493584324696866846)
(defparameter *vote-channel* 561356214555115552)

(defbot *vote-bot*
    (with-open-file (token-file (asdf:system-relative-pathname
                                 :helen-keller
                                 "token.txt"))
      (read-line token-file)))

(defvar *ballot-objects* (make-hash-table)
  "Mapping from vote IDs to vote objects")
(defvar *user-ballots* (make-hash-table)
  "Mapping from user IDs to vote IDs")

(defun make-ballot-id ()
  "Create a unique random vote ID."
  (loop
     for random-id = (random (expt 36 20))
     when (with-hash-table-iterator (next-entry *ballot-objects*)
            (loop (multiple-value-bind (more key val) (next-entry)
                    (declare (ignore val))
                    (cond
                      ((not more) (return t))
                      ((eql key random-id) (return nil))))))
     do (return random-id)))

(defclass bare-message ()
  ((id :initarg :id :accessor id)
   (channel-id :initarg :channel-id :accessor channel-id))
  (:documentation "The bare minimum required to represent a message;
  that is, its ID and its channel ID"))

(defun strip-message (msg)
  "Converts MSG to a BARE-MESSAGE."
  (make-instance 'bare-message
                 :id (lc:id msg)
                 :channel-id (lc:channel-id msg)))

;; Go from a bare message to a full message
(defmethod from-id ((msg bare-message) (c (eql :message))
                    &optional (bot *client*))
  (from-id (id msg) (from-id (channel-id msg) :channel bot) bot))

(defclass ballot ()
  ((message :initarg :message)
   (candidates :initarg :candidates)
   (record :initarg :record :reader record)))

(defun read-until (char &rest args)
  "Read characters until one of them is equal to CHAR."
  (loop with string-read = (make-array '(0)
                                       :element-type 'character
                                       :fill-pointer t
                                       :adjustable t)
     for next-char = (apply #'read-char args)
     until (or (null next-char) (eql next-char char))
     do (vector-push-extend next-char string-read)
     finally (return (unless (and (null next-char) (= (length string-read) 0))
                       string-read))))

(defun extract-users (str)
  "Extract newline-separated username-discriminator
pairs (e.g. Bob#1234) from STR, and find the user ID's of the server
members with the usernames and discriminators specified in STR. The
user ID's returned are in no particular order."
  (let* ((username-pairs
          (delete-duplicates
           (with-input-from-string (names str)
            (loop
               for name = (read-until #\# names nil)
               for discrim = (read-until #\Newline names nil)
               while (and name discrim)
               collecting (list name discrim)))
           :test #'equal))
         (members (loop for memb across (lc:members (from-id *vote-guild* :guild))
                     for match = (find-if
                                  (lambda (user)
                                    (and (equal (car user) (lc:name (lc:user memb)))
                                         (equal (cadr user) (lc:discrim (lc:user memb)))))
                                  username-pairs)
                     when match
                     collect (cons (lc:id (lc:user memb)) match))))
    (unless (eql (length username-pairs) (length members))
      (error 'user-error
             :message (apply #'format nil "The user~#[s (EMPTY LIST) were ~;~:
 ~1{~A#~A~} was ~;~
s ~1{~A#~A~} and ~1{~A#~A~} were ~:;~
s~@{~#[~; and~] ~1{~A#~A~}~^,~} were ~]~
not found on the server." (set-difference username-pairs (mapcar #'cdr members) :test 'equal))))
    (mapcar #'car members)))

(defmacro user-ballot-id (user)
  "Get a user's ballot ID (defined as a macro to allow SETF to work
with it)."
  `(gethash (lc:id ,user) *user-ballots*))

(defmacro lookup-ballot (ballot-id)
  "Look up a ballot based on its ID (defined as a macro to allow SETF to work
with it)."
  `(gethash ,ballot-id *ballot-objects*))

(defun handle-message (msg)
  (when (and
         (not (lc:botp (lc:author msg)))
         (typep (from-id (lc:channel-id msg) :channel) 'lc:dm-channel))
    (handler-case
        (let* ((ballot-id (make-ballot-id))
               (votes (extract-users (lc:content msg)))
               (ballot-record (create
                               (format nil "Ballot ID: ~36R~%Votes:~%~{<@~A>~%~}"
                                       ballot-id
                                       votes)
                               (from-id *vote-channel* :channel)
                               *client*)))
          ;; If the user already voted, delete the earlier ballot
          ;; record
          (awhen (user-ballot-id (lc:author msg))
            (let ((old-ballot (lookup-ballot it)))
              (erase (from-id (record old-ballot) :message))))
          (setf (gethash (lc:id (lc:author msg)) *user-ballots*) ballot-id)
          (setf (gethash ballot-id *ballot-objects*)
                (make-instance 'ballot
                               :message (strip-message msg)
                               :candidates votes
                               :record (strip-message ballot-record)))
          (reply msg (format nil "Your ballot ID is ~36R" ballot-id)))
      (user-error (err) (reply msg (format nil "Error: ~a" err))))))

(establish-handler :on-message-create 'handle-message)
