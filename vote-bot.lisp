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
  (add-event-handler
   evt
   (lambda (&rest args)
     (with-simple-restart
         (abort-handler "Aborts handling the current event.")
       (apply handler args)))))

(defbot *vote-bot*
    (with-open-file (token-file (asdf:system-relative-pathname
                                 :helen-keller
                                 "token.txt"))
      (read-line token-file)))

(defun handle-message (msg)
  (handler-case
      (cond
        ((lc:botp (lc:author msg)) nil)
        ((typep (from-id (lc:channel-id msg) :channel) 'lc:dm-channel)
         (process-ballot-msg msg))
        ((and
          (typep (from-id (lc:channel-id msg) :channel) 'lc:guild-channel)
          (string^= "&make-vote" (lc:content msg)))
         (process-vote-msg msg)))
    (user-error (err) (reply msg (format nil "Error: ~a" err)))))

(defun start-bot ()
  (establish-handler :on-message-create 'handle-message)
  (connect *vote-bot*))
