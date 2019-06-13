(in-package :helen-keller)

(defclass permission-criterion () ()
  (:documentation "Base class for criteria which can be used in deciding whether
  to grant a user permission to take a certain action."))

(defgeneric satisfies-p (user criterion)
  (:documentation "Checks whether USER satisfies CRITERION."))

(defclass guild-criterion (permission-criterion)
  ((guild-id :initarg :guild-id :accessor guild-id))
  (:documentation "A criterion which states that a user must be in a
  specific guild. Its SATISFIES-P method returns the member object
  corresponding to the user's membership in the guild."))

(cpk:defencoding guild-criterion
  guild-id)

(defmethod satisfies-p (user (criterion guild-criterion))
  (lc:member user (from-id (guild-id criterion) :guild)))

(defclass role-criterion (guild-criterion)
  ((role-id :initarg :role-id :accessor role-id))
  (:documentation "A criterion which states that a user must have a
  specific role in a specific guild."))

(cpk:defencoding role-criterion
  guild-id role-id)

(defmethod satisfies-p (user (criterion role-criterion))
  (let ((member (call-next-method)))
    (and
     member
     (find (role-id criterion) (lc:roles member) :key #'lc:id))))

(defclass any-criteria (permission-criterion)
  ((subcriteria :initarg :criteria :accessor subcriteria)))

(defmethod satisfies-p (user (criterion any-criteria))
  (some (lambda (c) (satisfies-p user c)) (subcriteria criterion)))
