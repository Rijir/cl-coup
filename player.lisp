(in-package :coup)

(defvar *player-id* 0)

;;TODO: make this a CLOS object so that I can
;;      automatically check action validity
(defclass player ()
  ((id :initarg :id
       :initform (incf *player-id*)
       :reader player-id)
   (cards :initarg :cards
          :initform '()
          :reader player-cards)
   (money :initarg :money
          :initform 0
          :reader player-money)))

;;TODO: add arround methods for each of these to validate returns
;; except that could be circumvented by a more specific arround method...
;; real solution would be to have functions wrap these methods which verify
;; returns

(defgeneric player-get-action (p g)
  (:documentation "Gets the action which the player would like to perform
                   this turn"))

(defgeneric player-lose-card (p g)
  (:documentation "Asks the player which card they want to lose.
                   Returns one card, which will be removed from the player's hand"))

(defgeneric player-exchange (p cards g)
  (:documentation "Asks the player to choose the card(s) to keep from the list
                   of cards presented from an exchange action.
                   Returns a list of cards to keep"))

(defgeneric player-block? (p a g)
  (:documentation "Asks the player if they want to block action a."))

(defgeneric player-contest? (p a g)
  (:documentation "Asks the player if they want to contest action a."))

(defgeneric copy-player (p &key
                             id
                             cards
                             money
                             &allow-other-keys)
  (:documentation "Initializes a new player of the same type with slots copied
                   from original player p, overriden by keyword args.")
  (:method ((p player) &key id cards money &allow-other-keys)
    (make-instance (class-of p)
                   :id (or id (player-id p))
                   :cards (or cards (player-cards p))
                   :money (or money (player-money p)))))

(defgeneric players-equal (p1 p2)
  (:method ((p1 player) (p2 player))
    (and (eq (player-id p1) (player-id p2))
         (equal (player-cards p1) (player-cards p2))
         (= (player-money p1) (player-money p2)))))

(defmethod print-object ((object player) stream)
  (format stream
          "~A~:@<ID: ~A, CARDS: ~A, MONEY: ~A~@:>"
          (class-name (class-of object))
          (player-id object)
          (player-cards object)
          (player-money object)))
