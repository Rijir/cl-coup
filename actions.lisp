(in-package :coup)

(require "alexandria")

(define-condition illegal-action-error (error) ())

;; TODO: more checks that actions are legal (by and target are valid players, etc)
(defclass action ()
  ((p :initarg :by
      :initform (error "Must specifiy a player doing this action")
      :reader action-by)))

(defclass income (action)
  ())

(defclass foreign-aid (action)
  ())

(defclass tax (action)
  ())

(defclass exchange (action)
  ())

(defclass block-action (action)
  ((target-action :initarg :target
                  :initform (error "Must specifiy an action to block")
                  :reader block-target)))

(defclass targeted-action (action)
  ((target :initarg :target
           :initform (error "Must specifiy a target player to assassinate")
           :reader action-target)))

(defclass assassinate (targeted-action)
  ())

(defclass steal (targeted-action)
  ())

(defclass coup (targeted-action)
  ())

(defgeneric block-is-valid (a p)
  (:method ((a foreign-aid) p) (find *duke* (player-cards p)))
  (:method ((a assassinate) p) (find *contessa* (player-cards p)))
  (:method ((a steal) p) (or (find *ambassador* (player-cards p))
                             (find *sargent* (player-cards p))))
  (:method (a p) nil))

(defgeneric action-is-valid (a)
  (:documentation "Returns the card which allows player p to perform action a
                   if one exists, t if no card is needed, or nil otherwise")
  (:method ((a income)) t)
  (:method ((a foreign-aid)) t)
  (:method ((a coup)) t)
  (:method ((a tax)) (find *duke* (player-cards (action-by a))))
  (:method ((a assassinate)) (find *assassin* (player-cards (action-by a))))
  (:method ((a exchange)) (find *ambassador* (player-cards (action-by a))))
  (:method ((a steal)) (find *sargent* (player-cards (action-by a))))
  (:method ((a block-action)) (block-is-valid (block-target a) (action-by a))))

(defun resolve-contest (a p1 p2 g)
  (let ((players (remove p1 (remove p2 (game-players g))))
        (deck (copy-seq (game-deck g)))
        (revealed-card (action-is-valid a p1)))
    (if revealed-card
        (progn
          (push revealed-card deck)
          (alexandria:shuffle deck)
          (let* ((p1-new (copy-player
                          p1
                          :cards (cons (pop deck)
                                       (remove revealed-card
                                               (player-cards p1)
                                               :count 1))))
                 (p2-new (multiple-value-bind (discard to-keep)
                             (funcall (player-influence-loss-fn p2)
                                      (player-cards p2))
                           (copy-player p2 :cards to-keep)))
                 (a-new (make-action :type (action-type a)
                                     :target (if (eq (action-target a)
                                                     p2)
                                                 p2-new
                                                 (action-target a)))))
            (push p1-new players)
            (push p2-new players)
            (apply-action a-new p1-new (copy-game-modified
                                        :players players
                                        :deck deck))))
        (multiple-value-bind (discard to-keep) (funcall
                                                (player-influence-loss-fn p1)
                                                (player-cards p1))
          ;; (setf (player-cards p1) to-keep)
          (push (copy-player p1 :cards to-keep) players)
          (push p2 players)
          (copy-game-modified
           :players players)))))

(defun contest-step (a g)
  (loop for p in (game-players g)
        when (player-contest? p a g)
          return (resolve-contest a (action-by a) p g)))

(defun block-step (a g)
  (loop for p in (game-players g)
        if (player-block? p a g)
          return (or (contest-step (make-instance 'block-action
                                                  :by p
                                                  :target a)
                                   g)
                     g)))

(defgeneric apply-action (a g)
  (:documentation "Returns a new game object representing the result of the
                   player announcing they are performing action a")
  (:method :around (a g)
    (or (block-step a g)
        (contest-step a g)
        (call-next-method))))

(defmethod apply-action :around ((a assassinate) g)
  (let ((p (action-by a)))
    (if (>= (player-money p) 3)
        (call-next-method a (inc-player-money p g -3))
        (error 'illegal-action-error))))

(defmethod apply-action :around ((a coup) g)
  (let ((p (action-by a)))
    (if (>= (player-money p) 7)
        (call-next-method a (inc-player-money p g -7))
        (error 'illegal-action-error))))

(defmethod apply-action ((a income) g)
  (inc-player-money (action-by a) g 1))

(defmethod apply-action ((a foreign-aid) g)
  (inc-player-money (action-by a) g 2))

(defmethod apply-action ((a tax) g)
  (inc-player-money (action-by a) g 3))

(defmethod apply-action ((a exchange) g)
  (let* ((p (action-by a))
         (deck (game-deck g))
         (cards (append (player-cards p)
                        (list (pop deck)
                              (pop deck))))
         (to-keep (player-exchange p cards g))
         (deck (append (remove (first to-keep)
                               (remove (second to-keep)
                                       cards
                                       :count 1)
                               :count 1)))
         (deck (alexandria:shuffle deck)))
    (copy-game-modified g
                        :players (substitute (copy-player p
                                                          :cards to-keep)
                                             p (game-players g))
                        :deck deck)))

(defmethod apply-action ((a assassinate) g)
  (lose-influence (action-target a) g))

(defmethod apply-action ((a coup) g)
  (lose-influence (action-target a) g))

(defmethod apply-action ((a steal) g)
  (let* ((p (action-by a))
         (target (action-target a))
         (money-available (min (player-money target) 2)))
    (inc-player-money p
                      (inc-player-money target
                                        g
                                        (- money-available))
                      money-available)))

(defmethod apply-action ((a block-action) g)
  g)

(defun lose-influence (p g)
  (let* ((lost-card (player-lose-card p g))
         (new-cards (remove lost-card (player-cards p) :count 1))
         (new-game (copy-game-modified
                    g
                    :players (substitute
                              (copy-player
                               p
                               :cards new-cards)
                              p
                              (game-players g)))))
    ;; (format t "discarding ~A~&" lost-card)
    ;; (format t "new hand: ~A~&" new-cards)
    ;; (format t "new game: ~A~&" new-game)
    new-game))

(defun inc-player-money (p g amount)
  (copy-game-modified g
                      :players (substitute
                                (copy-player
                                 p
                                 :money (+ (player-money p)
                                           amount))
                                p (game-players g))))
