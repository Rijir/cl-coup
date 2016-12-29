(in-package :coup)

(require "alexandria")

(define-condition illegal-action-error (error) ())

(defvar *income* 'income)
(defvar *foreign-aid* 'foreign-aid)
(defvar *tax* 'tax)
(defvar *assassinate* 'assassinate)
(defvar *exchange* 'exchange)
(defvar *steal* 'steal)
(defvar *coup* 'coup)
(defvar *block* 'block)

(defvar *all-action-types* (list *income*
                            *foreign-aid*
                            *tax*
                            *assassinate*
                            *exchange*
                            *steal*
                            *coup*
                            *block*))

(defstruct action (type :read-only) (target :read-only))

(defun action-is-legal (a p g)
  (if (> (length (player-cards p)) 0)
      (case (action-type a)
        ((*income* *foreign-aid* *tax* *exchange*) (null (action-target a)))
        (*assassinate* (and (>= (player-money p) 3)
                            (not (null (find (action-target a) (game-players g))))))
        (*coup* (and (>= (player-money p) 7)
                     (not (null (find (action-target a) (game-players g))))))
        (*steal* (not (null (find (action-target a) (game-players g)))))
        (*block* (actionp (action-target a))))
      nil))

(defun action-is-valid (a p)
  (let ((cards (player-cards p)))
    (case (action-type a)
      ((*income* *foreign-aid* *coup*) t
       (*tax* (find *duke* cards))
       (*steal* (find *sargent* cards))
       (*assassinate* (find *assassin* cards))
       (*exchange* (find *ambassador* cards))
       (*block* (case (action-type a)
                  ((*foreign-aid* (find *duke* cards))
                   (*steal* (or (find *sargent* cards)
                                (find *ambassador* cards)))
                   (*assassinate* (find *contessa* cards)))))))))

(defun apply-action (a p g)
  (if (action-is-legal a p g)
      (case (action-type a)
        (*income* (income p))
        (*foreign-aid* (foreign-aid p))
        (*tax* (tax p))
        (*assassinate* (assassinate (action-target a)
                                    (player-influence-loss-fn (action-target a))))
        (*coup* (coup (action-target a)
                      (player-influence-loss-fn (action-target a))))
        (*exchange* (exchange p g (player-exchange-fn p)))
        (*steal* (steal p (action-target a)))
        (*block* g)
        (t (error 'illegal-action-error)))
      (error 'illegal-action-error)))

(defun resolve-contest (a p1 p2 g)
  (let ((players (remove p1 (remove p2 (game-players g))))
        (deck (copy-seq (game-deck g)))
        (revealed-card (action-is-valid a p)))
    (if revealed-card
        (progn
          (push revealed-card deck)
          (alexandria:shuffle deck)
          (let* ((p1-new (copy-player-modified
                          p1
                          :cards (cons (pop deck)
                                       (remove revealed-card
                                               (player-cards p1)
                                               :count 1))))
                 (p2-new (multiple-value-bind (discard to-keep)
                             (funcall (player-influence-loss-fn p2)
                                      (player-cards p2))
                           (copy-player-modified p2 :cards to-keep)))
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
          (push (copy-player-modified p1 :cards to-keep) players)
          (push p2 players)
          (copy-game-modified
           :players players)))))

;; (defun contest-or-apply-action (a p g)
;;   (let ((contesting-player (loop for p2 in (game-players g)
;;                                  with contest-fn = (player-contest-fn p2)
;;                                  if (contest-fn a p)
;;                                    return p2)))
;;     (if contesting-player
;;         (resolve-contest (a p contesting-player g))
;;         (apply-action a p g))))

(defun inc-player-money (p g amount)
  (copy-game-modified g
                      :players (substitute
                                (copy-player-modified
                                 p
                                 :money (+ (player-money p)
                                           amount))
                                p (game-players g))))

(defun income (p g)
  (inc-player-money p g 1))

(defun foreign-aid (p g)
  (inc-player-money p g 2))

(defun tax (p g)
  (inc-player-money p g 3))

(defun assassinate-pre (p)
  (if (>= (player-money p) 3)
      (inc-player-money p g -3)
      (error 'illegal-action-error)))

(defun assassinate (p choose-fn)
  (lose-influence p choose-fn))

(defun coup-pre (p g)
  (if (>= (player-money p) 7)
      (inc-player-money p g -7)
      (error 'illegal-action-error)))

(defun coup (p choose-fn)
  (lose-influence p choose-fn))

(defun lose-influence (p g)
   "choose-fn: function to call to get which card the player wants to loose
              list of player's cards
              => cards to keep (same as previous cards minus one)"
  ;; (let ((influence-prev (length (player-cards p))))
  ;;   (multiple-value-bind (to-discard to-keep) (choose-fn (player-cards p))
  ;;     (if (and (eql (length to-keep) (- influence-prev 1))
  ;;              (equal (sort (copy-seq (player-cards p)) #'card<)
  ;;                     (sort (copy-seq (cons to-discard to-keep)) #'card<)))
  ;;         (setf (player-cards p) to-keep)
  ;;         (error 'illegal-action-error))))
  (copy-game-modified
   g
   :players (substitute (copy-player-modified p
                                              :cards
                                              (funcall influence-loss-fn
                                                       (player-cards p)))
                        p
                        (game-players g))))

(defun exchange (p g choose-fn)
  "p: the player using the exchange action
   g: the current game struct
   choose-fn: function to call to get the player's 2 cards to keep
              list of cards, player influence
              => first value: cards to keep
                 second value: cards to put back in the deck"
  ;; (let ((influence (length (player-cards p)))
  ;;       (cards (append (player-cards p)
  ;;                      (list (pop (game-deck g))
  ;;                            (pop (game-deck g))))))
  ;;   (multiple-value-bind (to-keep to-shuffle) (choose-fn (copy-seq cards) influence)
  ;;     (if (and (eql influence (length to-keep))
  ;;              (equal (sort (copy-seq cards) #'card<)
  ;;                     (sort (copy-seq (append to-keep to-shuffle)) #'card<)))
  ;;         (progn
  ;;           (setf (player-cards p) to-keep)
  ;;           (setf (game-deck g) (alexandria:shuffle
  ;;                                (append to-shuffle (game-deck g)))))
  ;;         (error 'illegal-action-error))))
  (let ((deck (copy-seq (game-deck g)))
        (influence (length (player-cards p))))
    (multiple-value-bind (to-keep to-shuffle) (funcall (player-exchange-fn p)
                                                       (append (player-cards p)
                                                               (list (pop deck)
                                                                     (pop deck)))
                                                       influence)
      (copy-game-modified
       g
       :players (substitute (copy-player-modified p :cards to-keep)
                            p
                            (game-players g))
       :deck (alexandria:shuffle (append to-shuffle deck))))))

(defun steal (p1 p2)
  "p1: the player using the steal action
   p2: the player to be stolen from"
  ;; (let ((money-available (min (player-money p2) 2)))
  ;;   (incf (player-money p1) money-available)
  ;;   (decf (player-money p2) money-available))
  (let ((money-available (min (player-money p2) 2)))
    (copy-game-modified
     g
     :players (substitute (copy-player-modified p1
                                                :money (+ money-available
                                                          (player-money p1)))
                          p1
                          (substitute (copy-player-modified p2
                                                            :money (- money-available
                                                                      (player-money p2)))
                                      p2
                                      (game-players g))))))
