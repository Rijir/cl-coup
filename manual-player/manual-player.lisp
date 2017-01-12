(in-package :cl-user)

(defpackage :coup-manual-player
  (:use :cl :coup))

(in-package :coup-manual-player)

(defclass manual-player (player)
  ())

(defun choose-from-options (options)
  (loop for i upfrom 0
        for option in options
        do (format t "[~D] ~A~&" i option))
  (format t "Enter option number: ")
  (handler-bind ((sb-int:simple-parse-error (lambda (foo)
                                              (format t "Invalid option~&")
                                              (choose-from-options options))))
    (let ((choice (read)))
      (if (and (integerp choice)
               (< choice (length options)))
          (nth choice options)
          (error "Invalid option number")))))

(defmethod player-get-action ((p manual-player) g)
  (format t "~A~&" p)
  (let ((action-type (choose-from-options
                      '(income
                        foreign-aid
                        tax
                        exchange
                        assassinate
                        steal
                        coup))))
    (case action-type
      ((income foreign-aid tax exchange)
       (make-instance action-type
                      :by p))
      ((assassinate steal coup)
       (make-instance action-type
                      :by p
                      :target (choose-from-options (game-players g)))))))

(defmethod player-lose-card ((p manual-player) g)
  (format t "Choose which card to lose~&")
  (choose-from-options (player-cards p)))

(defmethod player-exchange ((p manual-player) cards g)
  (format t "Choose first card to keep~&")
  (let ((first-card (choose-from-options cards)))
    (format t "Choose second card to keep~&")
    (list first-card
          (choose-from-options (remove first-card
                                       cards
                                       :count 1)))))

(defmethod player-block? ((p manual-player) a g)
  (y-or-n-p "<~D> Block ~A?" (player-id p) a))

(defmethod player-contest? ((p manual-player) a g)
  (y-or-n-p "<~D> Contest ~A?" (player-id p) a))

(defun run-new-manual-game (num-players)
  (let* ((ps (loop for i from 1 to num-players
                   collect (make-instance 'manual-player)))
         (g (init-game ps)))
    (run-game g)))
