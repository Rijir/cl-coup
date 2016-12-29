(in-package :coup)

(defun init-game (players &optional (num-copies 3))
  (let* ((deck (make-deck-shuffled num-copies))
         (players (loop for p in players
                        collect (copy-player-modified :cards (list (pop deck) (pop deck))
                                                      :money 3))))
    (make-game :players players :deck deck)))

(defun game-end (g)
  (loop for p in (game-players g)
        for num-alive = (if (> (length (player-cards p)) 0)
                            1
                            0)
          then (+ num-alive
                  (if (> (length (player-cards p)) 0)
                      1
                      0))
        when (> num-alive 1)
          do (return nil))
  t)

(defun contest-step (a p g)
  (loop for p2 in (game-players g)
        with contest-fn = (if (player-p p2)
                              (player-contest-fn p2)
                              (error 'error))
        if (contest-fn a p)
          return (resolve-contest a p p2 g)))

(defun block-step (a p g)
  (loop for p2 in (game-players g)
        with block-fn = (if (player-p p2)
                            (player-block-fn p2)
                            (error 'error))
        if (block-fn a p)
          return (let ((contest-result (contest-step (make-action
                                                      :type *block*
                                                      :target a)
                                                     p g)))
                   (or contest-result
                       g))))

(defun step-game (g pid)
  (let ((p (find-if (lambda (p) (eq pid (player-id p))) (game-players g))))
    (if (> (length (player-cards p)) 0)
        (let ((a (funcall (player-get-action p) g)))
          (if (action-is-legal a p g)
              (or (block-step a p g)
                  (contest-step a p g)
                  (apply-action a p g))
              (error 'illegal-action-error)))
        g)))

(defun run-game (g)
  (let ((player-ids (sort (map symbol #'player-id (game-players g))
                          (lambda (pid1 pid2) (string< (symbol-name pid1)
                                                       (symbol-name pid2)))))))
  (loop for round-number upfrom 1
        for g = g then (reduce #'step-game player-ids :initial-value g)
        until (game-end g)))
