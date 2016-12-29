(in-package :coup)

;;TODO: make this a CLOS object so that I can
;;      automatically check action validity
(defstruct player
  (id :read-only :type 'symbol)
  (cards :read-only)
  (money :read-only)

  get-action
  influence-loss-fn
  exchange-fn
  block-fn
  contest-fn)

(defun copy-player-modified (p &key
                                 id
                                 cards
                                 money
                                 get-action
                                 influence-loss-fn
                                 exchange-fn
                                 contest-fn)
  (make-player :id (if id id (player-id p))
               :cards (if cards cards (player-cards p))
               :money (if money money (player-money p))
               :get-action (if get-action get-action (player-get-action p))
               :influence-loss-fn (if influence-loss-fn
                                      influence-loss-fn
                                      (player-influence-loss-fn p))
               :exchange-fn (if exchange-fn exchange-fn (player-exchange-fn p))
               :block-fn (if block-fn block-fn (player-block-fn p))
               :contest-fn (if contest-fn contest-fn (player-contest-fn p))))
