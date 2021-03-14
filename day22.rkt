#lang racket
(require struct-update)

(struct game-state (our-hp our-mp our-armor boss-hp boss-dmg))
(define-struct-updaters game-state)

;; TODO: can we turn this into a macro?
(define
  (show-state state)
  (format "state: our-hp ~a our-mp ~a our-armor ~a boss-hp ~a boss-dmg ~a"
          (game-state-our-hp state) (game-state-our-mp state)
          (game-state-our-armor state) (game-state-boss-hp state)
          (game-state-boss-dmg state)))

(define initial-state (game-state 50 500 0 58 9))

(define spells
  '[('magic-missile '['{'('our-mp -53) '('boss-hp -4)}])
    ('drain '['{'('our-mp -73) '('boss-hp -2) '('our-hp +2)}])
    ('shield '['{'('our-mp -113) '('our-armor +7)} ; should last 6 turns
               '{}
               '{}
               '{}
               '{}
               '{}
               '{}
               '{'('our-armor -7)}])
    ('poison '['{'('our-mp -173)} ; should last 6 turns
               '{'('boss-hp -3)}
               '{'('boss-hp -3)}
               '{'('boss-hp -3)}
               '{'('boss-hp -3)}
               '{'('boss-hp -3)}
               '{'('boss-hp -3)}])
    ('recharge '['{'('our-mp -229)} ; should last 5 turns
                 '{'('our-mp +101)}
                 '{'('our-mp +101)}
                 '{'('our-mp +101)}
                 '{'('our-mp +101)}
                 '{'('our-mp +101)}])])



;; (print (show-state initial-state))
(pretty-print spells)
