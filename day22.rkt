#lang racket

(require racket/set)

(define initial-state
  #hash((our-hp . 50)
        (our-mp . 500)
        (our-armor . 0)
        (boss-hp . 58)
        (boss-dmg . 9)
        (current-spells . #hash())))

(define (cost spell-name)
  (let* ([first-turn (first (hash-ref spells spell-name))]
         [mp-expenditure-action (findf
                                 (lambda (effect) (equal? (car effect) 'our-mp))
                                 first-turn)])
    (* -1 (second mp-expenditure-action))))

(define spells
  #hash[(magic-missile . [{(our-mp -53) (boss-hp -4)}])
        (drain . [{(our-mp -73) (boss-hp -2) (our-hp +2)}])
        (shield . [{(our-mp -113) (our-armor +7)} ; should last 6 turns
                   {} {} {} {} {} {}
                   {(our-armor -7)}])
        (poison . [{(our-mp -173)} ; should last 6 turns
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}])
        (recharge . [{(our-mp -229)} ; should last 5 turns
                     {(our-mp +101)}
                     {(our-mp +101)}
                     {(our-mp +101)}
                     {(our-mp +101)}
                     {(our-mp +101)}])])

(define (apply-state-change state change)
  (hash-update state
               (car change)
               (lambda (x) (+ (second change) x))))


(define (cast-spell spell-name state)
  (let* ([spell-turns (hash-ref spells spell-name)]
         [to-apply-now (first spell-turns)]
         [to-apply-laters (rest spell-turns)])
    'hello))

(define (remaining-mp state) (hash-ref state 'our-mp))

(define (castable-spell-names state)
  (let ([inactive-spell-names
         (set-subtract (hash-keys spells)
                       (hash-keys (hash-ref state 'current-spells)))])
    (filter (lambda (spell-name) ((cost spell-name) . <= . (remaining-mp state)))
            inactive-spell-names)))




















;; TODO: try converting this to use structs (might need to use macros)
;; (require struct-update)

;; (struct game-state (our-hp our-mp our-armor boss-hp boss-dmg))
;; (define-struct-updaters game-state)

;; TODO: can we turn this into a macro?
;; (define
;; ;;   (show-state state)
;; ;;   (format "state: our-hp ~a our-mp ~a our-armor ~a boss-hp ~a boss-dmg ~a"
;; ;;           (game-state-our-hp state) (game-state-our-mp state)
;; ;;           (game-state-our-armor state) (game-state-boss-hp state)
;; ;;           (game-state-boss-dmg state)))

;; (define initial-state (game-state 50 500 0 58 9))
