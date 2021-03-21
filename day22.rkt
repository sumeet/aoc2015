#lang racket

(require racket/set)

(define input-state
  #hash((our-hp . 50)
        (our-mp . 500)
        (our-armor . 0)
        (boss-hp . 58)
        (boss-dmg . 9)
        (total-mp-spent . 0)
        (current-spells . #hash())))

(define sample-state
  #hash((our-hp . 10)
        (our-mp . 250)
        (our-armor . 0)
        (boss-hp . 13)
        (boss-dmg . 8)
        (total-mp-spent . 0)
        (current-spells . #hash())))

(define (pop-and-apply-effects state)
  (let* ([current-spells (hash-ref state 'current-spells)]
         [state (foldl
                 (lambda (spell-name state)
                   (apply-state-changes
                    (first (hash-ref current-spells spell-name))
                    state))
                 state
                 (hash-keys current-spells))]
         [current-spells (hash-ref state 'current-spells)]
         [current-spells
          (make-immutable-hash (filter (lambda (l) (not (not l)))
                             (hash-map
                              current-spells
                              (lambda (spell-name actions)
                                (if (= (length actions) 1) #f (cons spell-name (rest actions)))))))])
    (hash-set state 'current-spells current-spells)))

(define (next-states state)
  (let* ([spell-names (castable-spell-names state)]
         ;; apply effects before player turn
         [state (pop-and-apply-effects state)])
    (if (either-ko? state)
     ;; if either is KOd after applying effects, short circuit
     (list state)
     ;; player 1 turn
     (let*-values ([(player-turns) (map (lambda (spell-name) (cast-spell state spell-name)) spell-names)]
                   [(ko-turns non-ko-turns) (partition either-ko? player-turns)]
                   [(after-p1-turn) (append ko-turns (map pop-and-apply-effects non-ko-turns))]
                   ;; now do the boss turn
                   [(after-boss-attack) (map (compose pop-and-apply-effects boss-turn) after-p1-turn)])
       after-boss-attack))))

;; (define (apply-player-spell-and-boss-attack state spell-name)
;;   (let ([state (pop-and-apply-effects state)])
;;     (if (either-ko? state)
;;         state
;;         (let ([state (cast-spell state spell-name)])
;;           (if (either-ko? state)
;;               state
;;               (let ([state (boss-turn state)])
;;                 (if (either-ko? state) state (pop-and-apply-effects state))))))))
(define (apply-player-spell-and-boss-attack state spell-name)
  (if-not-ko-then
   (pop-and-apply-effects state)
   (cast-spell state)
   ))

(define (if-not-ko-then state func) (if (either-ko? state) state (func state)))

(define (all-final-states-with-boss-ko state)
  (let*-values ([(states) (next-states state)]
                ;; filter out states where we die
                [(states) (filter (compose not us-ko?) states)]
                [(boss-ko-states not-yet-ko-states) (partition boss-ko? states)])
    (append boss-ko-states ((compose flatten map) all-final-states-with-boss-ko not-yet-ko-states))))

(define (least-mp-expenditures starting-state)
  (let ([all-mps (map
                  (lambda (state) (hash-ref state 'total-mp-spent))
                  (all-final-states-with-boss-ko starting-state))])
    (sort (all-mps starting-state) <)))


(define (either-ko? state) (or (us-ko? state) (boss-ko? state)))
(define (us-ko? state) ((hash-ref state 'our-hp) . <= . 0))
(define (boss-ko? state) ((hash-ref state 'boss-hp) . <= . 0))

(define (cost spell-name)
  (let* ([first-turn (first (hash-ref spells spell-name))]
         [mp-expenditure-action (findf
                                 (lambda (effect) (equal? (car effect) 'our-mp))
                                 first-turn)])
    (* -1 (second mp-expenditure-action))))

(define spells
  #hash[(magic-missile . [{(our-mp -53) (total-mp-spent +53) (boss-hp -4)}])
        (drain . [{(our-mp -73) (total-mp-spent +73) (boss-hp -2) (our-hp +2)}])
        (shield . [{(our-mp -113) (total-mp-spent +113) (our-armor +7)} ; should last 6 turns
                   {} {} {} {} {} {}
                   {(our-armor -7)}])
        (poison . [{(our-mp -173) (total-mp-spent +173)} ; should last 6 turns
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}
                   {(boss-hp -3)}])
        (recharge . [{(our-mp -229) (total-mp-spent +229)} ; should last 5 turns
                     {(our-mp +101)}
                     {(our-mp +101)}
                     {(our-mp +101)}
                     {(our-mp +101)}
                     {(our-mp +101)}])])

(define (apply-state-change change state)
  (hash-update state
               (car change)
               (lambda (x) (+ (second change) x))))

(define (apply-state-changes changes state)
  (foldl apply-state-change state changes))

(define (boss-turn state)
  (hash-update state 'our-hp
               (lambda (cur-hp)
                 ((cur-hp . - . (hash-ref state 'boss-dmg)) . + . (hash-ref state 'our-armor)))))

(define (cast-spell state spell-name)
  (let* ([spell-turns (hash-ref spells spell-name)]
         [to-apply-now (first spell-turns)]
         [to-apply-laters (rest spell-turns)]
         [state (apply-state-changes to-apply-now state)])
    (if
     (empty? to-apply-laters)
     state
     (hash-update state 'current-spells
                  (lambda (current-spells)
                    (hash-set current-spells spell-name to-apply-laters))))))

(define (remaining-mp state) (hash-ref state 'our-mp))

(define (castable-spell-names state)
  (let ([inactive-spell-names
         (set-subtract (hash-keys spells)
                       (hash-keys (hash-ref state 'current-spells)))])
    (filter
     (lambda (spell-name) ((cost spell-name) . <= . (remaining-mp state)))
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
