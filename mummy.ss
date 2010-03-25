;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname mummy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct size (height width))
;;-------------------------------------------------------------------
;; Constants

(define SCREEN-WIDTH 600)
(define SCREEN-HEIGHT 400)

; location of the starting door
(define ENTRANCE-POS
  (make-posn 15 (/ SCREEN-HEIGHT 2)))

; valid directions for a moving character
(define DIRECTIONS (list "up" "down" "left" "right"))

; a value representing a stopped character
(define STUCK 'none)

; how fast can the player move
(define PLAYER-SPEED 2)

; how fast can a mummy move
(define MUMMY-SPEED 2)

(define player-size (make-size 25 25))
(define crypt-size  (make-size 60 40))
(define mummy-size  (make-size 25 25))

;;-------------------------------------------------------------------
;; Scenes and layers

(define empty-scene (rectangle SCREEN-WIDTH SCREEN-HEIGHT 'solid 'white))
(define background-layer empty-scene)

(define mummy-layer
  (rectangle
    (size-height mummy-size)
    (size-width  mummy-size)
    'solid 'red))

(define player-layer
  (rectangle
    (size-height player-size)
    (size-width  player-size)
    'solid 'green))

(define crypt-layer
  (overlay
    (rectangle
      (size-height crypt-size)
      (size-width crypt-size)
      'solid 'gray)
    (rectangle
      (size-height crypt-size)
      (size-width crypt-size)
      'outline 'black)))

(define scroll-layer empty-scene)
(define key-layer empty-scene)
(define king-mummy-layer empty-scene)
(define treasure-tomb-layer empty-scene)

;;-------------------------------------------------------------------
;; data structures

;; object ::
;; type : item-type
(define-struct object (type))

;; player ::
;; x         : Number
;; y         : Number
;; direction : DIRECTION
(define-struct player (x y direction))

;; crypt ::
;; x    : Number
;; y    : Number
;; open : Boolean
;; obj  : object
(define-struct crypt (x y open obj))

;; mummy ::
;; x         : Number
;; y         : Number
;; direction : DIRECTION
(define-struct mummy (x y direction))

;; world ::
;; score   : Number
;; p       : player
;; mummies : (mummy)
;; crypts  : (crypt)
(define-struct world (score p mummies crypts))

;;-------------------------------------------------------------------
;; initial states

;; initial-mummies :: (mummy)
(define (initial-mummies)
  (list (make-mummy 300 300 "up")))

;; initial-crypts :: (crypt)
(define (initial-crypts)
  (list
   (make-crypt 100 80 false (make-object 'key))
   (make-crypt 200 80 false (make-object 'key))
   (make-crypt 300 80 false (make-object 'key))
   (make-crypt 400 80 false (make-object 'key))
   (make-crypt 500 80 false (make-object 'key))
   
   (make-crypt 100 160 false (make-object 'key))
   (make-crypt 200 160 false (make-object 'key))
   (make-crypt 300 160 false (make-object 'key))
   (make-crypt 400 160 false (make-object 'key))
   (make-crypt 500 160 false (make-object 'key))
   
   (make-crypt 100 240 false (make-object 'key))
   (make-crypt 200 240 false (make-object 'key))
   (make-crypt 300 240 false (make-object 'key))
   (make-crypt 400 240 false (make-object 'key))
   (make-crypt 500 240 false (make-object 'key))
   
   (make-crypt 100 320 false (make-object 'key))
   (make-crypt 200 320 false (make-object 'key))
   (make-crypt 300 320 false (make-object 'key))
   (make-crypt 400 320 false (make-object 'key))
   (make-crypt 500 320 false (make-object 'key))))

;; initial-world :: world
(define (initial-world)
  (make-world
    0
    (make-player (posn-x ENTRANCE-POS) (posn-y ENTRANCE-POS) STUCK)
    (initial-mummies)
    (initial-crypts)))

;;-------------------------------------------------------------------
;; interface functions

; render-player :: player -> scene -> image
(define (render-player p scene)
  (place-image
    player-layer
    (player-x p)
    (player-y p)
    scene))

; render-mummy :: mummy -> scene -> image
(define (render-mummy m scene)
  (place-image
    mummy-layer
    (mummy-x m)
    (mummy-y m)
    scene))

; render-crypt :: crypt -> scene -> image
(define (render-crypt c scene)
  (place-image
    crypt-layer
    (crypt-x c)
    (crypt-y c)
    scene))

; render-world :: world -> image
(define (render-world w)
  (render-player (world-p w)
    (foldr
      (lambda (m l) (render-mummy m l))
      (foldr
        (lambda (c l) (render-crypt c l))
         background-layer
         (world-crypts w))
      (world-mummies w))))

;;-------------------------------------------------------------------
;; collision detection

; determines whether the player is colliding with a wall
; hit-wall? :: world -> world
(define (hit-wall? w)
  (let* ((p (world-p w))
         (x (player-x p))
         (y (player-y p)))
    (or (zero? x) (= x SCREEN-WIDTH)
        (zero? y) (= y SCREEN-HEIGHT))))

;;-------------------------------------------------------------------
;; utility functions

; takes a direction and flips it
; opposite-direction -> direction -> direction
(define (opposite-direction d)
  (cond
    ([equal? d "up"] "down")
    ([equal? d "down"] "up")
    ([equal? d "left"] "right")
    ([equal? d "right"] "left")))

;; player-stuck? :: player -> Boolean
(define (player-stuck? p)
  (equal? (player-direction p) 'none))

;;-------------------------------------------------------------------
;; movement functions

;; move-player :: player -> player
(define (move-player p)
  (let ((x (player-x p))
        (y (player-y p))
        (d (player-direction p)))
    (cond
      [(equal? d "down")  (make-player x (+ y PLAYER-SPEED) d)]
      [(equal? d "up")    (make-player x (- y PLAYER-SPEED) d)]
      [(equal? d "left")  (make-player (- x PLAYER-SPEED) y d)]
      [(equal? d "right") (make-player (+ x PLAYER-SPEED) y d)]
      [else p])))

;; move-mummy :: mummy -> mummy
(define (move-mummy m)
  (let ((x (mummy-x m))
        (y (mummy-y m))
        (d (mummy-direction m)))
    (cond
      [(equal? d "down")  (make-mummy x (+ y MUMMY-SPEED) d)]
      [(equal? d "up")    (make-mummy x (- y MUMMY-SPEED) d)]
      [(equal? d "left")  (make-mummy (- x MUMMY-SPEED) y d)]
      [(equal? d "right") (make-mummy (+ x MUMMY-SPEED) y d)]
      [else m])))

;; move-objects :: world -> world
(define (move-objects w)
  (make-world
    (world-score w)
    (move-player (world-p w))
    (map move-mummy (world-mummies w))
    (world-crypts w)))

;; handle-keyboard-input :: world -> direction -> world
(define (handle-keyboard-input w d)
  (let* ((p (world-p w))
         (x (player-x p))
         (y (player-y p)))
    (if (member d DIRECTIONS)
      (make-world
        (world-score w)
        (make-player x y d)
        (world-mummies w)
        (world-crypts w))
      w)))

    ;       ;;;;;;; ;;;;;;;  ;;;;;;; ;     ; ;;;;;;; ;;;;;;  ;;;;;;;
   ;       ;          ;        ;    ;     ; ;       ;     ; ;
  ;       ;;;;;;;    ;        ;    ;;;;;;; ;;;;;;; ;;;;;;  ;;;;;;;
 ;       ;          ;        ;    ;     ; ;       ;   ;   ;
;;;;;;; ;;;;;;;    ;        ;    ;     ; ;;;;;;; ;     ; ;;;;;;; BE LIGHT
(big-bang
  (initial-world)
  (on-key handle-keyboard-input)
  (on-tick move-objects)
  (on-draw render-world))