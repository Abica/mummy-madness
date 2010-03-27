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
(define PLAYER-SPEED 5)

; how fast can a mummy move
(define MUMMY-SPEED 5)

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

;; crypt ::
;; x    : Number
;; y    : Number
;; open : Boolean
;; obj  : object
(define-struct crypt (x y open obj))

;; sprite ::
;; x         : Number
;; y         : Number
;; direction : DIRECTION
;; speed     : Number
(define-struct sprite (x y direction speed))

;; world ::
;; score   : Number
;; p       : sprite
;; mummies : (sprite)
;; crypts  : (crypt)
(define-struct world (score p mummies crypts))

;;-------------------------------------------------------------------
;; initial states

;; initial-mummies :: (sprite)
(define (initial-mummies)
  (list (make-sprite 300 300 "up" MUMMY-SPEED)))

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
    (make-sprite (posn-x ENTRANCE-POS) (posn-y ENTRANCE-POS) STUCK PLAYER-SPEED)
    (initial-mummies)
    (initial-crypts)))

;;-------------------------------------------------------------------
;; interface functions

; render-player :: sprite -> scene -> image
(define (render-player p scene)
  (place-image
    player-layer
    (sprite-x p)
    (sprite-y p)
    scene))

; render-mummy :: sprite -> scene -> image
(define (render-mummy m scene)
  (place-image
    mummy-layer
    (sprite-x m)
    (sprite-y m)
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
; hit-wall? :: sprite -> Boolean
(define (hit-wall? s)
  (let ((x (sprite-x s))
        (y (sprite-y s)))
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

;; sprite-stuck? :: sprite -> Boolean
(define (sprite-stuck? s)
  (equal? (sprite-direction s) 'none))

;;-------------------------------------------------------------------
;; movement functions

;; move-sprite :: sprite -> sprite
(define (move-sprite s)
  (let ((x     (sprite-x s))
        (y     (sprite-y s))
        (d     (sprite-direction s))
        (speed (sprite-speed s)))
    (cond
      [(equal? d "down")  (make-sprite x (+ y speed) d speed)]
      [(equal? d "up")    (make-sprite x (- y speed) d speed)]
      [(equal? d "left")  (make-sprite (- x speed) y d speed)]
      [(equal? d "right") (make-sprite (+ x speed) y d speed)]
      [else s])))

;; update-world :: world -> world
(define (update-world w)
  (let ((crypts (world-crypts w)))
    (make-world
      (world-score w)
      (move-sprite (world-p w))
      (map move-sprite (world-mummies w))
      crypts)))

;;-------------------------------------------------------------------
;; player input functions

;; handle-keyboard-input :: world -> direction -> world
(define (handle-keyboard-input w d)
  (let* ((p (world-p w))
         (x (sprite-x p))
         (y (sprite-y p)))
    (if (member d DIRECTIONS)
      (make-world
        (world-score w)
        (make-sprite x y d PLAYER-SPEED)
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
  (on-tick update-world)
  (on-draw render-world))