(module mummy mzscheme
;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;#reader(lib "htdp-advanced-reader.ss" "lang")((modname mummy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
  (require 2htdp/universe)
  (require 2htdp/image)
  (require scheme/base)

  ;;-------------------------------------------------------------------
  ;; data structures

  ;; bounding-box ::
  ;; top    : Number
  ;; right  : Number
  ;; bottom : Number
  ;; left   : Number
  (define-struct bounding-box (top right bottom left))

  ;; size ::
  ;; height : Number
  ;; width  : Number
  (define-struct size (height width))

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
  ;; lives   : Number
  ;; p       : sprite
  ;; mummies : (sprite)
  ;; crypts  : (crypt)
  (define-struct world (score lives p mummies crypts))

  ;;-------------------------------------------------------------------
  ;; Constants

  (define SCREEN-WIDTH 630)
  (define SCREEN-HEIGHT 420)
  (define-struct posn (x y))
  ; location of the starting door
  (define ENTRANCE-POS
    (make-posn 15 (/ SCREEN-HEIGHT 2)))

  ; valid directions for a moving character
  (define DIRECTIONS (list "up" "down" "left" "right"))

  ; a value representing a stopped character
  (define STUCK 'none)

  ; how fast can the player move
  (define PLAYER-SPEED 5)

  ; how many lives does the player start with?
  (define PLAYER-STARTING-LIVES 5)

  ; how fast can a mummy move
  (define MUMMY-SPEED 5)

  ; how large are sprites
  (define sprite-size (make-size 30 30))

  ; how large are crypts
  (define crypt-size (make-size 90 60))

  ;;-------------------------------------------------------------------
  ;; Scenes and layers

  ;; empty-scene :: image
  (define empty-scene (rectangle SCREEN-WIDTH SCREEN-HEIGHT 'solid 'white))

  ;; background-layer :: image
  (define background-layer (rectangle SCREEN-WIDTH SCREEN-HEIGHT 'solid 'black))

  ;; mummy-layer :: image
  (define mummy-layer
    (rectangle
      (size-height sprite-size)
      (size-width  sprite-size)
      'solid 'red))

  ;; player-layer :: image
  (define player-layer
    (rectangle
      (size-height sprite-size)
      (size-width  sprite-size)
      'solid 'green))

  ;; crypt-layer :: image
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

  ;; scroll-layer :: image
  (define scroll-layer empty-scene)

  ;; key-layer :: image
  (define key-layer empty-scene)

  ;; king-mummy-layer :: image
  (define king-mummy-layer empty-scene)

  ;; treasure-tomb-layer :: image
  (define treasure-tomb-layer empty-scene)

  ;;-------------------------------------------------------------------
  ;; initial states

  ;; initial-mummies :: (sprite)
  (define (initial-mummies)
    (list (make-sprite 30 300 STUCK MUMMY-SPEED)))

  ;; initial-crypts :: (crypt)
  (define (initial-crypts)
    (list
     (make-crypt 75 90 #f (make-object 'key))
     (make-crypt 195 90 #f (make-object 'key))
     (make-crypt 315 90 #f (make-object 'key))
     (make-crypt 435 90 #f (make-object 'key))
     (make-crypt 555 90 #f (make-object 'key))

     (make-crypt 75 180 #f (make-object 'key))
     (make-crypt 195 180 #f (make-object 'key))
     (make-crypt 315 180 #f (make-object 'key))
     (make-crypt 435 180 #f (make-object 'key))
     (make-crypt 555 180 #f (make-object 'key))

     (make-crypt 75 270 #f (make-object 'key))
     (make-crypt 195 270 #f (make-object 'key))
     (make-crypt 315 270 #f (make-object 'key))
     (make-crypt 435 270 #f (make-object 'key))
     (make-crypt 555 270 #f (make-object 'key))

     (make-crypt 75 360 #f (make-object 'key))
     (make-crypt 195 360 #f (make-object 'key))
     (make-crypt 315 360 #f (make-object 'key))
     (make-crypt 435 360 #f (make-object 'key))
     (make-crypt 555 360 #f (make-object 'key))))

  ;; initial-world :: world
  (define (initial-world)
    (make-world
      0
      PLAYER-STARTING-LIVES
      (make-sprite (posn-x ENTRANCE-POS) (posn-y ENTRANCE-POS) STUCK PLAYER-SPEED)
      (initial-mummies)
      (initial-crypts)))

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

  ;; sprite->bounding-box :: sprite -> bounding-box
  (define (sprite->bounding-box s)
    (let ((x (sprite-x s))
          (y (sprite-y s)))
      (make-bounding-box
        y
        (+ x (size-height sprite-size))
        (+ y (size-width sprite-size))
        x)))

  ;; crypt->bounding-box :: crypt -> bounding-box
  (define (crypt->bounding-box c)
    (let ((x (crypt-x c))
          (y (crypt-y c)))
      (make-bounding-box
        y
        (+ x (size-height crypt-size))
        (+ y (size-width crypt-size))
        x)))

  ;;-------------------------------------------------------------------
  ;; interface functions

  ;; render-player :: sprite -> scene -> image
  (define (render-player p scene)
    (place-image
      player-layer
      (sprite-x p)
      (sprite-y p)
      scene))

  ;; render-mummy :: sprite -> scene -> image
  (define (render-mummy m scene)
    (place-image
      mummy-layer
      (sprite-x m)
      (sprite-y m)
      scene))

  ;; render-crypt :: crypt -> scene -> image
  (define (render-crypt c scene)
    (place-image
      crypt-layer
      (crypt-x c)
      (crypt-y c)
      scene))

  ;; render-score :: Number -> scene -> image
  (define (render-score score scene)
    (place-image
      (text (number->string score) 24 "olive")
      20
      20
      scene))

  ;; render-lives :: Number -> scene -> image
  (define (render-lives lives scene)
    (place-image
      (text (number->string lives) 24 "olive")
      (- SCREEN-WIDTH 50)
      20
      scene))

  ;; render-world :: world -> image
  (define (render-world w)
    (render-player (world-p w)
      (render-lives (world-lives w)
        (render-score (world-score w)
          (foldr
            (lambda (m l) (render-mummy m l))
            (foldr
              (lambda (c l) (render-crypt c l))
               background-layer
               (world-crypts w))
            (world-mummies w))))))

  ;;-------------------------------------------------------------------
  ;; collision detection

  ;; hit-wall? :: sprite -> Boolean
  ;;;
  ;; determines whether the player is colliding with a wall
  (define (hit-wall? s)
    (let ((x (sprite-x s))
          (y (sprite-y s)))
      (or (zero? x) (= x SCREEN-WIDTH)
          (zero? y) (= y SCREEN-HEIGHT))))

  ;; collided? :: bounding-box -> bounding-box -> Boolean
  ;;;
  ;; determine whether 2 bounding boxes have collided which each other
  (define (collided? a b)
    (and
      (> (bounding-box-right  a) (bounding-box-left   b))
      (< (bounding-box-left   a) (bounding-box-right  b))
      (> (bounding-box-bottom a) (bounding-box-top    b))
      (< (bounding-box-top    a) (bounding-box-bottom b))))

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

  ;; player-was-eaten? :: sprite -> (sprite) -> Boolean
  (define (player-was-eaten? player mummies)
    (let ((player-bb (sprite->bounding-box player)))
      (findf
        (lambda (m) (collided? player-bb (sprite->bounding-box m)))
        mummies)))

  ;; update-world :: world -> world
  (define (update-world w)
    (let* ((crypts  (world-crypts w))
           (player  (move-sprite (world-p w)))
           (mummies (map move-sprite (world-mummies w)))
           (lives   (world-lives w)))
      (make-world
        (world-score w)
        (if (player-was-eaten? player mummies) (- lives 1) lives)
        player
        mummies
        crypts)))

  ;; no-lives-remaining :: w -> Boolean
  (define (no-lives-remaining w)
    (<= (world-lives w) 0))

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
          (world-lives w)
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
    (on-draw render-world)
    (stop-when no-lives-remaining)))