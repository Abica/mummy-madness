(module mummy mzscheme
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
  ;; x              : Number
  ;; y              : Number
  ;; direction      : DIRECTION
  ;; last-direction : DIRECTION
  ;; speed          : Number
  (define-struct sprite (x y direction last-direction speed))

  ;; world ::
  ;; score   : Number
  ;; lives   : Number
  ;; p       : sprite
  ;; mummies : (sprite)
  ;; crypts  : (crypt)
  (define-struct world (score lives p mummies crypts))

  
  ;;-------------------------------------------------------------------
  ;; Constants

  (define SCREEN-WIDTH 660)
  (define SCREEN-HEIGHT 450)

  (define GRID-LEFT-MARGIN 15)
  (define GRID-TOP-MARGIN 44)
  (define GRID-WIDTH (- SCREEN-WIDTH  30))
  (define GRID-HEIGHT (- SCREEN-HEIGHT 59))
  
  (define-struct posn (x y))

  ; how large are sprites
  (define sprite-size (make-size 30 30))

  ; how large are crypts
  (define crypt-size (make-size 90 60))
  
  ; location of the starting door
  (define ENTRANCE-POS
    (make-posn (/ SCREEN-WIDTH 2) (+ (size-height sprite-size) 30)))

  ; valid directions for a moving character
  (define DIRECTIONS (list "up" "down" "left" "right"))

  ; a value representing a stopped character
  (define STUCK 'none)

  ; how many lives does the player start with?
  (define PLAYER-STARTING-LIVES 5)

  ; how fast can the player move
  (define PLAYER-SPEED 30)

  ; how fast can a mummy move
  (define MUMMY-SPEED 4)
  

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
        (+ x (size-width sprite-size))
        (+ y (size-height sprite-size))
        x)))

  ;; crypt->bounding-box :: crypt -> bounding-box
  (define (crypt->bounding-box c)
    (let ((x (crypt-x c))
          (y (crypt-y c)))
      (make-bounding-box
        y
        (+ x (size-width crypt-size))
        (+ y (size-height crypt-size))
        x)))

  
  ;;-------------------------------------------------------------------
  ;; collision detection

  ;; hit-wall? :: sprite -> Boolean
  ;;;
  ;; determines whether the player is colliding with a wall
  (define (hit-wall? s)
    (let ((x (sprite-x s))
          (y (sprite-y s)))
      (or (<= x 0) (>= x SCREEN-WIDTH)
          (<= y 30) (>= y SCREEN-HEIGHT))))

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

  ;; snap-x :: Number -> Number
  (define (snap-x x)
    (let ((w (size-width sprite-size)))
      (* w
         (round (/ x w)))))

  ;; snap-y :: Number -> Number
  (define (snap-y y)
    (let ((h (size-height sprite-size)))
      (* h
         (round (/ y h)))))
  
  ;; direction-to-axis :: DIRECTION -> ('x | 'y)
  (define (direction-to-axis d)
    (if
      (member d (list "up" "down"))
      'y
      'x))
 
  ;; on-x-axis? :: sprite -> Boolean
  (define (on-x-axis? s)
    (= (modulo (sprite-y s) 90) 60))
  
  ;; on-y-axis? :: sprite -> Boolean
  (define (on-y-axis? s)
    (= (modulo (sprite-x s) 120) 30))
  
  ;; at-crossroads? :: sprite -> Boolean
  (define (at-crossroads? s)
    (and (on-x-axis? s) (on-y-axis? s)))
  
  ;; axis-blocked? :: sprite -> ('x | 'y) -> Boolean
  (define (axis-blocked? s axis)
    (not
      (if (equal? axis 'x)
          (on-x-axis? s)
          (on-y-axis? s))))
  
  ;; direction-blocked? :: sprite -> DIRECTION -> Boolean
  (define (direction-blocked? s d)
    (axis-blocked? s (direction-to-axis d)))
  
  ;; first-unblocked-direction :: sprite -> DIRECTION
  (define (first-unblocked-direction s)
    (findf (lambda (d)
             (not (direction-blocked? s d))) 
           DIRECTIONS))

  ;; update-sprite-position :: sprite -> ('x | 'y) -> (+ | -) -> sprite
  (define (update-sprite-position s axis operator)
    (let* ((speed (sprite-speed s))
           (x     (sprite-x s))
           (y     (sprite-y s))
           (d     (sprite-direction s))
           (ld    (sprite-last-direction s))
           (new-s (if (equal? axis 'x)
                      (make-sprite (operator x speed) y STUCK d speed)
                      (make-sprite x (operator y speed) STUCK d speed))))
      (if (or (hit-wall? new-s)
              (axis-blocked? new-s axis))
          (make-sprite x y d ld speed)
          new-s)))

  ;; move-sprite :: sprite -> sprite
  (define (move-sprite s)
    (let* ((d (sprite-direction s)))
      (cond
        [(equal? d "down")  (update-sprite-position s 'y +)]
        [(equal? d "up")    (update-sprite-position s 'y -)]
        [(equal? d "left")  (update-sprite-position s 'x -)]
        [(equal? d "right") (update-sprite-position s 'x +)]
        [else s])))
 
    
  ;;-------------------------------------------------------------------
  ;; enemy ai

  ;; mummy-follows-player :: sprite -> sprite -> sprite
  (define (mummy-follows-player m p)
    (let* ((mx     (sprite-x m))
           (my     (sprite-y m))
           (px     (sprite-x p))
           (py     (sprite-y p))
           (y-axis (if (> my py) "up" "down"))
           (x-axis (if (> mx px) "left" "right"))
           (x-blocked? (direction-blocked? m x-axis))
           (y-blocked? (direction-blocked? m y-axis)))
    (move-sprite
     (make-sprite
       mx my
       (if (at-crossroads? m)
         (if (> (abs (- px mx))
                (abs (- py my)))
              x-axis
              y-axis)
         (if (direction-blocked? m (sprite-last-direction m))
             (first-unblocked-direction m)
             (sprite-last-direction m)))
       (sprite-direction m)
       (sprite-speed m)))))
  
  ;;-------------------------------------------------------------------
  ;; sprite health
  
  ;; player-was-eaten? :: sprite -> (sprite) -> Boolean
  (define (player-was-eaten? player mummies)
    (let ((player-bb (sprite->bounding-box player)))
      (findf
        (lambda (m) (collided? player-bb (sprite->bounding-box m)))
        mummies)))

  ;; no-lives-remaining :: w -> Boolean
  (define (no-lives-remaining w)
    (<= (world-lives w) 0))
  
  ;; update-world :: world -> world
  (define (update-world w)
    (let* ((lives   (world-lives w))
           (crypts  (world-crypts w))
           (player  (move-sprite (world-p w)))
           (mummies (map (lambda (m) (mummy-follows-player m player))
                         (world-mummies w))))
      (make-world
        (world-score w)
        (if (player-was-eaten? player mummies) (- lives 1) lives)
        player
        mummies
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
          (world-lives w)
          (make-sprite x y 
            (if (direction-blocked? p d)
              (sprite-direction p)
              d)
            (sprite-last-direction p)
            PLAYER-SPEED)
          (world-mummies w)
          (world-crypts w))
        w)))
  
  ;;-------------------------------------------------------------------
  ;; Scenes and layers

  ;; empty-scene :: image
  (define empty-scene (rectangle SCREEN-WIDTH SCREEN-HEIGHT 'solid 'yellow))

  ;; background-layer :: image
  (define background-layer
    (underlay/xy
      empty-scene
      GRID-LEFT-MARGIN GRID-TOP-MARGIN
      (rectangle GRID-WIDTH GRID-HEIGHT 'solid 'black)))

  ;; mummy-layer :: image
  (define mummy-layer
    (bitmap "mummy-forward.png"))
  
  ;; player-layer :: image
  (define player-layer
    (bitmap "player-forward.png"))

  ;; crypt-layer :: image
  (define crypt-layer
    (rectangle
      (size-height crypt-size)
      (size-width crypt-size)
      'solid 'gray))

  ;; scroll-layer :: image
  (define scroll-layer empty-scene)

  ;; key-layer :: image
  (define key-layer empty-scene)

  ;; king-mummy-layer :: image
  (define king-mummy-layer empty-scene)

  ;; treasure-tomb-layer :: image
  (define treasure-tomb-layer empty-scene)


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
      (text "Score " 24 "black")
      50
      20
      (place-image
        (text (number->string score) 24 "black")
        90
        20
        scene)))

  ;; render-lives :: Number -> scene -> image
  (define (render-lives lives scene)
    (place-image
      (text "Lives" 24 "black")
      (- SCREEN-WIDTH 60)
      20
      (place-image
        (text (number->string lives) 24 "black")
        (- SCREEN-WIDTH 20)
        20
        scene)))

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
  ;; initial states

  ;; initial-mummies :: (sprite)
  (define (initial-mummies)
    (list (make-sprite 30 300 "up" STUCK MUMMY-SPEED)
          (make-sprite 270 300 "down" STUCK MUMMY-SPEED)))

  ;; initial-crypts :: (crypt)
  (define (initial-crypts)
    (list
     (make-crypt 90 105 #f (make-object 'key))
     (make-crypt 210 105 #f (make-object 'key))
     (make-crypt 330 105 #f (make-object 'key))
     (make-crypt 450 105 #f (make-object 'key))
     (make-crypt 570 105 #f (make-object 'key))

     (make-crypt 90 195 #f (make-object 'key))
     (make-crypt 210 195 #f (make-object 'key))
     (make-crypt 330 195 #f (make-object 'key))
     (make-crypt 450 195 #f (make-object 'key))
     (make-crypt 570 195 #f (make-object 'key))

     (make-crypt 90 285 #f (make-object 'key))
     (make-crypt 210 285 #f (make-object 'key))
     (make-crypt 330 285 #f (make-object 'key))
     (make-crypt 450 285 #f (make-object 'key))
     (make-crypt 570 285 #f (make-object 'key))

     (make-crypt 90 375 #f (make-object 'key))
     (make-crypt 210 375 #f (make-object 'key))
     (make-crypt 330 375 #f (make-object 'key))
     (make-crypt 450 375 #f (make-object 'key))
     (make-crypt 570 375 #f (make-object 'key))))

  ;; initial-world :: world
  (define (initial-world)
    (make-world
      0
      PLAYER-STARTING-LIVES
      (make-sprite (posn-x ENTRANCE-POS) (posn-y ENTRANCE-POS) STUCK STUCK PLAYER-SPEED)
      (initial-mummies)
      (initial-crypts)))
  
  
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