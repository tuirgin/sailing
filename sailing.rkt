#lang racket
(require 2htdp/universe 2htdp/image)

;; ----------------------------------------------------------------------------------------
;; CONSTANTS

(define W 960)                          ; scene width
(define H 548)                          ; scene height

;; Images

(define BACK (bitmap/file "./img/ocean-bottom_layer.png"))
(define FRONT (bitmap/file "./img/ocean-top_layer.png"))
(define SHIP (bitmap/file "./img/sailors.png"))
(define SCENE (place-image/align BACK 0 0 "left" "top" (empty-scene W H)))

;; Ship starting points

(define SHIP-X                          ; start off-screen
  (- (+ (image-width SHIP) 9)))
(define SHIP-Y 200)

;; Ship motion (heave and pitch)

(define MAX-Y 220)                      ; mx ship heave
(define MIN-Y 190)                      ; mn ship heave
(define MAX-PITCH 10)                   ; pitch; i.e. rotation

;; ----------------------------------------------------------------------------------------
;; STATE

(struct ship (pitch heave posx))

;; ----------------------------------------------------------------------------------------
;; FUNCTIONS

(define (next-state state)
  (ship (alter-motion (ship-pitch state) (- MAX-PITCH) MAX-PITCH)
        (alter-motion (ship-heave state) MIN-Y MAX-Y)
        (+ (ship-posx state) 3)))

;; TODO: Improve ship physics by adding previous value accumulators to state
(define (alter-motion motion mn mx)
  (define rand-change (random))
  (cond
    [(<= motion mn) (+ motion (* rand-change 6))] ; more abrupt changes at limits
    [(>= motion mx) (- motion (* rand-change 6))]
    [else (define rand-pos (random 10))
          (cond
            [(= rand-pos 0) (+ motion (* rand-change 3))] ; less abrupt otherwise
            [(= rand-pos 1) (- motion (* rand-change 3))]
            [else motion])]))

(define (draw-scene state)
  (place-image/align
   FRONT 0 H "left" "bottom"
   (place-image (rotate (ship-pitch state) SHIP)
                (ship-posx state)
                (ship-heave state)
                SCENE)))

(define (end? state)
  (>= (ship-posx state) (+ W (image-height SHIP) 9)))

;; ----------------------------------------------------------------------------------------
;; MAIN

(define (main)
  (big-bang (ship 0 SHIP-Y SHIP-X)
            (on-tick next-state)
            (to-draw draw-scene)
            (stop-when end?)))

(main)
