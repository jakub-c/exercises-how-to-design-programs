;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.3.215) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SEGMENT-SIZE 20)
(define SEGMENT (circle (/ SEGMENT-SIZE 2) "solid" "red"))

(define HEIGHT (* SEGMENT-SIZE 5))
(define WIDTH (* SEGMENT-SIZE 5))
(define BG (rectangle HEIGHT WIDTH "solid" "white"))

; Number Number -> Image
; draw a worm based on the given coordinates
(check-expect (draw-worm 0 0)
              (place-image
               SEGMENT
               (+ (* 0 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (+ (* 0 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               BG))

(check-expect (draw-worm 10 15)
              (place-image
               SEGMENT
               (+ (* 10 SEGMENT-SIZE) SEGMENT-SIZE)
               (+ (* 15 SEGMENT-SIZE) SEGMENT-SIZE)
               BG))

(define (draw-worm x y)
  (place-image
   SEGMENT
   (+ (* x SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
   (+ (* y SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
   BG))


(define-struct ws [worm dir])
; A WorldState is a structure
; (make-ws Posn Dir)

(define WS1 (make-ws (make-posn 0 0) ""))
(define WS2 (make-ws (make-posn 0 0) "down"))
(define WS3 (make-ws (make-posn 0 0) "up"))
(define WS4 (make-ws (make-posn 10 10) "left"))
(define WS4 (make-ws (make-posn 10 10) "right"))

; Dir is one of:
;  - ""
;  - up
;  - down
;  - left
;  - right

; WorldState -> Number
; get the x coordinate of the worm from WorldState
(check-expect (worm-x WS1) 0)
(check-expect (worm-x WS4) 10)

;(define (worm-x ws) 0) ;stub
(define (worm-x ws)
  (posn-x (ws-worm ws)))

; WorldState -> Number
; get the y coordinate of the worm from WorldState
(check-expect (worm-y WS1) 0)
(check-expect (worm-y WS4) 10)

;(define (worm-x ws) 0) ;stub
(define (worm-y ws)
  (posn-y (ws-worm ws)))

; WorldState -> Image
; when needed, big-bang obtains the image of the current 
; state of the world by evaluating (render cw)
(check-expect (render WS1) (draw-worm 0 0))
; (define (render ws) empty-image) ;stub

(define (render ws)
  (draw-worm (posn-x (ws-worm ws)) (posn-y (ws-worm ws))))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [on-tick move-worm 1]
    [to-draw render]))

; (main (make-ws (make-posn 0 0) ""))

; WorldState -> Posn
(check-expect (move-worm WS1) (make-ws (make-posn 0 0) ""))
(check-expect (move-worm WS2) (make-ws (make-posn 0 1) "down"))
(check-expect (move-worm WS3) (make-ws (make-posn 0 -1) "up"))
(check-expect (move-worm WS4) (make-ws (make-posn 11 10) "left"))
(check-expect (move-worm WS4) (make-ws (make-posn 11 10) "right"))

;(define (move-worm ws) ws) ;stub

(define (move-worm ws)
  (make-ws
   (cond
     [(string=? "down" (ws-dir ws))
      (make-posn (worm-x ws)
                 (+ (worm-y ws) 1))]
     [(string=? "up" (ws-dir ws))
      (make-posn (worm-x ws)
                 (- (worm-y ws) 1))]
     [(string=? "left" (ws-dir ws))
      (make-posn (+ (worm-x ws) 1)
                 (worm-y ws))]
     [(string=? "right" (ws-dir ws))
      (make-posn (- (worm-x ws) 1)
                 (worm-y ws))]
     [else (ws-worm ws)])
   (ws-dir ws)))