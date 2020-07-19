;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.3.216) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SEGMENT-SIZE 20)
(define SEGMENT (circle (/ SEGMENT-SIZE 2) "solid" "red"))

(define HEIGHT (* SEGMENT-SIZE 11))
(define WIDTH (* SEGMENT-SIZE 11))
(define BG (rectangle HEIGHT WIDTH "solid" "white"))

(define-struct worm [pos dir])
; A Worm is a structure
; (make-worm Posn Dir)

; Dir is one of:
;  - ""
;  - up
;  - down
;  - left
;  - right

(define WORM1 (make-worm (make-posn 0 0) ""))
(define WORM2 (make-worm (make-posn 0 0) "down"))
(define WORM3 (make-worm (make-posn 0 0) "up"))
(define WORM4 (make-worm (make-posn 10 10) "left"))
(define WORM5 (make-worm (make-posn 10 10) "right"))
(define WORM6 (make-worm (make-posn -1 0) "right"))
(define WORM7 (make-worm (make-posn 0 -1) "right"))
(define WORM8 (make-worm (make-posn 9 9) "left"))

(define-struct ws [state worm])
; A WorldState is a structure
; (make-ws State Worm)

; State is one of:
;  - start
;  - play
;  - stop

(define WS1 (make-ws "play" WORM1))
(define WS2 (make-ws "play" WORM2))
(define WS3 (make-ws "play" WORM3))
(define WS4 (make-ws "play" WORM4))
(define WS5 (make-ws "play" WORM5))
(define WS6 (make-ws "stop" WORM5))
(define WS7 (make-ws "play" WORM7))


; Worm -> Number
; get the x coordinate of the worm from WorldState
(check-expect (worm-x WORM1) 0)
(check-expect (worm-x WORM4) 10)

;(define (worm-x ws) 0) ;stub
(define (worm-x w)
  (posn-x (worm-pos w)))

; Worm -> Number
; get the y coordinate of the worm from WorldState
(check-expect (worm-y WORM1) 0)
(check-expect (worm-y WORM4) 10)

;(define (worm-x ws) 0) ;stub
(define (worm-y w)
  (posn-y (worm-pos w)))

; Worm -> String
; get the y coordinate of the worm from WorldState
(check-expect (worm-direction WORM1) "")
(check-expect (worm-direction WORM4) "left")

;(define (worm-direction ws) "") ;stub
(define (worm-direction w)
  (worm-dir w))

; WorldState -> Image
; when needed, big-bang obtains the image of the current 
; state of the world by evaluating (render cw)
(check-expect (render WS1) (draw-worm 0 0))
(check-expect (render WS4) (draw-worm 10 10))
(check-expect (render WS6) (overlay/align "right" "bottom"
                                          (text "worm hit border" 15 "black")
                                          (draw-worm 10 10)))
; (define (render ws) empty-image) ;stub

(define (render ws)
  (if (string=? (ws-state ws) "stop")
      (overlay/align "right" "bottom"
                     (text "worm hit border" 15 "black")
                     (draw-worm (worm-x (ws-worm ws)) (worm-y (ws-worm ws))))
      (draw-worm (worm-x (ws-worm ws)) (worm-y (ws-worm ws)))))

; WorldState -> WorldState
(check-expect (tock (make-ws
                     "play"
                     (make-worm (make-posn 0 0) "up")))
                    (make-ws "stop" (make-worm (make-posn 0 -1) "up")))
(check-expect (tock WS7) (make-ws "stop" (make-worm (make-posn 1 -1) "right")))

(define (tock ws)
  (make-ws
   (if (worm-colided? (move-worm (ws-worm ws)) HEIGHT WIDTH)
       "stop"
       "play")
   (move-worm (ws-worm ws))))

; WorldState -> Worm
(check-expect (move-worm WORM1) (make-worm (make-posn 0 0) ""))
(check-expect (move-worm WORM2) (make-worm (make-posn 0 1) "down"))
(check-expect (move-worm WORM3) (make-worm (make-posn 0 -1) "up"))
(check-expect (move-worm WORM4) (make-worm (make-posn 9 10) "left"))
(check-expect (move-worm WORM5) (make-worm (make-posn 11 10) "right"))

;(define (move-worm ws) ws) ;stub

(define (move-worm w)
  (make-worm
   (cond
     [(string=? "down" (worm-direction w))
      (make-posn (worm-x w)
                 (+ (worm-y w) 1))]
     [(string=? "up" (worm-direction w))
      (make-posn (worm-x w)
                 (- (worm-y w) 1))]
     [(string=? "left" (worm-direction w))
      (make-posn (- (worm-x w) 1)
                 (worm-y w))]
     [(string=? "right" (worm-direction w))
      (make-posn (+ (worm-x w) 1)
                 (worm-y w))]
     [else
      (make-posn (worm-x w)
                 (worm-y w))])
   (worm-direction w)))

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

; Worm Number Number -> Boolean
; return #true when the worm has run through the wall
; the area size is provided by HEIGHT and WIDTH parameters
(check-expect (worm-colided? WORM1 10 10) #false)
(check-expect (worm-colided? WORM6 10 10) #true)
(check-expect (worm-colided? WORM7 10 10) #true)
(check-expect (worm-colided? WORM4 10 10) #true)
(check-expect (worm-colided? WORM8 10 10) #false)
(check-expect (worm-colided?
               (make-worm (make-posn 0 -1) "up") 10 10) #true)


; (define (worm-colided? worm h w) #true) ;stub
(define (worm-colided? worm h w)
  (or
   (>= (worm-x worm) w)
   (< (worm-x worm) 0)
   (>= (worm-y worm) h)
   (< (worm-y worm) 0)))

; WorldState -> Boolean
(check-expect (end-game? WS4) #false)

(define (end-game? ws)
  (worm-colided? (ws-worm ws) WIDTH HEIGHT))

; WorldState -> WorldState
(define (change ws a-key)
  (cond
    [(key=? a-key "up") (make-ws
                         "play"
                         (make-worm
                          (worm-pos (ws-worm ws))
                          "up"))]
    [(key=? a-key "down") (make-ws
                           "play"
                           (make-worm
                            (worm-pos (ws-worm ws))
                            "down"))]
    [(key=? a-key "left") (make-ws
                           "play"
                           (make-worm
                            (worm-pos (ws-worm ws))
                            "left"))]
    [(key=? a-key "right") (make-ws
                            "play"
                            (make-worm
                             (worm-pos (ws-worm ws))
                             "right"))]
    [else ws]))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-key change]
    [on-tick tock 1]
    [stop-when end-game? render]))

;(main (make-ws "play" (make-worm (make-posn 1 1) "up")))