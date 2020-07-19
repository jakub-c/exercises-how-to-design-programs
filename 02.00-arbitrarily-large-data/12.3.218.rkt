;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.3.218) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SEGMENT-SIZE 20)
(define SEGMENT (circle (/ SEGMENT-SIZE 2) "solid" "red"))

(define FOOD (circle (/ SEGMENT-SIZE 2) "solid" "green"))

(define HEIGHT 11)
(define WIDTH 11)

(define HEIGHT-PX  (* SEGMENT-SIZE HEIGHT))
(define WIDTH-PX  (* SEGMENT-SIZE WIDTH))
(define BG (rectangle HEIGHT-PX WIDTH-PX "solid" "white"))

(define-struct worm [lop dir])
; A Worm is a structure
; (make-worm List-of-Posn Dir)

; List-of-Posn is one of:
;  - '()
;  - (cons Posn List-of-Posn)

; Dir is one of:
;  - ""
;  - up
;  - down
;  - left
;  - right

(define WORM1 (make-worm
               (list (make-posn 0 0)) ""))
(define WORM2 (make-worm
               (list (make-posn 0 0)) "down"))
(define WORM3 (make-worm
               (list (make-posn 0 0)) "up"))
(define WORM4 (make-worm
               (list (make-posn 10 10)) "left"))
(define WORM5 (make-worm
               (list (make-posn 10 10)) "right"))
(define WORM6 (make-worm
               (list (make-posn -1 0)) "right"))
(define WORM7 (make-worm
               (list (make-posn 0 -1)) "right"))
(define WORM8 (make-worm
               (list (make-posn 9 9)) "left"))
(define WORM9 (make-worm
               (list (make-posn 0 0) (make-posn 10 10)) ""))

(define-struct ws [state worm])
; A WorldState is a structure
; (make-ws State Worm)

; State is one of:
;  - start
;  - play
;  - stop-wall
;  - stop-self

(define WS1 (make-ws "play" WORM1))
(define WS2 (make-ws "play" WORM2))
(define WS3 (make-ws "play" WORM3))
(define WS4 (make-ws "play" WORM4))
(define WS5 (make-ws "play" WORM5))
(define WS6 (make-ws "stop-wall" WORM6))
(define WS7 (make-ws "play" WORM7))


; Worm -> Number
; get the x coordinate of the worm from WorldState
(check-expect (worm-x (first (worm-lop WORM1))) 0)
(check-expect (worm-x (first (worm-lop WORM4))) 10)

;(define (worm-x ws) 0) ;stub
(define (worm-x w)
  (posn-x w))

; Worm -> Number
; get the y coordinate of the worm from WorldState
(check-expect (worm-y (first (worm-lop WORM1))) 0)
(check-expect (worm-y (first (worm-lop WORM4))) 10)

;(define (worm-x ws) 0) ;stub
(define (worm-y w)
  (posn-y w))

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
(check-expect (render WS1) (draw-worm (worm-lop WORM1)))
(check-expect (render WS4) (draw-worm (worm-lop WORM4)))
(check-expect (render WS6) (overlay/align "right" "bottom"
                                          (text "worm hit border" 15 "black")
                                          (draw-worm (worm-lop WORM6))))
; (define (render ws) empty-image) ;stub

(define (render ws)
  (cond
    [(string=? (ws-state ws) "stop-wall")
      (overlay/align "right" "bottom"
                     (text "worm hit border" 15 "black")
                     (draw-worm (worm-lop (ws-worm ws))))]
    [(string=? (ws-state ws) "stop-self")
      (overlay/align "right" "bottom"
                     (text "worm hit itself" 15 "black")
                     (draw-worm (worm-lop (ws-worm ws))))]
    [else
      (draw-worm (worm-lop (ws-worm ws)))]))

; WorldState -> WorldState
; handle the updates of the game
(check-expect (tock
               (make-ws "play"
                        (make-worm (list (make-posn 2 2) (make-posn 2 3)) "")))
              (make-ws "play"
                       (make-worm (list (make-posn 2 2) (make-posn 2 3)) "")))
(check-expect (tock
               (make-ws "play"
                        (make-worm (list (make-posn 2 2)) "up")))
              (make-ws "play"
                       (make-worm (list (make-posn 2 1)) "up")))
(check-expect (tock
               (make-ws "play"
                        (make-worm (list (make-posn 2 2) (make-posn 2 3)) "up")))
              (make-ws "play"
                       (make-worm (list (make-posn 2 1) (make-posn 2 2)) "up")))
(check-expect (tock
               (make-ws "play"
                        (make-worm (list (make-posn 0 0)) "up")))
              (make-ws "stop-wall"
                       (make-worm (list (make-posn 0 -1)) "up")))
(check-expect (tock WS7)
              (make-ws "stop-wall"
                       (make-worm (list (make-posn 1 -1)) "right")))
(check-expect (tock
               (make-ws "play"
                        (make-worm (list (make-posn 10 9)) "right")))
              (make-ws "stop-wall"
                       (make-worm (list (make-posn 11 9)) "right")))

(define (tock ws)
  (make-ws
   (cond
     [(worm-collided-wall? (move-worm (ws-worm ws)) HEIGHT WIDTH) "stop-wall"]
     [(worm-collided-itself? (move-worm (ws-worm ws))) "stop-self"]
     [else "play"])
   (move-worm (ws-worm ws))))

; Worm -> Worm
; move the segments of the worm in the provided direction
(check-expect (move-worm (make-worm (list (make-posn 2 2)) "up"))
              (make-worm (list (make-posn 2 1)) "up"))
(check-expect (move-worm (make-worm (list (make-posn 2 2)) ""))
              (make-worm (list (make-posn 2 2)) ""))
(check-expect (move-worm (make-worm (list (make-posn 2 2) (make-posn 3 2)) "down"))
              (make-worm (list (make-posn 2 3) (make-posn 2 2)) "down"))
(check-expect (move-worm (make-worm (list (make-posn 9 9) (make-posn 8 9)) "right"))
              (make-worm (list (make-posn 10 9) (make-posn 9 9)) "right"))


(define (move-worm w)
  (drop-tail (add-head w)))

; Worm - Worm
; remove the last element from the List-of-positions of worm's segements
(check-expect (drop-tail (make-worm (list (make-posn 2 3) (make-posn 2 4)) ""))
              (make-worm (list (make-posn 2 3) (make-posn 2 4)) ""))
(check-expect (drop-tail (make-worm (list (make-posn 3 4) (make-posn 4 4)) "up"))
              (make-worm (list (make-posn 3 4)) "up"))
(check-expect (drop-tail (make-worm (list (make-posn 2 3)) ""))
              (make-worm (list (make-posn 2 3)) ""))
(check-expect (drop-tail WORM9) (make-worm (list (make-posn 0 0) (make-posn 10 10)) ""))

(define (drop-tail w)
  (cond
    [(empty? (worm-lop w)) w]
    [(string=? (worm-dir w) "") w]
    [else
     (make-worm
      (reverse (rest (reverse (worm-lop w))))
      (worm-dir w))]))

; Worm -> Worm
; add a segment to the begginig of the worm
(check-expect (add-head WORM1)
              (make-worm (list (make-posn 0 0)) ""))
(check-expect (add-head WORM2)
              (make-worm (list (make-posn 0 1) (make-posn 0 0)) "down"))
(check-expect (add-head WORM3)
              (make-worm (list (make-posn 0 -1) (make-posn 0 0)) "up"))
(check-expect (add-head WORM4)
              (make-worm (list (make-posn 9 10) (make-posn 10 10)) "left"))
(check-expect (add-head WORM5)
              (make-worm (list (make-posn 11 10) (make-posn 10 10)) "right"))

;(define (add-head ws) ws) ;stub

(define (add-head w)
  (make-worm
   (cond
     [(string=? "down" (worm-direction w))
      (cons (add-segment-below (first (worm-lop w)))
            (worm-lop w))]
     [(string=? "up" (worm-direction w))
      (cons (add-segment-above (first (worm-lop w)))
            (worm-lop w))]
     [(string=? "left" (worm-direction w))
      (cons (add-segment-left (first (worm-lop w)))
            (worm-lop w))]
     [(string=? "right" (worm-direction w))
      (cons (add-segment-right (first (worm-lop w)))
            (worm-lop w))]
     [else
      (worm-lop w)])
   (worm-direction w)))

; Worm -> Worm
(check-expect (add-segment-below (make-posn 5 5))
              (make-posn 5 6))
(check-expect (add-segment-below (make-posn 0 0))
              (make-posn 0 1))

(define (add-segment-below position)
  (make-posn
   (posn-x position)
   (+ (posn-y position) 1)))

; Worm -> Worm
(check-expect (add-segment-above (make-posn 5 5))
              (make-posn 5 4))
(check-expect (add-segment-above (make-posn 0 0))
              (make-posn 0 -1))

(define (add-segment-above position)
  (make-posn
   (posn-x position)
   (- (posn-y position) 1)))

; Worm -> Worm
(check-expect (add-segment-left (make-posn 5 5))
              (make-posn 4 5))
(check-expect (add-segment-left (make-posn 0 0))
              (make-posn -1 0))

(define (add-segment-left position)
  (make-posn
   (- (posn-x position) 1)
   (posn-y position)))

; Worm -> Worm
(check-expect (add-segment-right (make-posn 5 5))
              (make-posn 6 5))
(check-expect (add-segment-right (make-posn 0 0))
              (make-posn 1 0))

(define (add-segment-right position)
  (make-posn
   (+ (posn-x position) 1)
   (posn-y position)))
  

; List-of-Posn -> Image
; draw a worm from a list of segments
(check-expect (draw-worm (worm-lop WORM1))
              (place-image
               SEGMENT
               (+ (* 0 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (+ (* 0 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               BG))

(check-expect (draw-worm (worm-lop WORM4))
              (place-image
               SEGMENT
               (+ (* 10 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (+ (* 10 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               BG))

(check-expect (draw-worm (worm-lop WORM9))
              (place-image
               SEGMENT
               (+ (* 0 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (+ (* 0 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (place-image
                SEGMENT
                (+ (* 10 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                (+ (* 10 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                BG)))

(define (draw-worm lop)
  (cond
    [(empty? lop) BG]
    [else
     (place-image
      SEGMENT
      (+ (* (posn-x (first lop)) SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
      (+ (* (posn-y (first lop)) SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
      (draw-worm (rest lop)))]))

; Worm Number Number -> Boolean
; return #true when the worm has run through the wall
; the area size is provided by HEIGHT and WIDTH parameters
(check-expect (worm-collided-wall? WORM1 10 10) #false)
(check-expect (worm-collided-wall? WORM6 10 10) #true)
(check-expect (worm-collided-wall? WORM7 10 10) #true)
(check-expect (worm-collided-wall? WORM4 10 10) #true)
(check-expect (worm-collided-wall? WORM8 10 10) #false)
(check-expect (worm-collided-wall?
               (make-worm (list (make-posn 0 -1)) "up") 10 10) #true)
(check-expect (worm-collided-wall?
               (make-worm (list (make-posn 10 10)) "up") 10 10) #true)
(check-expect (worm-collided-wall?
               (make-worm (list (make-posn 11 9)) "right") 11 11) #true)


; (define (worm-collided-wall? worm h w) #true) ;stub
(define (worm-collided-wall? worm h w)
  (or
   (>= (posn-x (first (worm-lop worm))) w)
   (<  (posn-x (first (worm-lop worm))) 0)
   (>= (posn-y (first (worm-lop worm))) h)
   (<  (posn-y (first (worm-lop worm))) 0)))

; Worm -> Worm
; check if the first segment of the worm collided with the body of the worm
(check-expect (worm-collided-itself? (make-worm (list (make-posn 0 0)) ""))
              #false)
(check-expect (worm-collided-itself? (make-worm (list
                                                 (make-posn 4 4)
                                                 (make-posn 4 5)
                                                 (make-posn 5 5)
                                                 (make-posn 5 4)
                                                 (make-posn 4 4)) ""))
              #true)

(define (worm-collided-itself? w)
  (member? (first (worm-lop w))
           (rest (worm-lop w))))

; WorldState -> Boolean
(check-expect (end-game? WS4) #false)

(define (end-game? ws)
  (or
   (worm-collided-wall? (ws-worm ws) WIDTH HEIGHT)
   (worm-collided-itself? (ws-worm ws))))

; WorldState -> WorldState
(define (change ws a-key)
  (cond
    [(and
      (key=? a-key "up")
      (not (string=? (worm-direction (ws-worm ws)) "down")))
     (make-ws
      "play"
      (make-worm
       (worm-lop (ws-worm ws))
       "up"))]
    [(and
      (key=? a-key "down")
      (not (string=? (worm-direction (ws-worm ws)) "up")))
     (make-ws
      "play"
      (make-worm
       (worm-lop (ws-worm ws))
       "down"))]
    [(and
      (key=? a-key "left")
      (not (string=? (worm-direction (ws-worm ws)) "right")))
     (make-ws
      "play"
      (make-worm
       (worm-lop (ws-worm ws))
       "left"))]
    [(and
      (key=? a-key "right")
      (not (string=? (worm-direction (ws-worm ws)) "left")))
     (make-ws
      "play"
      (make-worm
       (worm-lop (ws-worm ws))
       "right"))]
    [else ws]))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-key change]
    [on-tick tock 0.3]
    [stop-when end-game? render]
    ))

(main (make-ws "play"
               (make-worm (list
                           (make-posn 2 1)
                           (make-posn 2 2)
                           (make-posn 2 3)
                           (make-posn 2 4)
                           (make-posn 2 5)) "")))