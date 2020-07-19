;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.3.219) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;;;;;
;;;;;
; to do
; abstract the x,y segment calculation into a function
; (+ (* 10 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
; (+ (* 10 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))

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

(define FOOD1 (make-posn 2 2))

(define-struct ws [state worm food])
; A WorldState is a structure
; (make-ws State Worm Posn)

; State is one of:
;  - start
;  - play
;  - stop-wall
;  - stop-self

(define WS1 (make-ws "play" WORM1 FOOD1))
(define WS2 (make-ws "play" WORM2 FOOD1))
(define WS3 (make-ws "play" WORM3 FOOD1))
(define WS4 (make-ws "play" WORM4 FOOD1))
(define WS5 (make-ws "play" WORM5 FOOD1))
(define WS6 (make-ws "stop-wall" WORM6 FOOD1))
(define WS7 (make-ws "play" WORM7 FOOD1))


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
(check-expect (render WS1)
              (draw-food FOOD1
                         (draw-worm (worm-lop WORM1))))
(check-expect (render WS4)
              (draw-food FOOD1
                         (draw-worm (worm-lop WORM4))))
(check-expect (render WS6) (overlay/align "right" "bottom"
                                          (text "worm hit border" 15 "black")
                                          (draw-food FOOD1
                                                     (draw-worm (worm-lop WORM6)))))
(check-expect (render (make-ws "play"
                               (make-worm (list (make-posn 2 3)) "up")
                               (make-posn 4 4)))
              (place-image
               FOOD
               (+ (* 4 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (+ (* 4 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (place-image
                SEGMENT
                (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                (+ (* 3 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                BG)))

(check-expect (render (make-ws
                       "play"
                       (make-worm (list (make-posn 2 2) (make-posn 3 2) (make-posn 4 2) (make-posn 5 2)) "left")
                       (make-posn 2 3)))
              (place-image
               FOOD
               (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (+ (* 3 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (place-image
                SEGMENT
                (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                (place-image
                 SEGMENT
                 (+ (* 3 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                 (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                 (place-image
                  SEGMENT
                  (+ (* 4 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                  (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                  (place-image
                   SEGMENT
                   (+ (* 5 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                   (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
                   BG))))))
              
; (define (render ws) empty-image) ;stub

(define (render ws)
  (cond
    [(string=? (ws-state ws) "stop-wall")
     (overlay/align "right" "bottom"
                    (text "worm hit border" 15 "black")
                    (draw-food (ws-food ws)
                               (draw-worm (worm-lop (ws-worm ws)))))]
    [(string=? (ws-state ws) "stop-self")
     (overlay/align "right" "bottom"
                    (text "worm hit itself" 15 "black")
                    (draw-worm (worm-lop (ws-worm ws))))]
    [else
     (draw-food (ws-food ws)
                (draw-worm (worm-lop (ws-worm ws))))]))

; WorldState -> WorldState
; handle the updates of the game
(check-random (tock
               (make-ws "play"
                        (make-worm (list (make-posn 2 2) (make-posn 2 3)) "")
                        FOOD1))
              (make-ws "play"
                       (make-worm (list (make-posn 2 2) (make-posn 2 3)) "")
                       (spawn-food
                        (make-worm (list (make-posn 2 2) (make-posn 2 3)) "")
                        HEIGHT WIDTH)))
#;(check-random (tock
                 (make-ws "play"
                          (make-worm (list (make-posn 2 2)) "up")
                          FOOD1))
                (make-ws "play"
                         (make-worm (list (make-posn 2 1) (make-posn 2 2)) "up")
                         (spawn-food
                          (make-worm (list (make-posn 2 1) (make-posn 2 2)) "up")
                          HEIGHT WIDTH)))
#;(check-random (tock
                 (make-ws "play"
                          (make-worm (list (make-posn 2 2) (make-posn 2 3)) "up")
                          FOOD1))
                (make-ws "play"
                         (make-worm (list (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)) "up")
                         (spawn-food
                          (make-worm (list (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)) "up")
                          HEIGHT WIDTH)))
(check-expect (tock
               (make-ws "play"
                        (make-worm (list (make-posn 0 0)) "up")
                        FOOD1))
              (make-ws "stop-wall"
                       (make-worm (list (make-posn 0 -1)) "up")
                       FOOD1))
(check-expect (tock WS7)
              (make-ws "stop-wall"
                       (make-worm (list (make-posn 1 -1)) "right")
                       FOOD1))
(check-expect (tock
               (make-ws "play"
                        (make-worm (list (make-posn 10 9)) "right")
                        FOOD1))
              (make-ws "stop-wall"
                       (make-worm (list (make-posn 11 9)) "right")
                       FOOD1))
(check-random (tock
               (make-ws "play"
                        (make-worm (list (make-posn 2 2)) "up")
                        (make-posn 2 2)))
              (make-ws "play"
                       (make-worm (list (make-posn 2 1) (make-posn 2 2)) "up")
                       (spawn-food
                        (make-worm (list (make-posn 2 1) (make-posn 2 2)) "up")
                        HEIGHT WIDTH)))
(check-random (tock
               (make-ws "play"
                        (make-worm (list (make-posn 1 1)) "up")
                        (make-posn 1 1)))
              (make-ws "play"
                       (make-worm (list (make-posn 1 0) (make-posn 1 1)) "up")
                       (spawn-food
                        (make-worm (list (make-posn 1 0) (make-posn 1 0)) "up")
                        HEIGHT WIDTH)))

(define (tock ws)
  (make-ws
   (cond
     [(worm-collided-wall? (move-worm (ws-worm ws) (ws-food ws)) HEIGHT WIDTH) "stop-wall"]
     [(worm-collided-itself? (move-worm (ws-worm ws) (ws-food ws))) "stop-self"]
     [else "play"])
   (move-worm (ws-worm ws) (ws-food ws))
   (if (worm-collided-food? (ws-worm ws) (ws-food ws))
       (spawn-food (ws-worm ws) HEIGHT WIDTH)
       (ws-food ws))))

; Worm -> Worm
; move the segments of the worm in the provided direction
(check-expect (move-worm (make-worm (list (make-posn 2 2)) "up") (make-posn 3 3))
              (make-worm (list (make-posn 2 1)) "up"))
(check-expect (move-worm (make-worm (list (make-posn 2 2)) "") (make-posn 3 4))
              (make-worm (list (make-posn 2 2)) ""))
(check-expect (move-worm (make-worm (list (make-posn 2 2) (make-posn 3 2)) "down") (make-posn 4 4))
              (make-worm (list (make-posn 2 3) (make-posn 2 2)) "down"))
(check-expect (move-worm (make-worm (list (make-posn 9 9) (make-posn 8 9)) "right") (make-posn 4 4))
              (make-worm (list (make-posn 10 9) (make-posn 9 9)) "right"))
(check-expect (move-worm (make-worm (list (make-posn 2 2)) "up") (make-posn 2 2))
              (make-worm (list (make-posn 2 1) (make-posn 2 2)) "up"))


(define (move-worm w f)
  (if (worm-collided-food? w f)
      (add-head w)
      (drop-tail (add-head w))))

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

; Worm Number Number -> Posn
; spawn a new food in the random place on the map but not where the worm's head is
#;(check-satisfied (spawn-food
                    (make-worm (list (make-posn 0 0) (make-posn 0 1)) "up")
                    2 2)
                   not-0-0-and-0-1?)

(define (spawn-food worm h w)
  (spawn-food-check worm (random (+ h 1)) (random (+ w 1))))

; Worm Number Number -> Posn
; check if a food coordinate is colliding with a list of worm segment coordinates
(check-expect (spawn-food-check (make-worm
                                 (list (make-posn 0 0) (make-posn 0 1) (make-posn 1 0))
                                 "up")
                                2 2)
              (make-posn 2 2))
(check-expect (spawn-food-check (make-worm
                                 (list (make-posn 0 0) (make-posn 0 1))
                                 "up")
                                2 2)
              (make-posn 2 2))

(define (spawn-food-check worm x y)
  (if (member? (make-posn x y) (worm-lop worm))
      (spawn-food worm x y)
      (make-posn x y)))


;; Posn -> Boolean
;; for testing only
;; check if the result is not of one of the positions
(check-expect (not-0-0-and-0-1?  (make-posn 0 0)) #false)
(check-expect (not-0-0-and-0-1? (make-posn 0 1)) #false)
(check-expect (not-0-0-and-0-1? (make-posn 1 1)) #true)

(define (not-0-0-and-0-1? p)
  (and
   (not (and (= (posn-x p) 0) (= (posn-y p) 0)))
   (not (and (= (posn-x p) 0) (= (posn-y p) 1)))))


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

; Posn -> Image
; draw food on a given image
(check-expect (draw-food (make-posn 2 2) BG)
              (place-image
               FOOD
               (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               (+ (* 2 SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
               BG))
               
(define (draw-food f img)
  (place-image
   FOOD
   (+ (* (posn-x f) SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
   (+ (* (posn-y f) SEGMENT-SIZE) (/ SEGMENT-SIZE 2))
   img))

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

; Worm Posn -> Bool
; check if worm has coffided with the food
(check-expect (worm-collided-food?
               (make-worm (list (make-posn 2 2)) "up")
               (make-posn 2 3))
              #false)
(check-expect (worm-collided-food?
               (make-worm (list (make-posn 2 2)) "up")
               (make-posn 2 2))
              #true)

(define (worm-collided-food? w f)
  (member? f (worm-lop w)))

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
       "up")
      (ws-food ws))]
    [(and
      (key=? a-key "down")
      (not (string=? (worm-direction (ws-worm ws)) "up")))
     (make-ws
      "play"
      (make-worm
       (worm-lop (ws-worm ws))
       "down")
      (ws-food ws))]
    [(and
      (key=? a-key "left")
      (not (string=? (worm-direction (ws-worm ws)) "right")))
     (make-ws
      "play"
      (make-worm
       (worm-lop (ws-worm ws))
       "left")
      (ws-food ws))]
    [(and
      (key=? a-key "right")
      (not (string=? (worm-direction (ws-worm ws)) "left")))
     (make-ws
      "play"
      (make-worm
       (worm-lop (ws-worm ws))
       "right")
      (ws-food ws))]
    [else ws]))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-key change]
    [on-tick tock 0.3]
    [stop-when end-game? render]
    ;[state #t]
    ))

(main (make-ws "play"
               (make-worm (list
                           (make-posn 2 1)) "")
               (make-posn 0 0)))