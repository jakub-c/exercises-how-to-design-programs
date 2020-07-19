;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 4.5-sample-problem) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 400)
(define HEIGHT 400)
(define empt-sc (rectangle WIDTH HEIGHT "solid" "transparent"))
(define VELOCITY 3)

(define ROCKET (above (triangle 20 "solid" "red")
                      (beside/align "bottom"
                                    (rectangle 10 30 "solid" "green")
                                    (rectangle 20 40 "solid" "blue")
                                    (rectangle 10 30 "solid" "green"))))

(define ROCKET-BOTTOM-POS-Y (- HEIGHT (/ (image-height ROCKET) 2)))

;; RCO (rocket countdown) is one of
;; - "resting"
;; - -3, -2, -1
;; a NonNegativeNumber
;; interpretation a grounded rocket, a countdown,
;; a number that denotes the distnace from the
;; top of the screen to middle of the rocket image

(define (rco-template x)
  (cond
    [(string? x) ...]
    [(<= -3 x -1) ...]
    [(>= x 0) ...]))

;; Number Number -> Image
;; draw the image of the rocket on the screen based on x, y coordinates
(define (draw-rocket x y)
  (place-image
   ROCKET x y empt-sc))
  
;; RCO -> Image
;; draw a racket and countdown text
(check-expect (draw "resting") (draw-rocket
                                (/ WIDTH 2)
                                ROCKET-BOTTOM-POS-Y))

(check-expect (draw -2) (place-image
                         (text "-2" 36 "black")
                         (/ WIDTH 2)
                         (/ HEIGHT 2)
                         (draw-rocket
                          (/ WIDTH 2)
                          ROCKET-BOTTOM-POS-Y)))

(check-expect (draw -1) (place-image
                         (text "-1" 36 "black")
                         (/ WIDTH 2)
                         (/ HEIGHT 2)
                         (draw-rocket
                          (/ WIDTH 2)
                          ROCKET-BOTTOM-POS-Y)))

(check-expect (draw (/ HEIGHT 2)) (draw-rocket
                                   (/ WIDTH 2)
                                   (/ HEIGHT 2)))

(check-expect (draw 40) (draw-rocket
                         (/ WIDTH 2)
                         40))

; (define (draw x) empt-sc) ; stub

(define (draw x)
  (cond
    [(string? x)
     (draw-rocket
      (/ WIDTH 2)
       ROCKET-BOTTOM-POS-Y)]
    [(<= -3 x -1) (place-image
                   (text (number->string x) 36 "black")
                   (/ WIDTH 2)
                   (/ HEIGHT 2)
                   (draw-rocket
                    (/ WIDTH 2)
                    ROCKET-BOTTOM-POS-Y))]
    [(>= x 0) (draw-rocket
               (/ WIDTH 2)
               x)]))


; RCO -> RCO
; advances to the next step of the rocket, from "resting" to countdown to flying
(check-expect (tick "resting") "resting")
(check-expect (tick -3) -2)
(check-expect (tick -2) -1)
(check-expect (tick -1) ROCKET-BOTTOM-POS-Y)
(check-expect (tick 375) (- 375 VELOCITY))

; (define (tick x) 4) ;stub

(define (tick x)
  (cond
    [(string? x)
     (if (string=? x "resting")
         "resting"
         true)]
    [(<= -3 x -1)
     (if (<= -3 x -2)
         (+ x 1)
         ROCKET-BOTTOM-POS-Y)]
    [(>= x 0) (- x VELOCITY)]))

;; RCO KeyPress -> RCO
;; starts the countdown when spacebar is pressed
(check-expect (start "resting" " ") -3)
(check-expect (start "resting" "a") "resting")
(check-expect (start -2 " ") -2)

; (define (start x key) 10) ;stub

(define (start x key)
  (cond
    [(string? x) (if (string=? key " ") -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))


; RCO -> RCO
(define (main s)
  (big-bang s
    [to-draw draw]
    [on-tick tick]
    [on-key start]))

(main "resting")