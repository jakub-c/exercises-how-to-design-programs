;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 478-483) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ==================== Exercise 478 ====================

; Q: You can also place the first queen in all squares of the top-most row,
;   the right-most column, and the bottom-most row.
;   Explain why all of these solutions are just like the three
;   scenarios depicted in figure 173.

; A: Because every time you stick a figure by the wall of the
;    board, it will always leave at least 2 empty spaces.


; Q: Is it possible to place even a second queen after you place one
;    on the central square of a 3 by 3 board?
; A: No - this will threaten all the rest of the spaces.

; =================== End of exercise ==================

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

; ==================== Exercise 479 ====================

; QP QP -> Boolean
; determine whether queens placed on the two respective
; squares would threaten each other
; (define (threatening? q1 q2) #false) ;stub

(check-expect (threatening?
               (make-posn 0 0)
               (make-posn 7 7))
              #true)
(check-expect (threatening?
               (make-posn 0 3)
               (make-posn 2 1))
              #true)
(check-expect (threatening?
               (make-posn 1 1)
               (make-posn 1 6))
              #true)
(check-expect (threatening?
               (make-posn 1 2)
               (make-posn 7 2))
              #true)
(check-expect (threatening?
               (make-posn 1 2)
               (make-posn 2 0))
              #false)
(check-expect (threatening?
               (make-posn 1 2)
               (make-posn 3 5))
              #false)

(define (threatening? q1 q2)
  (local ((define pieces-on-x (= (posn-x q1) (posn-x q2)))
          (define pieces-on-y (= (posn-y q1) (posn-y q2)))
          (define pieces-on-/-diagonal (= (- (posn-x q1) (posn-y q1))
                                          (- (posn-x q2) (posn-y q2))))
          (define pieces-on-\-diagonal (= (+ (posn-x q1) (posn-y q1))
                                          (+ (posn-x q2) (posn-y q2)))))
    (or pieces-on-x
        pieces-on-y
        pieces-on-/-diagonal
        pieces-on-\-diagonal)))

; =================== End of exercise ==================

; ==================== Exercise 480 ====================

(define NSIZE 80)
(define QUEEN-FIG (star (/ NSIZE 2) "solid" "gray"))
(define SPACE (overlay
               (square NSIZE "solid" "white")
               (square NSIZE "outline" "black")))

; N [List-of QPs] Image -> Image
; produce an image of an n by n chess board with the
; given image placed according to the given QPs
; (define (render-queens n loq figure) empty-image) ;stub

(define q-input `(,(make-posn 0 0) ,(make-posn 1 1)))
(check-expect (render-queens 2 q-input QUEEN-FIG)
              (above
               (beside SPACE (overlay QUEEN-FIG SPACE))
               (beside (overlay QUEEN-FIG SPACE) SPACE)
               empty-image))

(define (render-queens n loq figure)
  (local ((define (draw-row y-pos)
            (foldr (lambda (index-x image)
                     (if (ormap (lambda (q) (and (= (posn-x q) index-x)
                                             (= (posn-y q) y-pos)))
                            loq)
                         (beside (overlay QUEEN-FIG SPACE) image)
                         (beside SPACE image)))
                   empty-image
                   (range 0 n 1)))
          (define draw-grid
            (foldl (lambda (index-y image)
                   (above (draw-row index-y) image))
                   empty-image
                   (range 0 n 1))))
    draw-grid))

; =================== End of exercise ==================