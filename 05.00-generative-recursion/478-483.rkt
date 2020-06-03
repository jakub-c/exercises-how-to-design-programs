;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 478-483) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/abstraction)
(require racket/list)

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

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem 
 
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
 
;(define (n-queens n) #false)
; (define (n-queens n) 4QUEEN-SOLUTION-2)

; ==================== Exercise 481 ====================

; N -> [[List-of QP] -> Boolean]
(define (n-queens-solution? n)
  (lambda (q)
    (cond [(empty? q) #false]
          [(<= n 3) #false]
          [(not (= (length q) n)) #false]
          [else
           (local ((define all-pairs-are-not-threatning
                     (andmap (lambda (x)
                               (andmap (lambda (y) (not (threatening? x y)))
                                       (remove x q)))
                             q)))
             all-pairs-are-not-threatning)])))

;(check-expect ((n-queens-solution? 3) '()))
#;(check-expect ((n-queens-solution? 4) '())
                #false)
(check-expect ((n-queens-solution? 4) 4QUEEN-SOLUTION-2)
              #true)
(check-expect ((n-queens-solution? 4) (list (make-posn 0 3) (make-posn 2 1) (make-posn 0 1) (make-posn 0 2)))
              #false)

; (check-satisfied (n-queens 4) (n-queens-solution? 4))

; [List-of QP] -> Boolean
; is the result equal [as a set] to one of two lists
#;(define (is-queens-result? x)
    (or (set=? 4QUEEN-SOLUTION-1 x)
        (set=? 4QUEEN-SOLUTION-2 x)))

; List List -> Boolean
; determine whether input lists contain
; the same items—regardless of order.
; (define (set=? s1 s2) #false) ;stub

(check-expect (set=? '(1 2 3) '(3 2 1)) #true)
(check-expect (set=? '(4 2 3) '(3 2 1)) #false)
(check-expect (set=? '() '(1)) #false)

(define (set=? s1 s2)
  (local ((define set-lengths-not-equal?
            (not (= (length s1) (length s2)))))
    (cond [set-lengths-not-equal? #false]
          [else
           (andmap (lambda (el)
                     (member? el s2))
                   s1)])))

; =================== End of exercise ==================

; ==================== Exercise 482 ====================

; N -> Board 
; creates the initial n by n board
#;(define (board0 n) ...)
 
; Board QP -> Board 
; places a queen at qp on a-board
#;(define (add-queen a-board qp)
    a-board)
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
#;(define (find-open-spots a-board)
    '())

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
#;(define (place-queens a-board n)
    #false)

#;(define (place-queens a-board n)
    (local ((define new-board-state
              (add-queen a-board (first (find-open-spots a-board)))))
      (cond [(= n 0) '()]
            [else
             (cons new-board-state
                   (place-queens new-board-state (- n 1)))])))

; =================== End of exercise ==================

; ==================== Exercise 483 ====================

; A Board is a [List-of Square]

(define-struct square0 [x y threatened])
; A Square is a structure:
;   (make-square0 N N Boolean)

; Number ->  Board
(define (board0 n)
  (local ((define grid   (map (lambda (x)
                                (map (lambda (y)
                                       (make-square0 x y #f))
                                     (range 0 n 1)))
                              (range 0 n 1))))
    (flatten grid)))

(check-expect (board0 2) `(,(make-square0 0 0 #f)
                           ,(make-square0 0 1 #f)
                           ,(make-square0 1 0 #f)
                           ,(make-square0 1 1 #f)))

; Board QP -> Board 
; places a queen at qp on a-board
(define (add-queen a-board qp)
  ;; todo - I could use filter on a-board to just check
  ;; filelds that are not already thretened
  (map (lambda (sq)
         (local ((define sq-x (square0-x sq))
                 (define sq-y (square0-y sq))
                 (define sq-threatened? (threatening? qp
                                                      (make-posn sq-x sq-y))))
           (if sq-threatened?
               (make-square0 sq-x sq-y #t)
               sq)))
       a-board))

(check-expect (add-queen (board0 3) (make-posn 0 0))
              `(,(make-square0 0 0 #t)
                ,(make-square0 0 1 #t)
                ,(make-square0 0 2 #t)
                ,(make-square0 1 0 #t)
                ,(make-square0 1 1 #t)
                ,(make-square0 1 2 #f)
                ,(make-square0 2 0 #t)
                ,(make-square0 2 1 #f)
                ,(make-square0 2 2 #t)))

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  (foldr (lambda (sq acc)
           (local ((define sq-x (square0-x sq))
                   (define sq-y (square0-y sq))
                   (define sq-threatened? (square0-threatened sq)))
             (if sq-threatened?
                 acc
                 (cons (make-posn sq-x sq-y)
                       acc))))
         '()
         a-board))
                  

(check-expect (find-open-spots `(,(make-square0 0 0 #t)
                                 ,(make-square0 0 1 #f)
                                 ,(make-square0 1 0 #t)
                                 ,(make-square0 1 1 #f)))
              `(,(make-posn 0 1) ,(make-posn 1 1)))

(check-expect (find-open-spots `(,(make-square0 0 0 #t)
                                 ,(make-square0 0 1 #t)))
              '())

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (local ((define open-spots (find-open-spots a-board)))
    (cond [(empty? open-spots) #false]
          [(= n 1) (list (first open-spots))]
          [else
           (place-queens/list open-spots a-board n)])))

; [List-of Square] Board N -> [Maybe [List-of QP]
(define (place-queens/list loq a-board n)
    (cond [(empty? loq) #false]
          [else
           (local ((define candidate (place-queens (add-queen a-board (first loq)) (- n 1))))
             (cond [(boolean? candidate)
                    (place-queens/list (rest loq) a-board n)]
                   [else (cons (first loq) candidate)]))]))

(define (n-queens n)
  (place-queens (board0 n) n))

(check-expect (n-queens 3) #false)
(check-satisfied (n-queens 4) (n-queens-solution? 4))
; (render-queens 8 (n-queens 8) QUEEN-FIG)

; =================== End of exercise ==================
