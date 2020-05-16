;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |454|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 454 ====================

; Number [List-of Number] -> [List-of [List-of Number]
; produce a matrix that is of a size n x n
; the matrix contains the numbers passed as the argument
; (define (create-matrix n) '()) ;stub

(check-expect
 (create-matrix 2 (list 1 2 3 4))
 (list (list 1 2)
       (list 3 4)))

(check-expect
 (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
 (list (list 1 2 3)
       (list 4 5 6)
       (list 7 8 9)))

(define (create-matrix n l)
  (cond [(empty? l) '()]
        [else
         (cons (take n l)
               (create-matrix n (drop n l)))]))

; Number [List-of Number] -> [List-of Number]

(check-expect (take 3 '(1 2 3 4 5))
              '(1 2 3))

(define (take n l)
  (cond [(= n 0) '()]
        [else
         (cons (first l)
               (take (sub1 n) (rest l)))]))

; Number [List-of Number] -> [List-of Number]

(check-expect (drop 3 '(1 2 3 4 5))
              '(4 5))

(define (drop n l)
  (cond [(= n 0) l]
        [else
         (drop (sub1 n) (rest l))]))

; =================== End of exercise =================
