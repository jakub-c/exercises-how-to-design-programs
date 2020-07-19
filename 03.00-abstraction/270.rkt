;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |270|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> List-of-Number
; creates the list (list 0 ... (- n 1)) for any natural number n;
(check-expect (mk-list 3) (list -1 0 1))
(define (mk-list n)
  (build-list n sub1))

; Number -> List-of-Number
; creates the list (list 1 ... n) for any natural number n;
(check-expect (mk-list-v2 3) (list 1 2 3))
(define (mk-list-v2 n)
  (build-list n add1))

; Number -> List-of-Number
; creates the list (list 1 1/2 ... 1/n) for any natural number n;
(check-expect (mk-list-v3 3) (list 1 (/ 1 2) (/ 1 3)))

(define (mk-list-v3 n)
  (local (; Number -> Number
          (define (make-fraction n)
            (if (= n 0)
                1
                (/ 1 (+ n 1)))))            
    (build-list n make-fraction)))

; Number -> [List-of Numbers]
; create the list of the first n even numbers;
(check-expect (list-of-even 3) '(0 2 4))
(check-expect (list-of-even 6) '(0 2 4 6 8 10))

; (define (list-of-even n) '()) ;stub
(define (list-of-even n)
  (local (; Number -> Number
          (define (times-2 n) (* n 2)))
    (build-list n times-2)))

; Number -> [List-of [List-of Number]]
; creates a diagonal square of 0s and 1s
(define (identityM n)
  (local (; [List-of [List-of Number]] -> [List -of [List-of Number]]
          ; add 0 to each list in each row
          (define (cons-0 lon)
            (cond [(empty? lon) '()]
                  [else
                   (cons
                    (append '(0) (first lon))
                    (cons-0 (rest lon)))]))
          ; Number -> List-of Number
          (define (fill-with-0 n)
            (if (> n 0)
                (cons 0 (fill-with-0 (- n 1)))
                '())))
    (cond [(= n 0) '()]
          [else
           (cons
            (cons 1 (fill-with-0 (- n 1)))
            (cons-0 (identityM (- n 1))))])))

;(identityM 4)

; Number [Number -> Number] -> [List-of Number]
; tabulate a given function on numbers from 0 to n
; (check-expect (tabulate 3 sin) (list (sin 0) (sin 1) (sin 2)))
; this check expect works but racket has problems comparing inexact numbers

;(define (tabulate n f) '()) ;stub

(define (tabulate n f)
  (build-list n f))