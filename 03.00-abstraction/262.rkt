;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |262|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

; Number -> List-of [List-of Number]
;create diagonal squares of 0s and 1s

(check-expect (identityM 0) '())
(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 2)
              (list (list 1 0) (list 0 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; (define (dentityM n) '()) ;stub

(define (identityM n)
  (cond [(= n 0) '()]
        [else
         (create-square n n)]))


(check-expect (create-square 0 0) '())
(check-expect (create-square 1 1) '((0)))
(check-expect (create-square 2 2) '((0 0) (0 0)))
(check-expect (create-square 3 3) '((0 0 0) (0 0 0)  (0 0 0)))

(check-expect (push-to-end (list 0 1) (list (list 1 0))) '((1 0) (0 1)))


(define (push-to-end el l)
  (cond
    [(empty? l) el]
    [else
     (append l (list el))]))

;(define (create-square n) '()) ;stub
(define (create-square x y)
  (cond [(= 0 y) '()]
        [else
         (push-to-end (insert-1-at y (list-with-n-zeros x))
                      (create-square x (- y 1)))]))

(check-expect (list-with-n-zeros 0) '())
(check-expect (list-with-n-zeros 1) '(0))
(check-expect (list-with-n-zeros 2) '(0 0))
(check-expect (list-with-n-zeros 3) '(0 0 0))

; (define (list-with-n-zeros n) '()) ;stub
(define (list-with-n-zeros n)
  (cond [(= 0 n) '()]
        [else
         (cons 0
               (list-with-n-zeros (- n 1)))]))

; Number List -> List
; insert 1 at position n in a list
(check-expect (insert-1-at 0 '()) '())
(check-expect (insert-1-at 1 '(0)) '(1))
(check-expect (insert-1-at 2 '(0 0)) '(0 1))
(check-expect (insert-1-at 1 '(0 0)) '(1 0))
(check-expect (insert-1-at 1 '(0 0 0)) '(1 0 0))

; (define (insert-1-at n l) '()) ;stub
(define (insert-1-at n l)
  (cond
    [(empty? l) '()]
    [else
     (append
      (take l (- n 1))
      (cons 1
            (drop l n)))]))


