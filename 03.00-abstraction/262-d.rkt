;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 262-d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; https://www.reddit.com/r/Racket/comments/doicim/identity_matrix_problem/

;(check-expect (identityM 0) '())
;(check-expect (identityM 1) '((1)))
;(check-expect (identityM 2) '((1 0) (0 1)))
;(check-expect (identityM 3) '((1 0 0) (0 1 0) (0 0 1)))

(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 2)
              (list (list 1 0) (list 0 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))


; (define (identityM n) '()) ;stub
(define (identityM n)
  (cond [(= 1 n) '((1))]
        [else
         (cons
          (cons 1 (make-list (- n 1) 0))
          (cons-0 (identityM (- n 1))))]))


;(check-expect (cons-0 '(())) '((0)))
;(check-expect (cons-0 '((1))) '((0 1)))
;(check-expect (cons-0 '((1 0) (0 1))) '((0 1 0) (0 0 1)))
(define (cons-0 l)
  (cond [(empty? l) '()]
        [else
         (cons (cons 0 (first l))
               (cons-0 (rest l)))]))

(identityM 4)