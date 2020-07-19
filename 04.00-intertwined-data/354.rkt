;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |354|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define EVAL-ERROR "the input value is not numeric")

(define-struct add [left right])
(define-struct mul [left right])

; BSL-var-expr -> Boolean
(define (numeric? expr)
  (match expr
    [(? number?) #true]
    [(? symbol?) #false]
    [(? add?) (and
               (andmap
                numeric? (list (add-left expr)))
               (andmap
                numeric? (list (add-right expr))))]
    [(? mul?) (and
               (andmap
                numeric? (list (mul-left expr)))
               (andmap
                numeric? (list (mul-right expr))))]))

;;;;;;;;;;;

; BSL-var-expr -> Number
; determine value if numeric? yields true for the input
; otherwise signal an error

(check-expect (eval-variable 5) 5)
(check-error (eval-variable 'x))
(check-expect 
 (eval-variable (make-add 2 (make-mul 2 3))) 
 8
 )
(check-error (eval-variable (make-add 2 (make-mul 2 'y))))

(define (eval-variable expr)
  (if (numeric? expr)
      (match expr
        [(? number?) expr]
        [(? add?) (+ (eval-variable (add-left expr)) (eval-variable (add-right expr)))]
        [(? mul?) (* (eval-variable (mul-left expr)) (eval-variable (mul-right expr)))])
      (error EVAL-ERROR)))
