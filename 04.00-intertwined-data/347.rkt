;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |347|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An Expression is one of:
; - Number
; - (make-add Expression Expression)
; - (make-mul Expression Expression)

; An Add is a structure:
;  (make-add Expression Expression)
(define-struct add [left right])
; A Mul is a structure:
;  (make-add Expression Expression)
(define-struct mul [left right])


(check-expect (eval-expression (make-add -1 2)) (+ -1 2))
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)
(check-expect (eval-expression (make-mul (make-add 1 (make-mul 2 3)) 3.14))
              (*
               (+ 1 (* 2 3))
               3.14))

;  consumes a representation of a BSL expression and computes its value
; (define (eval-expression expr) 0) ;stub

(define (eval-expression expr)
  (cond [(number? expr) expr]
        [else
         (if (add? expr)
           (+ (eval-expression (add-left expr)) (eval-expression (add-right expr)))
           (* (eval-expression (mul-left expr)) (eval-expression (mul-right expr))))]))