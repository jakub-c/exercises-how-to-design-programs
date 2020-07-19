;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |353|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ### Data definitions
(define-struct add [left right])
(define-struct mul [left right])

; BSL-var-expr -> Boolean
; determine whether a BSL-var-expr is also a BSL-expr
; is expression only made of numbers
(check-expect (numeric? 'x) #false)
(check-expect (numeric? 4) #true)
(check-expect (numeric? (make-add 4 5)) #true)
(check-expect (numeric? (make-add 4 'x)) #false)
(check-expect (numeric? (make-mul 4 8)) #true)
(check-expect (numeric? (make-mul 4 (make-add 3 'y))) #false)

; (define (numeric? expr) #false) ;stub

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