;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |352|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ### Data definitions
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like ex with all occurrences of x replaced by v
(check-expect (subst 1 'x 2)
              1)

(check-expect (subst 'x 'x 2)
              2)

(check-expect (subst (make-add 'x 2) 'x 4)
              (make-add 4 2))

(check-expect (subst (make-add 'x
                               (make-add 'x 5))
                     'x 4)
              (make-add 4
                        (make-add 4 5)))


; (define (subst ex sym num) 0) ;stub

(define (subst ex sym num)
  (cond [(number? ex) ex]
        [(equal? ex sym) num]
        [else
         (match ex
           [add? (make-add (subst (add-left ex) sym num)
                           (subst (add-right ex) sym num))]
           [mul? (make-mul (subst (add-left ex) sym num)
                           (subst (add-right ex) sym num))])]))
