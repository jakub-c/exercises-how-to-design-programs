;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |355|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define WRONG "parser error!")
(define EVAL-ERROR "the input value is not numeric")

(define-struct add [left right])
(define-struct mul [left right])

; An Atom is one of: 
; – Number
; – String
; – Symbol 

(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))


; produces a BSL-var-expr like ex with all occurrences of x replaced by v
; BSL-var-expr Symbol Number -> BSL-var-expr

(define (subst ex sym num)
  (cond [(number? ex) ex]
        [(equal? ex sym) num]
        [else
         (match ex
           [add? (make-add (subst (add-left ex) sym num)
                           (subst (add-right ex) sym num))]
           [mul? (make-mul (subst (add-left ex) sym num)
                           (subst (add-right ex) sym num))])]))

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

(define (eval-variable expr)
  (if (numeric? expr)
      (match expr
        [(? number?) expr]
        [(? add?) (+ (eval-variable (add-left expr)) (eval-variable (add-right expr)))]
        [(? mul?) (* (eval-variable (mul-left expr)) (eval-variable (mul-right expr)))])
      (error EVAL-ERROR)))


; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define VARS '((x 4) (y 5)))

(check-expect (eval-variable* (make-add 1 1) VARS) 2)
(check-expect (eval-variable* (make-add (make-add 'x 5) (make-add 'y (make-add 'y 5))) VARS) 24)

(define scope0 '((x 2) (y 3)))

; BSL-var-expr AL -> [Either Number Error]
(check-expect (eval-variable* 5 scope0) 5)
(check-expect (eval-variable* 'x scope0) 2)
(check-error (eval-variable* 'lol scope0))
(check-expect 
  (eval-variable* (make-add 'x (make-mul 'y 5)) scope0) 
  17
  )

; BSL-var-expr AL -> Number
; (define (eval-variable* ex da) 0) ;stub

(define (eval-variable* ex da)
  (local ((define (replace-vars ex da)
            (cond [(empty? da) ex]
                  [else
                   (local ((define parsed-expression
                             (subst ex (first (first da)) (second (first da)))))
                     (eval-variable* parsed-expression (rest da)))])))
    (eval-variable (replace-vars ex da))))