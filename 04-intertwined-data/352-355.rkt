;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 352-355) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ### Data definitions
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of: 
; – Number 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; ==================== Exercise 352 ====================

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like `ex`
; with all occurrences of `sym` replaced by `num`
(check-expect (subst 1 'x 2)
              1)

(check-expect (subst 'x 'x 2)
              2)

(check-expect (subst 'y 'x 2)
              'y)

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
        [(symbol? ex)
         (if (equal? ex sym)
             num
             ex)]
        [else
         (match ex
           [(? add?) (make-add (subst (add-left ex) sym num)
                           (subst (add-right ex) sym num))]
           [(? mul?) (make-mul (subst (mul-left ex) sym num)
                           (subst (mul-right ex) sym num))])]))

; =================== End of exercise ==================

; ==================== Exercise 353 ====================

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

; =================== End of exercise ==================

; ==================== Exercise 354 ====================

(define EVAL-ERROR "the input value is not numeric")

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

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define VARS '((x 4) (y 5)))

; BSL-var-expr AL -> Number

(check-expect (eval-variable* (make-add 1 1) VARS) 2)
(check-error (eval-variable* (make-add "a" 1) VARS))
(check-expect (eval-variable*
               (make-add (make-add 'x 5)
                         (make-add 'y
                                   (make-add 'y 5))) VARS)
              24)

; (define (eval-variable* ex da) 0) ;stub

(define (eval-variable* ex da)
  (local ((define (replace-vars ex da)
            (cond [(empty? da) ex]
                  [else
                   (local ((define subst-expression
                             (subst ex
                                    (first (first da))
                                    (second (first da)))))
                     (eval-variable* subst-expression (rest da)))])))
    (eval-variable (replace-vars ex da))))

; =================== End of exercise ==================

; ==================== Exercise 355 ====================

; BSL-var-expr AL -> Number
(check-expect (eval-var-lookup (make-add 1 1) VARS) 2)
(check-error (eval-var-lookup (make-add "a" 1) VARS))
(check-expect (eval-var-lookup
               (make-add (make-add 'x 5)
                         (make-add 'y
                                   (make-add 'y 5))) VARS)
              24)

; (define (eval-var-lookup e da) 0) ;stub

(define (eval-var-lookup e da)
  (local ((define (find-syms-val sym lookup)
            (second (assq sym lookup))))
    (match e
      [(? number?) e]
      [(? symbol?) (find-syms-val e da)]
      [(? add?) (+ (eval-var-lookup (add-left e) da)
                   (eval-var-lookup (add-right e) da))]
      [(? mul?) (* (eval-var-lookup (mul-left e) da)
                   (eval-var-lookup (mul-right e) da))])))


; =================== End of exercise ===================

; ==================== Exercise 356 =====================

; Fun is a structure:
;  - (make-fun 'f (make-add Symbol BSL-fun-expr))
(define-struct fun [name expression])

; A BSL-fun-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

; (k (+ 1 1))
(make-fun 'k (make-add 1 1))

;(* 5 (k (+ 1 1)))
(make-mul 5 (make-fun 'k
                      (make-add 1 1)))

; (* (i 5) (k (+ 1 1)))
(make-mul
 (make-fun 'i 5)
 (make-fun 'k
           (make-add 1 1)))

; =================== End of exercise ===================

