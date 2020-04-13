;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 352-361) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
                               (subst (mul-right ex) sym num))]
           [else (error "unhandled subst type")])]))

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

; ==================== Exercise 357 =====================

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; determine the value of ex, evaluate function defintions
; f - function name
; x - function paramater
; b - function body
(check-error (eval-definition1 'f
                               'f 'x 1))

(check-expect (eval-definition1
               (make-fun 'f 1) 'f 'x 1)
              1)
(check-error (eval-definition1
              (make-fun 'g 1) 'f 'x 1))
(check-expect (eval-definition1 1
                                'f 'x 1)
              1)
(check-expect (eval-definition1
               (make-add 1 (make-mul 2 3))
               'f 'x 1)
              7)
(check-expect (eval-definition1
               (make-fun 'f (make-add 1 1))
               'f 'x (make-add 'x 'x))
              4)
(check-expect (eval-definition1
               (make-add
                (make-fun 'f (make-add 1 1))
                4)
               'f 'x (make-add 'x 'x))
              8)
(check-expect (eval-definition1
               (make-fun 'f
                         (make-fun 'f 4))
               'f 'x (make-add 'x 'x))
              16)
(check-expect (eval-definition1
               (make-mul (make-fun 'f
                                   (make-fun 'f 4))
                         (make-add 4
                                   (make-fun 'f (make-add 4 5))))
               'f 'x (make-add 'x 'x))
              352)
; construct an input for eval-definition1 that causes it to run forever
#;(check-error (eval-definition1
                (make-fun 'f 5) 
                'f 
                'x 
                (make-fun 'f 5)))

; (define (eval-definition1 ex f x b) 0) ;stub

(define (eval-definition1 ex f x b)
  (match ex
    ([? number?] ex)
    ([? symbol?]
     (error "no variable assignment"))
    ([? fun?]
     (local ((define argument (eval-definition1 (fun-expression ex) f x b))
             (define fun-name-is-defined? (equal? (fun-name ex) f)))
       (if fun-name-is-defined?
           (eval-definition1 (subst b x argument) f x b)
           (error "function undefined"))))
    ([? add?] (+ (eval-definition1 (add-left ex) f x b)
                 (eval-definition1 (add-right ex) f x b)))
    ([? mul?] (* (eval-definition1 (mul-left ex) f x b)
                 (eval-definition1 (mul-right ex) f x b)))))

; =================== End of exercise ===================

; ==================== Exercise 358 =====================

(define-struct fun-def [name parameter body])
; BSL-fun-def is a structure:
;  - (make-fun-def (Symbol Symbol BSL-fun-expression)

; (define (f x) (+ 3 x))
(define f (make-fun-def 'f 'x (make-add 3 'x)))

;(define (g y) (f (* 2 y)))
(define g (make-fun-def 'g 'y (make-fun 'f (make-mul 2 'y))))

;(define (h v) (+ (f v) (g v)))
(define h (make-fun-def 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))


; BSL-fun-def* is one of:
; - '()
; - (cons BSL-fun-def '())

(define da-fgh (list f g h))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'z))

(define (lookup-def da f)
  (local ((define result (filter (lambda (el)
                                   (if (fun-def? el)
                                       (equal? (fun-def-name el) f)
                                       #false))
                                 da)))
    (if (empty? result)
        (error "definition not found")
        (first result))))

; =================== End of exercise ===================

; ==================== Exercise 359 =====================

; BSL-fun-expression BSL-fun-def* -> Number
; produce the result that DrRacket shows if you evaluate ex
; in the interactions area,
; assuming the definitions area contains da
(define function-defitinions1
  (list (make-fun-def 'f 'x 1)))
(define function-defitinions2
  (list (make-fun-def 'f 'x (make-add 'x 'x))
        (make-fun-def 'g 'x (make-add 'x (make-fun 'f 4)))))

(check-error (eval-function*
              'f function-defitinions1))

(check-expect (eval-function*
               (make-fun 'f 1) function-defitinions1)
              1)
(check-error (eval-function*
              (make-fun 'g 1) function-defitinions1))
(check-expect (eval-function* 1 function-defitinions1)
              1)
(check-expect (eval-function*
               (make-add 1 (make-mul 2 3)) function-defitinions1)
              7)
(check-expect (eval-function*
               (make-fun 'f (make-add 1 1)) function-defitinions2)
              4)
(check-expect (eval-function*
               (make-add
                (make-fun 'f (make-add 1 1))
                4)
               function-defitinions2)
              8)
(check-expect (eval-function*
               (make-fun 'f
                         (make-fun 'f 4))
               function-defitinions2)
              16)
(check-expect (eval-function*
               (make-mul (make-fun 'f
                                   (make-fun 'f 4))
                         (make-add 4
                                   (make-fun 'f (make-add 4 5))))
               function-defitinions2)
              352)
(check-expect (eval-function* (make-fun 'g 3) function-defitinions2) 11)
; construct an input for eval-function* that causes it to run forever
#;(check-error (eval-function*
                (make-fun 'f 5) 
                'f 
                'x 
                (make-fun 'f 5)))
;;;;;;
; this section helper has been added to make the refactoring of
; eval-function* easier while working on the ex. 362
; this is a referesher of structs, conts and structs that were needed
;;;;;;

; (define (eval-function* ex da) 0) ;stub
#;(define function-defitinions2
    (list (make-fun-def 'f 'x (make-add 'x 'x))
          (make-fun-def 'g 'x (make-add 'x (make-fun 'f 4)))))
; (define-struct fun-def [name parameter body])
; (define-struct fun [name expression])
#;(check-expect (subst (make-add 'x
                                 (make-add 'x 5))
                       'x 4)
                (make-add 4
                          (make-add 4 5)))
;;;;;;
; end of helper section
;;;;;;

(define (eval-function* ex da)
  (match ex
    [(? number?) ex]
    [(? symbol?)
     (const-def-value (lookup-con-def da ex))]
    [(? fun?) (local ((define argument (fun-expression ex))
                      (define current-function-name (fun-name ex))
                      (define found-definition (lookup-def da current-function-name))
                      (define found-body (fun-def-body found-definition))
                      (define found-parameter (fun-def-parameter found-definition)))
                (eval-function* (subst.v2 found-body found-parameter argument) da))]
    [(? add?) (+ (eval-function* (add-left ex) da)
                 (eval-function* (add-right ex) da))]
    [(? mul?) (* (eval-function* (mul-left ex) da)
                 (eval-function* (mul-right ex) da))]))

(check-expect (subst.v2 (make-add 'x
                                  (make-add 'x 5))
                        'x 4)
              (make-add 4
                        (make-add 4 5)))
(check-expect (subst.v2 (make-fun 'f (make-add 'x 6))
                        'x 4)
              (make-fun 'f (make-add 4 6)))

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like `ex`
; with all occurrences of `sym` replaced by `num`
(define (subst.v2 ex sym num)
  (cond [(number? ex) ex]
        [(symbol? ex)
         (if (equal? ex sym)
             num
             ex)]
        [else
         (match ex
           [(? add?) (make-add (subst.v2 (add-left ex) sym num)
                               (subst.v2 (add-right ex) sym num))]
           [(? mul?) (make-mul (subst.v2 (mul-left ex) sym num)
                               (subst.v2 (mul-right ex) sym num))]
           [(? fun?) (make-fun (fun-name ex) (subst.v2 (fun-expression ex) sym num))]
           [else (error "unhandled subst type")])]))
    
; =================== End of exercise ===================

; ==================== Exercise 360 =====================

(define-struct const-def [name value])
; BSL-const-def is a structure:
;  - (make-const-def Symbol Number)

; BSL-da-all is one of:
; - '()
; - (cons BSL-fun-def BSL-da-all)
; - (cons BSL-const-def BSL-da-all)

(define example-all-definitions
  (list (make-const-def 'x 10)
        (make-const-def 'y 11)
        (make-fun-def 'f 'x (make-add 'x 'x))
        (make-fun-def 'g 'h (make-mul (make-add 1 'h) 'h))))

; BSL-da-all Symbol -> BSL-const-def
; produce the representation of a constant definition whose name is x,
; otherwise the function signals an error
(check-expect (lookup-con-def example-all-definitions 'x) (make-const-def 'x 10))
(check-error (lookup-con-def example-all-definitions 'a))

;(define (lookup-con-def da x) (make-const-def 'x 1)) ;stub

(define (lookup-con-def da x)
  (local ((define result
            (filter (lambda (el)
                      (match el
                        [(? const-def?) (equal? (const-def-name el) x)]
                        [else #false]))
                    da)))
    (if (empty? result)
        (error "variable definition not found")
        (first result))))

; BSL-data-all Symbol -> BSL-fun-expression
; produce the representation of a function definition whose name is f,
; if such a piece of data exists in da
(check-error (lookup-fun-def example-all-definitions 'z))
(check-expect (lookup-fun-def example-all-definitions 'f)
              (make-fun-def 'f 'x (make-add 'x 'x)))
(check-expect (lookup-fun-def example-all-definitions 'g)
              (make-fun-def 'g 'h (make-mul (make-add 1 'h) 'h)))

; (define (lookup-fun-def da f) (make-fun-def 'f 'x (make-add 'x 1))) ;stub
(define (lookup-fun-def da f)
  (local ((define result
            (filter (lambda (el)
                      (match el
                        [(? fun-def?) (equal? (fun-def-name el) f)]
                        [else #false]))
                    da)))
    (if (empty? result)
        (error "function definition not found")
        (first result))))

; =================== End of exercise ===================

; ==================== Exercise 361 =====================

; BSL-fun-expression BSL-da-all -> Number
; produce the same value that DrRacket shows
; if the expression is entered at the prompt
; in the interactions area and the definitions area contains
; the appropriate definitions
(check-expect (eval-all 1 example-all-definitions)
              1)
(check-expect (eval-all 'y example-all-definitions)
              11)
(check-expect (eval-all (make-add 1 1) example-all-definitions)
              2)
(check-expect (eval-all (make-add 'x 1) example-all-definitions)
              11)
(check-expect (eval-all (make-add 'x
                                  (make-mul 'x 'y))
                        example-all-definitions)
              120)
(check-expect (eval-all (make-fun 'f 1) example-all-definitions)
              2)
(check-expect (eval-all (make-fun 'f (make-add 1 1)) example-all-definitions)
              4)
(check-expect (eval-all (make-fun 'f
                                  (make-add 1
                                            (make-fun 'f 4))) example-all-definitions)
              18)
(check-expect (eval-all (make-fun 'f (make-add 'x 1)) example-all-definitions)
              22)

; (define (eval-all ex da) 0) ;stub
(define (eval-all ex da)
  (match ex
    [(? number?) ex]
    [(? symbol?)
     (const-def-value (lookup-con-def da ex))]
    [(? fun?)
     (eval-function* ex da)]
    [(? add?) (+ (eval-all (add-left ex) da)
                 (eval-all (add-right ex) da))]
    [(? mul?) (* (eval-all (mul-left ex) da)
                 (eval-all (mul-right ex) da))]))
; =================== End of exercise ===================

; ==================== Exercise 362 =====================
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)
      (boolean? a)))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      ;[(< L 3) (error "expression length error")]
      [(and (= L 1) (symbol? (first s))) (first s)]
      [(and (= L 2) (symbol? (first s)))
       (make-fun (first s) (parse (second s)))]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error "unsupported operation")])]
      [else (error "parsing error")])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error "value type error")]
    [(symbol? s) s]))


(check-expect (parse-da-sl (list `(define x 1)
                                 `(define (f x) (+ x x))))
              (list (make-const-def 'x 1)
                    (make-fun-def 'f 'x (make-add 'x 'x))))
(define (parse-da-sl s)
  (map (lambda (el)
         (match el
           [(list keyword (list fn-name fn-param) exp)
            (make-fun-def fn-name fn-param (parse exp))]
           [(list keyword name value)
            (make-const-def name value)]
           [else (error "can't parse the definitions")]))
       s))

; S-expr SL -> Number
; interpret and evaluate quoted expression and definitions
(check-expect (interpreter 1 '()) 1)
(check-expect (interpreter `(* (+ 1 1) 2) '()) 4)
(check-expect (interpreter `(* (+ 1 x) 3) `((define x 4))) 15)
(check-error (interpreter `(f 1) `()))
(check-expect (interpreter `(f (+ 1 1))
                           `((define (f x) (+ x x))))
              4)
(check-expect (interpreter `(* (f (+ 1 1))
                               y)
                           `((define (f x) (+ x x)) (define y 2)))
              8)
(check-expect (interpreter `(area-of-circle 4)
                           `((define close-to-pi 3.14)
                             (define (area-of-circle r)
                               (* close-to-pi (* r r)))))
              50.24)
(check-expect (interpreter `(volume-of-10-cylinder 4)
                           `((define close-to-pi 3.14)
                             (define (area-of-circle r)
                               (* close-to-pi (* r r)))
                             (define (volume-of-10-cylinder r)
                               (* 10 (area-of-circle r)))))
              502.4)

;(define (interpreter ex da) 1) ;stub

(define (interpreter ex da)
  (eval-all (parse ex) (parse-da-sl da)))
  
; =================== End of exercise ===================
