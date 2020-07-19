;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 355-assq) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define WRONG "parser error!")

(define-struct add [left right])
(define-struct mul [left right])


; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
(check-error (parse-sl '("a")) WRONG)
(check-error (parse-sl '(1 2 3)) WRONG)
(check-expect (parse-sl '(+ 2 3)) (make-add 2 3))
(check-expect (parse-sl '(* 2 3)) (make-mul 2 3))
(check-error (parse-sl '(/ 2 3)) WRONG)

; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))

(check-expect (parse-atom 1) 1)
(check-error (parse-atom "abc") WRONG)
(check-error (parse-atom 'sybmol) WRONG)


(define (eval-expression expr)
  (cond [(number? expr) expr]
        [else
         (if (add? expr)
             (+ (eval-expression (add-left expr)) (eval-expression (add-right expr)))
             (* (eval-expression (mul-left expr)) (eval-expression (mul-right expr))))]))


 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

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

(define (subst ex x v)
  (local ((define (subst-sl sl)
            (cond [(empty? sl) '()]
                  [else (cons (subst (first sl) x v)
                              (subst (rest sl) x v))])))
    (cond [(atom? ex)
           (if (equal? ex x)
               v
               ex)]
          [else (subst-sl ex)])))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define VARS '((x 4) (y 5)))

(check-expect (eval-variable* '(+ 1 1) VARS) 2)
(check-expect (eval-variable* '(+ x 1) VARS) 5)
(check-expect (eval-variable* '(+ (+ x 5) (+ y (+ y 5))) VARS) 24)

; BSL-var-expr AL -> Number
; (define (eval-variable* ex da) 0) ;stub

(define (eval-variable* ex da)
  (local ((define (replace-vars ex da)
            (cond [(empty? da) ex]
                  [else
                   (local ((define parsed-expression
                             (subst ex (first (first da)) (second (first da)))))
                     (eval-variable* parsed-expression (rest da)))])))
    (eval-expression (parse (replace-vars ex da)))))

(check-expect (eval-var-lookup '(+ 1 1) VARS) 2)
(check-expect (eval-var-lookup '(+ x 1) VARS) 5)
(check-expect (eval-var-lookup '(+ (+ x 5) (+ y (+ y 5))) VARS) 24)

; BSL-var-expr AL -> Number
(define (eval-var-lookup e da)
  (local ((define (lookup e)
            (cond [(atom? e)
                   (if (and (symbol? e)
                            (and (not (equal? e '+))
                                (not (equal? e '*))))
                       (second (assq e da))
                       e)]
                  [else (lookup-sl e)]))
          (define (lookup-sl l)
            (cond [(empty? l) '()]
                  [else
                   (cons
                    (lookup (first l))
                    (lookup-sl (rest l)))])))
     (eval-expression (parse (lookup e)))))
                
