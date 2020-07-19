;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |357|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; produces a BSL-func-expr like ex with all occurrences of x replaced by v
; BSL-func-expr Symbol Number -> BSL-var-expr

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

; An Atom is one of: 
; – Number
; – String
; – Symbol 

(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))

(check-expect (eval-definition1 '(f 1) 'f 'x '(+ x x))
              2)
(check-expect (eval-definition1 '(f (+ 1 1)) 'f 'x '(+ x x))
              4)

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
;(define (eval-definition1 ex f x b) 1) ; stub

#;(define (eval-definition1 ex f x b)
    (local ((define value (eval-definition1 arg f x b))
            (define plugd (subst b x arg-value)))
      (eval-definition1 plugd f x b)))

(define (eval-definition1 ex f x b)
  (subst b x (second ex)))