;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |398|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ==================== Exercise 398 ====================

; Combination is a list of: operator, coefficient, variable / number
;  - (list '+ Number Symbol)
;  - (list '+ Number Number)

(define combination '(* 5 x))
(define combination2 '(* 5 1))

; LinComb (linear combination) is one of:
;  - Combination
;  - (append '+ [List-of LinComb])

(define lincomb '(+ combination combination))

(define linear-combination '(+ (* 5 x) (* 17 y) (* 3 z)))
(define variable-values '(10 1 2))

; LinComb [List-of Number] -> Number
; produces the value of the combination LinComb
; and a list of variable values

; (define (value loc lov) 0) ;stub

(check-expect (value linear-combination variable-values) 73)

(define (value lincomb lov)
  (evaluate (parse lincomb lov)))

; LinComb [List-of Number] -> LinComb
; (define (parse lincomb lov) combination) ;stub

(check-expect (parse '(+ (* 5 x)) '(1))
              '(+ (* 5 1)))
(check-expect (parse linear-combination variable-values)
              '(+ (* 5 10) (* 17 1) (* 3 2)))

(define (parse lincomb lov)
  (local ((define list-of-combinations (rest lincomb))
          (define (parse-combination comb var)
            (list
             (first comb)
             (second comb)
             var)))
    (cons '+ (map parse-combination list-of-combinations lov))))

; LinComb -> Number
(check-expect (evaluate '(+ (* 5 1))) 5)

(define (evaluate lincomb)
  (for/sum ([c (rest lincomb)])
    (* (second c) (third c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-expect (value-v2 linear-combination variable-values) 73)

(define (value-v2 lincomb lov)
  (for/sum ([combination (rest lincomb)] [variable lov])
    (* (combination c) variable)))

; =================== End of exercise ==================
