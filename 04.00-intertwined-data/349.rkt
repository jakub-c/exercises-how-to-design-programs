;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |349|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define WRONG "parser error!")

(define-struct add [left right])
(define-struct mul [left right])

(check-error (parse '("a")))
(check-expect (parse '(+ 1 (* 2 3)))
              (make-add 1
                        (make-mul 2 3)))

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
