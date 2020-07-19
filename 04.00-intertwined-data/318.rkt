;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |318|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
	
; An Atom is one of: 
; – Number
; – String
; – Symbol 

(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((hello world) hello))) 3)

; An Atom is one of: 
; – Number
; – String
; – Symbol

(define (atom? a)
  (or
   (number? a)
   (string? a)
   (symbol? a)))

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (depth sexp)
  (local (; S-expr Symbol -> N
          (define (count-depth sexp)
            (cond
              [(atom? sexp) 1]
              [else (count-sl sexp)]))
          ; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count-depth (first sl)) (count-sl (rest sl)))])))
    (count-depth sexp)))
