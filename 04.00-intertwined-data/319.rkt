;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |319|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(check-expect (substitute 'world 'world 'hello) 'hello)
(check-expect (substitute '(world hello) 'world 'hello)
              '(hello hello))
(check-expect (substitute '(((hello world) hello)) 'world 'hello)
              '(((hello hello) hello)))

; An Atom is one of: 
; – Number
; – String
; – Symbol

(define (atom? a)
  (or
   (number? a)
   (string? a)
   (symbol? a)))

; S-expr -> S-expr
; (define (substitute sexp old new)) ;stub

(define (substitute sexp old new)
  (local (; Atom -> Atom
          (define (replace-atom a)
            (if (equal? a old)
                new
                a))
          (define (replace-sl sl)
            (cond [(empty? sl) '()]
                  [else (cons
                         (replace (first sl))
                         (replace-sl (rest sl)))]))
          ; S-exp -> S-exp
          (define (replace sexp)
            (cond
              [(atom? sexp) (replace-atom sexp)]
              [else (replace-sl sexp)])))
    (replace sexp)))
