;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |401|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 400 ====================

; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr S-expr -> Boolean
; determine whether two S-expressions are equal
; (define (sexp=? s1 s2) #false) ;stub

(check-expect (sexp=? 1 1) #true)
(check-expect (sexp=? "1" "2") #false)
(check-expect (sexp=? "1" 2) #false)
(check-expect (sexp=? '(a b) '(a b)) #true)
(check-expect (sexp=? '(a (b 2) (c 3)) '(a (b 2) (c 4))) #false)

(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))

; first implementation
#;(define (sexp=? s1 s2)
    (cond [(and (atom? s1) (atom? s2)) (equal? s1 s2)]
          [(or (and (cons? s1) (atom? s2))
               (and (atom? s1) (cons? s2))) #false]
          [else
           (if (or (empty? s1) (empty? s2))
               #true
               (and
                (sexp=? (first s1) (first s2))
                (sexp=? (rest s1) (rest s2))))]))

; move if to cond
#;(define (sexp=? s1 s2)
  (cond [(or (empty? s1) (empty? s2)) #true]
        [(and (atom? s1) (atom? s2)) (equal? s1 s2)]
        [(or (and (cons? s1) (atom? s2))
             (and (atom? s1) (cons? s2))) #false]
        [else
         (and
          (sexp=? (first s1) (first s2))
          (sexp=? (rest s1) (rest s2)))]))

; remove redundant check
(define (sexp=? s1 s2)
  (cond [(or (empty? s1) (empty? s2)) #true]
        [(and (atom? s1) (atom? s2)) (equal? s1 s2)]
        [else
         (and
          (sexp=? (first s1) (first s2))
          (sexp=? (rest s1) (rest s2)))]))

; =================== End of exercise ==================
