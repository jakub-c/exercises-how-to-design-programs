;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |399|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require racket/list)

; ==================== Exercise 399 ====================
 
; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
; see exercise 213
; (define (arrangements names) '(("a" "b") ("b" "a"))) ;stub

(check-expect (arrangements '("a" "b" "c"))
              '(("a" "b" "c")
                ("a" "c" "b")
                ("b" "a" "c")
                ("b" "c" "a")
                ("c" "a" "b")
                ("c" "b" "a")))

(define (arrangements names)
  (local ((define (each-name n)
            (cond [(empty? n) '()]
                  [else
                   (append (find-arrangements
                            (first n)
                            (remove (first n) names)
                            '())
                           (each-name (rest n)))])))
    (each-name names)))

; String [List-of String] '() -> [List-of [List-of String]]

;(define (find-arrangements el to-use used) '()) ;stub

(check-expect (find-arrangements "a" '("b" "c") '())
              '(("a" "b" "c")
                ("a" "c" "b")))

(define (find-arrangements el to-use used)
  (cond [(empty? to-use) '()]
        [else (cons
               (cons el (append to-use used))
               (find-arrangements el
                                  (rest to-use)
                                  (cons (first to-use) used)))]))

; [NEList-of X] -> X 
; returns a random item from the list 
; (define (random-pick l) (first l)) ;stub

(check-random (random-pick '(1 2 3)) (list-ref '(1 2 3) (random 3)))

(define (random-pick l)
  (list-ref l (random (length l))))

; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place 
; (define (non-same names ll) ll) ; stub

(check-expect (non-same '(1 2 3) '((1 2 3) (1 3 2) (2 1 3) (2 3 1)))
              '((2 3 1)))

(define (non-same names ll)
  (local ((define (any-place-matches? l1 l2)
            (or (equal? (first l1) (first l2))
                (equal? (second l1) (second l2))
                (equal? (third l1) (third l2)))))
    (cond [(empty? ll) '()]
          [else
           (append
            (if (any-place-matches? names (first ll))
                '()
                (list (first ll)))
            (non-same names (rest ll)))])))

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
   (non-same names (arrangements names))))

(gift-pick '("Louise" "Jane" "Laura" "Dana" "Mary"))
 
; =================== End of exercise ==================
