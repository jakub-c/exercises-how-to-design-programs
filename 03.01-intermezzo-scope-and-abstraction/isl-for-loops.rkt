;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname isl-for-loops) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
; IndexValue is:
; (list Number X)

; [List-of X] -> [List-of IndexValue]
(check-expect (enumerate '("a" "b" "c"))
              '((1 "a") (2 "b") (3 "c")))
(check-expect (enumerate '(1 2 3))
              '((1 1) (2 2) (3 3)))

(define (enumerate l)
  (map list
       (build-list (length l) (lambda (x) (+ x 1)))
       l))


; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its index 
 
(check-expect
 (enumerate-v2 '(a b c)) '((1 a) (2 b) (3 c)))
 
(define (enumerate-v2 lx)
  (for/list ([x lx] [ith (length lx)])
    (list (+ ith 1) x)))

(define (enumerate.v3 lx)
  (for/list ([item lx] [ith (in-naturals 1)])
    (list ith item)))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
(check-expect (cross '("a" "b" "c") '(1 2))
              '(("a" 1) ("a" 2) ("b" 1) ("b" 2) ("c" 1) ("c" 2)))

(check-satisfied (cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))

; (define (cross l1 l2) '(())) ;stub
(define (cross l1 l2)
  (foldr (lambda (x y)
           (local ((define (produce-pairs lx)
                     (foldr (lambda (a b)
                              (cons
                               (list x a)
                               b))
                            '() lx)))
             (append 
              (produce-pairs l2)
              y)))         
         '() l1))

(define (cross-v2 l1 l2)
  (for*/list ([x1 l1][x2 l2])
    (list x1 x2)))


; [List-of X] [X -> Boolean] -> [Maybe X]
#;(check-expect (and-map (range 0 10 1) (lambda (i) (> (- 9 i) 0)))
                #false)
(check-expect (and-map (range 0 10 1) (lambda (i) (if (>= i 0) i #false)))
              9)              

; (define (and-map l f) #f) ; stub
(define (and-map l f)
  (cond [(empty? (rest l)) (first l)]
        [else
         (if (equal? (f (first l)) #false)
             #false
             (and-map (rest l) f))]))



(check-expect (or-map (range 0 10 1) (lambda (i) (if (= (- 9 i) 0) i #false)))
              9)
(check-expect (or-map (range 0 10 1) (lambda (i) (if (< i 0) i #false)))
              #false)

; [List-of X] [X -> Boolean] -> [Maybe X] 
;(define (or-map l f) #false) ;stub

(define (or-map l f)
  (cond [(empty? (rest l)) (f (first l))]
        [else
         (if (equal? (f (first l)) #false)
             (or-map (rest l) f)
             (first l))]))