;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |393|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ==================== Exercise 393 ====================

; A Son is one of: 
; – empty 
; – (cons Number Son)
; 
; Constraint If s is a Son, 
; no number occurs twice in s

; Son Son -> Son
; produces one set that contains the elements of both inputs
;(define (union son1 son2) '()) ;stub

(check-expect (union '() '()) '())
(check-expect (union '(1) '()) '(1))
(check-expect (union '() '(1)) '(1))
(check-expect (union '(0 1 2) '(1 2 3)) '(0 1 2 3))

; inputs could have been illustrated as a table like in 391
; the tests illustrate all the cases from the table though

; first iteration
#;(define (union son1 son2)
    (cond [(and (empty? son1)
                (empty? son2)) '()]
          [(and (cons? son1)
                (empty? son2)) son1]
          [(and (empty? son1)
                (cons? son2)) son2]
          [(and (cons? son1)
                (cons? son2))
           (local ((define merged-lists (append son1 son2))
                   (define (add-unique val list-so-far)
                     (if (member? val list-so-far)
                         list-so-far
                         (cons val list-so-far))))
             (foldr add-unique '() merged-lists))]))

; find common outputs
#;(define (union son1 son2)
    (cond [(and (empty? son1)
                (empty? son2)) (append son1 son2)]
          [(and (cons? son1)
                (empty? son2)) (append son1 son2)]
          [(and (empty? son1)
                (cons? son2)) (append son1 son2)]
          [(and (cons? son1)
                (cons? son2))
           (local ((define merged-lists (append son1 son2))
                   (define (add-unique val list-so-far)
                     (if (member? val list-so-far)
                         list-so-far
                         (cons val list-so-far))))
             (foldr add-unique '() merged-lists))]))

; apply de Morgan's laws
#;(define (union son1 son2)
    (cond [(and (or (empty? son1)
                    (cons? son1)) ; this is always true
                (empty? son2))
           (append son1 son2)]
          [(and (empty? son1)
                (cons? son2))
           (append son1 son2)]
          [(and (cons? son1)
                (cons? son2))
           (local ((define merged-lists (append son1 son2))
                   (define (add-unique val list-so-far)
                     (if (member? val list-so-far)
                         list-so-far
                         (cons val list-so-far))))
             (foldr add-unique '() merged-lists))]))

; simplify
#;(define (union son1 son2)
    (cond [(empty? son2)
           (append son1 son2)]
          [(and (empty? son1)
                (cons? son2)) ; this will be true based on the first step
           (append son1 son2)]
          [(and (cons? son1)
                (cons? son2))
           (local ((define merged-lists (append son1 son2))
                   (define (add-unique val list-so-far)
                     (if (member? val list-so-far)
                         list-so-far
                         (cons val list-so-far))))
             (foldr add-unique '() merged-lists))]))

; simplify
#;(define (union son1 son2)
    (cond [(empty? son2)
           (append son1 son2)]
          [(empty? son1)
           (append son1 son2)]
          [else
           (local ((define merged-lists (append son1 son2))
                   (define (add-unique val list-so-far)
                     (if (member? val list-so-far)
                         list-so-far
                         (cons val list-so-far))))
             (foldr add-unique '() merged-lists))]))

(define (union son1 son2)
  (cond [(or (empty? son2)
             (empty? son1))
         (append son1 son2)]
        [else
         (local ((define merged-lists (append son1 son2))
                 (define (add-unique val list-so-far)
                   (if (member? val list-so-far)
                       list-so-far
                       (cons val list-so-far))))
           (foldr add-unique '() merged-lists))]))

; ======================================================

; Son Son -> Son
; produce the set of exactly those elements that occur in both inputs
; (define (intersect son1 son2) '()) ;stub

(check-expect (intersect '() '()) '())
(check-expect (intersect '(1) '()) '())
(check-expect (intersect '() '(1)) '())
(check-expect (intersect '(0 1 2) '(1 2 3)) '(1 2))

; first implementation
#;(define (intersect son1 son2)
    (cond [(and (empty? son1)
                (empty? son2)) '()]
          [(and (cons? son1)
                (empty? son2)) '()]
          [(and (empty? son1)
                (cons? son2)) '()]
          [(and (cons? son1)
                (cons? son2))
           (if (member? (first son1) son2)
               (cons (first son1) (intersect (rest son1) son2))
               (intersect (rest son1) son2))]))

; group and calls
#;(define (intersect son1 son2)
    (cond [(or (and (empty? son1)
                    (empty? son2))
               (and (cons? son1)
                    (empty? son2))
               (and (empty? son1)
                    (cons? son2)))
           '()]
          [(and (cons? son1)
                (cons? son2))
           (if (member? (first son1) son2)
               (cons (first son1) (intersect (rest son1) son2))
               (intersect (rest son1) son2))]))

; apply simplification based on the common sense
; and looking at the examples
; if one of the sets is empty by definitions intersect is empty
(define (intersect son1 son2)
  (cond [(or (empty? son1)
             (empty? son2))
         '()]
        [else
         (if (member? (first son1) son2)
             (cons (first son1) (intersect (rest son1) son2))
             (intersect (rest son1) son2))]))

; =================== End of exercise ==================


; ==================== Exercise 394 ====================

; [List-of Number] [List-of Number] -> [List-of Number]
; condition: input lists have to be sorted in ascending order
; produce a single sorted list of numbers that contains all the numbers
; on both inputs lists
; numbers can be repeated in the output
; (define (merge lon1 lon2) '()) ;stub

(check-expect (merge '(1) '(2)) '(1 2))
(check-expect (merge '(1) '(1)) '(1 1))
(check-expect (merge '(1 2) '(3 4)) '(1 2 3 4))
(check-expect (merge '(3 4) '(1 2)) '(1 2 3 4))
(check-expect (merge '(1 2 3 4 6) '(5)) '(1 2 3 4 5 6))
(check-expect (merge '(7) '(1 2 3)) '(1 2 3 7))

; initial implementation
#;(define (merge lon1 lon2)
    (cond [(empty? lon1) lon2]
          [(empty? lon2) lon1]
          [(= (first lon1) (first lon2))
           (cons (first lon1)
                 (cons (first lon2)
                       (merge (rest lon1) (rest lon2))))]
          [(> (first lon1) (first lon2))
           (cons (first lon2)
                 (merge lon1 (rest lon2)))]
          [(< (first lon1) (first lon2))
           (cons (first lon1)
                 (merge (rest lon1) lon2))]))

;simplify (= (first lon1) (first lon2)) condition
#;(define (merge lon1 lon2)
    (cond [(empty? lon1) lon2]
          [(empty? lon2) lon1]
          [(= (first lon1) (first lon2))
           (cons (first lon1)
                 (merge (rest lon1) lon2))]
          [(> (first lon1) (first lon2))
           (cons (first lon2)
                 (merge lon1 (rest lon2)))]
          [(< (first lon1) (first lon2))
           (cons (first lon1)
                 (merge (rest lon1) lon2))]))

; merge duplicate conditions
#;(define (merge lon1 lon2)
    (cond [(empty? lon1) lon2]
          [(empty? lon2) lon1]
          [(or (= (first lon1) (first lon2))
               (< (first lon1) (first lon2)))
           (cons (first lon1)
                 (merge (rest lon1) lon2))]
          [(> (first lon1) (first lon2))
           (cons (first lon2)
                 (merge lon1 (rest lon2)))]))

; simplify with else
(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [(> (first lon1) (first lon2))
         (cons (first lon2)
               (merge lon1 (rest lon2)))]
        [else
         (cons (first lon1)
               (merge (rest lon1) lon2))]))

; =================== End of exercise ==================

; ==================== Exercise 395 ====================

; [X] [List-of X] Number -> [List-of X]
; produce the first n items from the list or all of l if it is too short
;(define (take l n) '()) ;stub

(check-expect (take '() 1) '())
(check-expect (take '(1) 1) '(1))
(check-expect (take '(1 2 3) 2) '(1 2))
(check-expect (take '(1 2 3) 5) '(1 2 3))

(define (take l n)
  (cond
    [(or (empty? l)
         (= n 0)) '()]
    [else (cons (first l)
                (take (rest l) (- n 1)))]))

; [X] [List-of X] Number -> [List-of X]
; produce a list with the first n items removed
; or just ’() if list is too short
;(define (drop l n) '()) ;stub

(check-expect (drop '() 2) '())
(check-expect (drop '(1) 1) '())
(check-expect (drop '(1 2 3 4) 2) '(3 4))
(check-expect (drop '(1 2 3 4) 5) '())

; initial implementation
#;(define (drop l n)
    (cond [(or (empty? l)
               (>= n (length l))) '()]
          [(= n 0) l]
          [else
           (drop (rest l) (- n 1))]))

; if we want simplicity over performance
; (>= n (length l) could be removed
(define (drop l n)
  (cond [(empty? l) '()]
        [(= n 0) l]
        [else
         (drop (rest l) (- n 1))]))


; =================== End of exercise ==================
