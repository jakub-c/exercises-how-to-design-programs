;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 489-491) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))
 
; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
 
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
 
(define (add-to-each n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-to-each n (rest l)))]))

; ==================== Exercise 489 ====================

; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
 
(check-expect (cons 50 (add-to-each.v2 50 '(40 110 140 170)))
              '(50 90 160 190 220))
 
(define (add-to-each.v2 n l)
  (map (lambda (el)
         (+ n el))
       l))

; =================== End of exercise ==================

; ==================== Exercise 490 ====================

; Q: Develop a formula that describes the abstract running time
; of relative->absolute

; A: The hand evaluation is described below.
;     I think that the formula is O(n^2) because
;     there are two recursive calls per step of a function

; (relative->absolute (build-list size add1))

; size = 1

; (relative->absolute (build-list size add1))
; ==
; (relative->absolute (list 1))
; ==
;(define (relative->absolute l)
;  (cond
;    [(empty? l) '()]
;    [else (local ((define rest-of-l '())
;                  (define adjusted
;                    (add-to-each 1 '())))
;            (cons (first l) adjusted))]))
;==
; (cons 1 '())


; size = 2

; (relative->absolute (build-list size add1))
; ==
; (relative->absolute (list 1 2))
; ==
;(define (relative->absolute l)
;  (cond
;    [(empty? l) '()]
;    [else (local ((define rest-of-l
;                     (relative->absolute (list 2))
;                  (define adjusted
;                    (add-to-each 1 rest-of-l)))
;            (cons 1 adjusted))]))
;==
;(define (relative->absolute l)
;  (cond
;    [(empty? l) '()]
;    [else (local ((define rest-of-l
;                     (relative->absolute (list 2))
;                  (define adjusted
;                    (add-to-each 1 (list 2)))
;            (cons 1 adjusted))]))
;==
;(define (relative->absolute l)
;  (cond
;    [(empty? l) '()]
;    [else (local ((define rest-of-l
;                     (relative->absolute (list 2))
;                  (define adjusted
;                    (add-to-each 1 (list 2)))
;            (cons 1 (list 3))]))
;==
; (list 1 3)


; size = 3

; (relative->absolute (build-list size add1))
; ==
; (relative->absolute (list 1 2 3))
; ==
;(define (relative->absolute l)
;  (cond
;    [(empty? l) '()]
;    [else (local ((define rest-of-l
;                    (relative->absolute (list 2 3))))
;                  (define adjusted
;                    (add-to-each 1 rest-of-l)))
;            (cons 1 adjusted))]))
;==
; ==
;(define (relative->absolute l)
;  (cond
;    [(empty? l) '()]
;    [else (local ((define rest-of-l
;                    (relative->absolute
;                      (cond
;                        [(empty? l) '()]
;                        [else (local ((define rest-of-l
;                                         (relative->absolute (list 3))))
;                                       (define adjusted
;                                         (add-to-each 2 3)))
;                                (cons 2 5))])))))
;                  (define adjusted
;                    (add-to-each 1 (list 2 5)))
;            (cons 1 (list 3 6)))]))
;==
; (cons 1 3 6)

; =================== End of exercise ==================

; ==================== Exercise 491 ====================

#; (define (relative->absolute l)
 (reverse
   (foldr (lambda (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse (rest l)))))

; Q: Does your friend’s solution mean there is no need
;     for our complicated design in this motivational section?
; A: If reverse needs to recursively traverse the list in each step
;     the lambda based solution will be slower.

; =================== End of exercise ==================