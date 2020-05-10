;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 425-429) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 425 ====================

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
; create a list of values larter than n
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
; create a list of values smaller than n
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

; =================== End of exercise ==================

; ==================== Exercise 426 ====================

; (quick-sort< '(1))
; ==
; (append (quick-sort< '())
;         (list 1)
;         (quick-sort '()))
; ==
; (append '()
;          (list 1)
;          '())
; ==
; (list 1)

(define (quick-sort<.v2 alon)
  (cond
    [(empty? alon) '()]
    [(= 1 (length alon)) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<.v2 (smallers alon pivot))
                    (list pivot)
                    (quick-sort<.v2 (largers alon pivot))))]))

(check-expect (quick-sort<.v2 '(1)) '(1))

; the additional cond check saves 2 steps

; =================== End of exercise ==================

; ==================== Exercise 427 ====================

; [List-of Number] -> [List-of Number]
; (define (sort< alon) '()) ;stub

(check-expect (sort< '()) '())
(check-expect (sort< '(1)) '(1))
(check-expect (sort< '(3 2 1)) '(1 2 3))
(check-expect (sort< '(1 4 3 2)) '(1 2 3 4))
(check-expect (sort< (list 12 20 -5)) '(-5 12 20))

(define (sort< alon)
  (cond [(empty? alon) '()]
        [(cons? alon) (insert (first alon) (sort< (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
; (define (insert n alon) alon) ;stub

(check-expect (insert 2 '(1 3)) '(1 2 3))
(check-expect (insert 1 '(3 4)) '(1 3 4))

(define (insert n alon)
  (cond [(empty? alon) (cons n '())]
        [else
         (if (<= n (first alon))
             (cons n alon)
             (cons (first alon) (insert n (rest alon))))]))

(check-expect (quick-sort<.v3 '(9 8 7 6 5 4 3 2 1))
              '(1 2 3 4 5 6 7 8 9))
(check-expect (quick-sort<.v3 '(2 3 -5))
              '(-5 2 3))

(define TRESHOLD 0)

(define (quick-sort<.v3 alon)
  (cond
    [(empty? alon) '()]
    [(= 1 (length alon)) alon]
    [(<= (length alon) TRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<.v3 (smallers alon pivot))
                    (list pivot)
                    (quick-sort<.v3 (largers alon pivot))))]))

; =================== End of exercise ==================

; ==================== Exercise 428 ====================

(check-expect (quick-sort<.v4 '(4 1 1 3 7 7))
              '(1 1 3 4 7 7))
 
(define (quick-sort<.v4 alon)
  (cond
    [(empty? alon) '()]
    [(= 1 (length alon)) alon]
    [(<= (length alon) TRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon))
                  (define all-instances-of-pivot
                    (filter (lambda (x) (= pivot x)) alon)))
            (append (quick-sort<.v4 (smallers alon pivot))
                    all-instances-of-pivot
                    (quick-sort<.v4 (largers alon pivot))))]))

; =================== End of exercise ==================

; ==================== Exercise 429 ====================

(check-expect (quick-sort<.v5 '(4 1 1 3 7 7))
              '(1 1 3 4 7 7))
 
(define (quick-sort<.v5 alon)
  (cond
    [(empty? alon) '()]
    [(= 1 (length alon)) alon]
    [(<= (length alon) TRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon))
                  (define all-instances-of-pivot
                    (filter (lambda (x) (= pivot x)) alon)))
            (append (quick-sort<.v5 (smallers.v2 alon pivot))
                    all-instances-of-pivot
                    (quick-sort<.v5 (largers.v2 alon pivot))))]))

; [List-of Number] Number -> [List-of Number]
; create a list of values larter than n
(define (largers.v2 alon n)
  (filter (lambda (el) (> el n)) alon))

 
; [List-of Number] Number -> [List-of Number]
; create a list of values smaller than n
(define (smallers.v2 alon n)
  (filter (lambda (el) (< el n)) alon))

; =================== End of exercise ==================

; ==================== Exercise 430 ====================

(check-expect (quick-sort.v6 '(4 1 1 3 7 7) <)
              '(1 1 3 4 7 7))

(define (quick-sort.v6 alon cmp)
  (cond
    [(empty? alon) '()]
    [(= 1 (length alon)) alon]
    [(<= (length alon) TRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort.v6
                     (filter (lambda (el) (cmp el pivot))
                             (rest alon))
                     cmp)
                    (list pivot)
                    (quick-sort.v6
                     (filter (lambda (el) (not (cmp el pivot)))
                             (rest alon))
                     cmp)))]))

; =================== End of exercise ==================
