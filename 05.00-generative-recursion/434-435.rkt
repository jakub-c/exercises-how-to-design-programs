;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 434-435) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 434 ====================

; Q: What can go wrong when this version is used
;    with the quick-sort< definition from
;    Recursion that Ignores Structure?
; A: the example below shows a case where the function
;    returns the same outupt as the input
;    when combined with the pivot from quick-sort
;    it will cause an infinite loop because
;    pivot 3 will call  (smallers '(1 2 3) 3)

(check-expect (smallers '(1 2 3) 3) '(1 2 3))


; [List-of Number] Number -> [List-of Number]
(define (smallers l n)
  (cond
    [(empty? l) '()]
    [else (if (<= (first l) n)
              (cons (first l) (smallers (rest l) n))
              (smallers (rest l) n))]))

; =================== End of exercise ==================

; ==================== Exercise 435 ====================

; (rest alon) in the filter functions fixes the problem
; of triggering infinite loops like the ones in 434

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
