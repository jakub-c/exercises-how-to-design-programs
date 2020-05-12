;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |438|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

; ==================== Exercise 438 ====================

; Q: In your words: how does greatest-divisor-<= work?
;    Use the design recipe to find the right words.
;    Why does the locally defined greatest-divisor-<= recur on (min n m)?

; A: Example:
;    (gcd-structural 10 4)
;    pass 4 to greatest-divisor-<= because common divisor can't be
;    higher than that
;    divide 10 and 4 by 4
;    the (remainder n i) returns 2 so we need to look further
;    call greatest-divisor-<= again but pass 3
;    10 and 4 can't be divided by 3 without any remainder so
;    we need to look further
;    10 and 4 can be divited by 2 with no remainder so the answer is 2

; =================== End of exercise ==================

; ==================== Exercise 439 ====================

; (time (gcd-structural 101135853 45014640))
; cpu time: 5917 real time: 5941

; =================== End of exercise ==================

; ==================== Exercise 440 ====================

(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S)) 
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

; (time (gcd-generative 101135853 45014640))
; cpu time: 0 real time: 1

; =================== End of exercise ==================

; ==================== Exercise 441 ====================

; Evaluate
; (quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
; by hand

; (quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
; ==
; (append
;   (quick-sort< (list 6 8 9 3 2))
;   (list 10)
;   (quick-sort< (list 14 12 11 14 16)))
; ==
; (append
;   (append
;     (append
;      (quick-sort< (list 2))
;      (list 3)
;      (quick-sort< '()))
;     (list 6)
;     (append
;      (quick-sort< '())
;       (list 8)
;      (quick-sort< (list 9)))
;   (list 10)
;   (append
;     (append
;       (quick-sort< (list 11))
;       (list 12)
;       (quick-sort< '()))
;     (list 14 14)
;     (quick-sort< (list 16)))


; Evaluate
; (quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
; by hand

; (quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
; ==
; (append
;  (quick-sort< '())
;  (list 1)
;  (append
;    (quick-sort< '())
;    (list 2)
;    (quick-sort< (list 3 4 5 6 7 8 9 10 11 12 13 14)))
;
; and so on and so forth, we'll be branching out for
; each and every number, suddenly quick-sort is not so quick anymore

; =================== End of exercise ==================

; ==================== Exercise 442 ====================

(define TRESHOLD 3)

(define (quick-sort alon cmp)
  (cond
    [(empty? alon) '()]
    [(= 1 (length alon)) alon]
    [(<= (length alon) TRESHOLD) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort
                     (filter (lambda (el) (cmp el pivot))
                             (rest alon))
                     cmp)
                    (list pivot)
                    (quick-sort
                     (filter (lambda (el) (not (cmp el pivot)))
                             (rest alon))
                     cmp)))]))

(define (sort< alon)
  (cond [(empty? alon) '()]
        [(cons? alon) (insert (first alon) (sort< (rest alon)))]))

(define (insert n alon)
  (cond [(empty? alon) (cons n '())]
        [else
         (if (<= n (first alon))
             (cons n alon)
             (cons (first alon) (insert n (rest alon))))]))


(check-expect (quick-sort '(9 8 7 6 5 4 3 2 1) <)
              '(1 2 3 4 5 6 7 8 9))
(check-expect (sort< '(9 8 7 6 5 4 3 2 1))
              '(1 2 3 4 5 6 7 8 9))

(require 2htdp/abstraction)

; Number -> [List-of Number]
(define (create-test length)
  (for/list ([el length])
    (random length)))

(check-random (create-test 4)
              (list (random 4) (random 4) (random 4) (random 4)))

; (define sort-test-case-long (create-test 2000))

; (time (quick-sort sort-test-case-long <))
; cpu time: 43 real time: 43 gc time: 26

; (time (sort< sort-test-case-long))
; cpu time: 1503 real time: 1520 gc time: 65

(define sort-test-case-short (create-test 15))

(time (quick-sort sort-test-case-short <))
; cpu time: 0 real time: 1 gc time: 0

(time (sort< sort-test-case-short))
; cpu time: 0 real time: 0 gc time: 0

; the values oscilate around 0 and 1 for smaller
; values so it's quite hard to make a reasonable test
; on the hardware I'm using at the moment

; =================== End of exercise ==================

; ==================== Exercise 443 ====================

#;(define (gcd-structural n m)
  (cond
    [(and (= n 1) (= m 1)) ...]
    [(and (> n 1) (= m 1)) ...]
    [(and (= n 1) (> m 1)) ...]
    [else
     (... (gcd-structural (sub1 n) (sub1 m)) ...
      ... (gcd-structural (sub1 n) m) ...
      ... (gcd-structural n (sub1 m)) ...)]))

; Q: Why is it impossible to find a divisor with this strategy?
;    (the template listed above)
; A: Because we want to terminate when
;    "(= (remainder n i) (remainder m i) 0)"
;    not when the whole list is processed

; ==================== Exercise 444 ====================

#;(define (gcd-structural S L)
  (largest-common (divisors S S) (divisors S L)))
 
; N[>= 1] N[>= 1] -> [List-of N]
; computes the divisors of l smaller or equal to k
#; (define (divisors k l)
  '())
 
; [List-of N] [List-of N] -> N
; finds the largest number common to both k and l
#; (define (largest-common k l)
  1)

; Q: Why do you think divisors consumes two numbers?
;    Why does it consume S as the first argument in both uses?
; A: divisors consume two numbers so we can iterate over the
;    list from k to 1
;    S is consumed first because we want to limit the number
;    of searches - it doesn't make sense to look for divisors
;    bigger than S

; =================== End of exercise ==================
