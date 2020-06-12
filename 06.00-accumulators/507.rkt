;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |507|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 507 ====================

; version 4
; [X Y] [X Y -> Y] Y [List-of X] -> Y
(define (f*ldl f e l0)
  (local (;[X Y] Y [List-of X] -> Y
          ; accumulator a is a value passed to the f and than
          (define (fold/a a l)
            (cond
              [(empty? l) a]
              [else
               (fold/a (f (first l) a) (rest l))])))
    (fold/a e l0)))

; ===================

; [X] Number [>= 1] [Number -> X] -> [List-of X]
; accumulator style build-list
;(define (build-l*st n f) '()) ;stub

(check-expect (build-l*st 50 add1)
              (build-list 50 add1))
(check-expect (build-l*st 10 (lambda (el) (if (> el 5) #t #f)))
              (build-list 10 (lambda (el) (if (> el 5) #t #f))))

(define (build-l*st n0 f)
  (local (; Number [List-of X] -> [List-of X]
          ; accumulator result collect all applications of
          ; function f to a number from 1 to n0
          (define (build-l*st/a n result)
            (cond [(zero? n) result]
                  [else (build-l*st/a (sub1 n)
                                      (cons (f (- n 1)) result))])))
    (build-l*st/a n0 '())))

; =================== End of exercise ==================
