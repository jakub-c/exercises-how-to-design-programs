;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |506|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 506 ====================

; [Any -> Any] [List-of Any] -> [List-of Any]
; accumulator style version of a map function
; (define (acc-map f l) '()) ;stub

(check-expect (acc-map add1 '(1 2 3))
              (map add1 '(1 2 3)))
(check-expect (acc-map (lambda (el) (if (zero? el) #t #f)) '(0 1 0))
              (map (lambda (el) (if (zero? el) #t #f)) '(0 1 0)))

(define (acc-map f l0)
  (local (;[List-of Any] -> [List-of Any]
          ; acumulator result stores the results of each
          ; application of a function f to the item on the list
          (define (acc-map/a l result)
            (cond [(empty? l) result]
                  [else
                   (cons (f (first l))
                         (acc-map/a (rest l) result))])))
    (acc-map/a l0 '())))

; =================== End of exercise ==================
