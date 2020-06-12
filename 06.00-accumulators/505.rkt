;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |505|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 505 ====================

; N [>=1] -> Boolean
; determines whether n is a prime number
(check-expect (is-prime? 2) #t)
(check-expect (is-prime? 283) #t)
(check-expect (is-prime? 284) #f)


(define (is-prime? n0)
  (local (; N [>=1] N [>=1] -> Boolean
          ; accumulator number-to-check represents number from (- n 1) to 1
          ; we divide n by each end every one of those numbers
          (define (is-prime/a n number-to-check)
            (cond
              [(= number-to-check 1) #t]
              [else (if (= 0
                           (modulo n number-to-check))
                        #false
                        (is-prime/a n (sub1 number-to-check)))])))
    (is-prime/a n0 (- n0 1))))

; =================== End of exercise ==================
