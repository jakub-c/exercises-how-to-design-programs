;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |504|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 504 ====================

; [List-of Number] -> Number
; produce the corresponding number based on the
; list of digits provided
; first item on the list is the most significant digit
; (define (to10 lod) 0) ;stub

(check-expect (to10 '(2)) 2)
(check-expect (to10 '(1 2)) 12)
(check-expect (to10 '(1 0 2)) 102)
(check-expect (to10 '(1 0 0 2)) 1002)

(define (to10 lod0)
  (local (; [List-of Number] Number -> Number
          ; accumulator num is a sum of multiplications
          (define (to10/a lod num)
            (cond [(empty? (rest lod)) (+ num (first lod))]
                  [else
                   (to10/a (rest lod)
                           (+ num
                              (* (first lod) (expt 10 (- (length lod) 1)))))])))
    (to10/a lod0 0)))

; =================== End of exercise ==================
