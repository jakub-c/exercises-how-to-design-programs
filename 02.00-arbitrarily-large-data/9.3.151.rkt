;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9.3.151) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Natural Natural -> Natural
; multiply n by x
(check-expect (multiply 0 2) 0)
(check-expect (multiply 2 0) 0)
(check-expect (multiply 2 2) 4)
(check-expect (multiply 3 2) 6)

; (define (multiply n x) 0) ;stub

(define (multiply n x)
  (cond
    [(or (zero? n) (zero? x)) 0]
    [else
     (+ n (multiply n (- x 1)))]))
