;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9.6-count) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; List-of-string String -> N
; determines how often s occurs in los
(check-expect (count '() "a") 0)
(check-expect (count (cons "a" '()) "a") 1)
(check-expect (count (cons "b" (cons "a" '())) "a") 1)
(check-expect (count (cons "a" (cons "a" '())) "a") 2)
(check-expect (count (cons "c" (cons "b" '())) "a") 0)

;(define (count los s) 0) ;stub

(define (count los s)
  (cond
    [(empty? los) 0]
    [else
     (if (string=? s (first los))
         (+ 1 (count (rest los) s))
         (count (rest los) s))]))