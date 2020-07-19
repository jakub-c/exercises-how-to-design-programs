;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |273|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [X->X] [List-of X] -> [List-of X]
; use fold to define map
(check-expect (fold-map + '(1 2 3)) (map + '(1 2 3)))

; (define (fold-map f l) '()) ;stub
(define (fold-map f l)
  (local (; X -> X
          (define (create-list i1 i2)
            (cons (f i1) i2)))
    (foldr create-list '() l)))