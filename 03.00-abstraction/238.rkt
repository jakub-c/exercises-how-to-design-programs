;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |238|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
    
; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; Nelon -> Number
; exctract an element from the list that

; (define (extract F l) '()) ;stub

(define (extract F l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (if (F (first l) (extract F (rest l)))
         (first l)      
         (extract F (rest l)))]))

; Nelon -> Number
; determines the lowest number in the list
(check-expect (inf-1 '(1)) 1)
(check-expect (inf-1 '(5 7 1 8)) 1)

; (define (inf-1 l) '()) ;stub
(define (inf-1 l)
  (extract < l))

; Nelon -> Number
; deterines the highest number in the list
;(check-expect (sup-1 '(1)) 1)
(check-expect (sup-1 '(5 7 1 8)) 8)
#;(check-expect (sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                             12 11 10 9 8 7 6 5 4 3 2 1))
                25)

; (define (sup-1 l) '()) ;stub
(define (sup-1 l)
  (extract > l))

; Nelon -> Number
; exctract an element from the list that

(define (extract-2 F l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (F (first l) (extract-2 F (rest l)))]))

; Nelon -> Number
; determines the lowest number in the list
(check-expect (inf-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                             12 11 10 9 8 7 6 5 4 3 2 1))
              1)
                
(define (inf-2 l)
  (extract-2 min l))

; Nelon -> Number
; determines the highest number in the list
(check-expect (sup-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
                             12 11 10 9 8 7 6 5 4 3 2 1))
              25)
                
(define (sup-2 l)
  (extract-2 max l))