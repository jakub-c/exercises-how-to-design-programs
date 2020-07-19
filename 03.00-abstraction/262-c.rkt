;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 262-c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> List
(check-expect (draw-matrix 2) (list (list 1 0) (list 0 1)))
(define (draw-matrix x)
  (draw-rows (list-from-1-to x) x))

(check-expect (draw-rows '(1 2 3) 3)
              (list (list 1 0 0)
                    (list 0 1 0)
                    (list 0 0 1)))
(define (draw-rows indexes length)
  (cond [(empty? indexes) '()]
        [else
         (cons (draw-row (first indexes) length)
               (draw-rows (rest indexes) length))]))

(check-expect (draw-row 2 2) (list 0 1))
(check-expect (draw-row 1 1) (list 1))

(define (draw-row index length)
  (cond [(= 0 length) '()]
        [else
         (add-at-end
          (if (= index length)
              1
              0)
          (draw-row index (- length 1)))]))


; Number List -> List
; add Number to the end of the List
(check-expect (add-at-end 1 '()) '(1))
(check-expect (add-at-end 3 '(1 2)) '(1 2 3))

; (define (add-at-end n l) '()) ;stub
(define (add-at-end n l)
  (cond [(empty? l) (list n)]
        [else
         (cons (first l)
               (add-at-end n (rest l)))]))


(check-expect (list-from-1-to 2) '(1 2))
(check-expect (list-from-1-to 1) '(1))
(check-expect (list-from-1-to 5) '(1 2 3 4 5))

; (define (list-from-1-to n) '())
(define (list-from-1-to n)
  (cond [(= n 1) '(1)]
        [else
         (append (list-from-1-to (- n 1))
                 (list n))]))

