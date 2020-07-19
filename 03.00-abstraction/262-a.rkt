;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 262-a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
m; Number -> List-of [List-of Number]
;create diagonal squares of 0s and 1s

(check-expect (identityM 0) '())
(check-expect (identityM 1)
              (list (list 1)))
#;(check-expect (identityM 2)
                (list (list 1 0) (list 0 1)))
#;(check-expect (identityM 3)
                (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))


; (define (identityM n) '()) ;stub
(define (identityM n)
  (create-rows n n))

; NoNZeroNumber Number -> List
; create x number of rows, of y length
(check-expect (create-rows 1 1) (list (create-row 1 1)))
(check-expect (create-rows 2 2) (list (create-row 1 2)
                                      (create-row 2 2)))

(define (create-rows x y)
  (cond [(= x 0) '()]
        [else
         (cons (create-row y x)
               (create-rows y (- x 1)))]))

; Number Number -> List
; create a row of 0s of a given length, insert 1 on the defined index position
(check-expect (create-row 1 1) '(1))
(check-expect (create-row 1 3) '(1 0 0))
(check-expect (create-row 3 3) '(0 0 1))

;(define (create-row n i) '()) ;stub
(define (create-row n i)
  (cond [(= i 0) '()]
        [else
         (add-at-end (if (= n i)
                         1
                         0)
                     (create-row n (- i 1)))]))

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
