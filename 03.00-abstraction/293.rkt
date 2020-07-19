;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |293|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

(check-expect (find 2 '()) #f)
(check-expect (find 4 '(1 2 3)) #f)
(check-expect (find 2 '(1 2 3)) '(2 3))
(check-expect (find 1 '(1 2 3)) '(1 2 3))
(check-expect (find 3 '(1 2 3)) '(3))

; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
 
(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
 
(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))

;(check-expect [(found? 2 '()) #f] #f) 

; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
(define (found? x l)
  (lambda (result)
    (cond [(and (equal? result #f) (empty? l)) #t]
          [(and (equal? result #f) (not (member? x l))) #t]
          [else
           (and
            (= x (first result))
            (contains? l result))])))

(check-satisfied (find 2 '())
                 (found? 2 '()))

(check-satisfied (find 4 '(1 2 3))
                 (found? 4 '(1 2 3)))

(check-satisfied (find 2 '(1 2 3))
                 (found? 2 '(1 2 3)))