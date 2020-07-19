;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |294|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))


(check-expect (index 0 '(1 2)) #f)
(check-expect (index 1 '(1 2)) 0)
(check-expect (index 3 '(1 2 3)) 2)

; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
 
(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
 
(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))

; X [List-of X] -> [[Maybe N] -> Boolean]
(define (is-index? x l)
  (lambda (result)
    (cond [(and (not (member? x l))
                (equal? result #f)) #t]
          [else (= (index x l)
                   result)])))

(check-expect [(is-index? 0 '(1 2)) #f] #t)
(check-expect [(is-index? 1 '(1 2)) 0] #t)

(check-satisfied (index 0 '(1 2))
                 (is-index? 0 '(1 2)))
(check-satisfied (index 4 '(1 2))
                 (is-index? 4 '(1 2)))
(check-satisfied (index 1 '(1 2))
                 (is-index? 1 '(1 2)))
(check-satisfied (index 3 '(1 2))
                 (is-index? 3 '(1 2)))