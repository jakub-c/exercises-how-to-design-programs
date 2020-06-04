;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |484|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 484 ====================

(define (infL l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (local ((define s (infL (rest l))))
            (if (< (first l) s) (first l) s))]))

; Hand-evaluate
; (infL (list 3 2 1 0))

; ==
;(cond
;  [(empty? (rest l)) #false]
;  [else (local ((define s (infL '(2 1 0))))
;          (if (< (first l) s) (first l) s))])
; ==
; evaluate all recursive calls in the (define s ...
; [else (local ((define s 0)))
;      (if (< (first l) s) (first l) s))]
; ==
; go up in the nested if calls based on the recursion above
;      (if (< 1 0) 1 0))
; ==
;      (if (< 2 0) 2 0))
; ==
;      (if (< 3 0) 3 0))
;
; result: 0

; Q: Argue that infL uses on the “order of n steps”
;     in the best and the worst case.
; A: The following line is the answer:
;     (define s (infL (rest l)))
;     the recursion needs to evaluate the whole list
;     before it moves on to the comparison
;     therefore no matter the soring the number of steps
;     will be the same


; =================== End of exercise ==================