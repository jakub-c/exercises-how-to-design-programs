;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |256|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...

(check-expect (argmax-v2 second '())
              '())

(check-expect (argmax-v2 second (list (list 'a 1)))
              (argmax second (list (list 'a 1))))

(check-expect (argmax-v2 second (list (list 'b 1) (list 'a 2)))
              (argmax second (list (list 'b 1) (list 'a 2))))

(check-expect (argmax-v2 second (list (list 'b 2) (list 'a 2)))
              (argmax second (list (list 'b 2) (list 'a 2))))

(check-expect (argmax-v2 second (list (list 'c 3) (list 'b 2) (list 'a 2)))
              (argmax second (list (list 'c 3) (list 'b 2) (list 'a 2))))

(define (argmax-v2 f lx)
  (cond
    [(empty? lx) '()]
    [(= 1 (length lx)) (first lx)]
    #;[(= 2 (length lx)) (if (l>= (first lx) (second lx))
                           (first lx)
                           (second lx))]
    [else
     (if (l>= (first lx) (second lx))
         (argmax-v2 f (cons (first lx) (rest (rest lx))))
         (argmax-v2 f (cons (second lx) (rest (rest lx)))))]))

(check-expect (l>= (list 'a 1) (list 'b 2))
              #f)
(check-expect (l>= (list 'a 2) (list 'b 1))
              #t)

; List List -> Boolean
(define (l>= l1 l2)
  (>= (second l1) (second l2)))