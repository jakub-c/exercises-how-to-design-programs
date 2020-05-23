;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 471-475) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

; ==================== Exercise 471 ====================

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

; A Node is a Symbol
; A Graph is a [List-of [List-of Node]]

; Node Graph -> [List-of Node]
; produce the list of immediate neighbors of n in g
; (define (neighbors n g) '()) ;stub

(check-expect (neighbors 'E sample-graph)
              '(C F))
(check-expect (neighbors 'B sample-graph)
              '(E F))

(define (neighbors n g)
  (foldr (lambda (current-list return-val)
           (if (equal? n (first current-list))
               (append (rest current-list) return-val)
               return-val))
         '()
         g))

; =================== End of exercise ==================
