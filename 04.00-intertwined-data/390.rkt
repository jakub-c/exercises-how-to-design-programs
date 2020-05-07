;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |390|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N is one of: 
; – 0
; – (add1 N)

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
#;(define (list-pick l n)
'a)

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0))
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3))

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
(define (list-pick l n)
  (cond
    [(and (= n 0) (empty? l))
     (error 'list-pick "list too short")]
    [(and (> n 0) (empty? l))
     (error 'list-pick "list too short")]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))

; ==================== Exercise 390 ====================

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

; TOS [List-of Direction] -> TOS
; consume a tree of symbols and a list of directions
; (define (tree-pick tos dirs) 'x) ;stub

(check-expect (tree-pick 'a '()) 'a)
(check-error (tree-pick 'a '(left)))
(check-expect (tree-pick (make-branch 'a 'b) '(left))
              'a)
(check-expect (tree-pick (make-branch 'a 'b) '(right))
              'b)
(check-error (tree-pick (make-branch
                         (make-branch 'a 'b)
                         'c)
                        '(left)))
(check-expect (tree-pick (make-branch
                          (make-branch 'a 'b)
                          'c)
                         '(left left))
              'a)
(check-expect (tree-pick (make-branch
                          (make-branch 'a
                                       (make-branch 'b 'c))
                          'd)
                         '(left right right))
              'c)

(define (tree-pick tos dirs)
  (cond [(and (symbol? tos)
              (empty? dirs)) tos]
        [(and (symbol? tos)
              (not (empty? dirs)))
         (error "directions don't match the tree structure")]
        [(and (branch? tos)
              (empty? dirs))
         (error "directions don't match the tree structure")]
        [(and (branch? tos)
              (equal? (first dirs) 'left))
         (tree-pick (branch-left tos) (rest dirs))]
        [(and (branch? tos)
              (equal? (first dirs) 'right))
         (tree-pick (branch-right tos) (rest dirs))]))

; =================== End of exercise ==================
