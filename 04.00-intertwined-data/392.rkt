;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |392|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ==================== Exercise 392 ====================

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

;               | (empty? dirs)          | (cons? dirs)
;------------------------------------------------------
;(symbol? TOS)  | (and (symbol? TOS)     | (and (symbol? TOS)
;               |      (empty? dirs))    |     (cons? dirs))
;               |                        |
;(branch? TOS)  | (and (branch? TOS)     |  (and (branch? TOS)
;               |      (empty? dirs))    |       (cons? dirs)))

; implementation from 390
#;(define (tree-pick tos dirs)
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

; refactor based on the table
#;(define (tree-pick tos dirs)
    (cond [(and (symbol? tos)
                (empty? dirs))
           tos]
          [(and (symbol? tos)
                (cons? dirs))
           (error "directions don't match the tree structure")]
          [(and  (branch? tos)
                 (empty? dirs))
           (error "directions don't match the tree structure")]
          [(and (branch? tos)
                (cons? dirs))
           (match (first dirs)
             ['left (tree-pick (branch-left tos) (rest dirs))]
             ['right (tree-pick (branch-right tos) (rest dirs))])]))

; appy de Morgan’s laws
(define (tree-pick tos dirs)
  (cond [(and (symbol? tos)
              (empty? dirs))
         tos]
        [(or (and (symbol? tos)
                  (cons? dirs))
             (and  (branch? tos)
                   (empty? dirs)))
         (error "directions don't match the tree structure")]
        [(and (branch? tos)
              (cons? dirs))
         (match (first dirs)
           ['left (tree-pick (branch-left tos) (rest dirs))]
           ['right (tree-pick (branch-right tos) (rest dirs))])]))

; =================== End of exercise ==================