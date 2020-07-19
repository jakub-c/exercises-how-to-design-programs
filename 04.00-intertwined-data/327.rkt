;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |327|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)




; produce a BST that is just like the original BST
; and that in place of one NONE subtree contains the new node structure

; BT Number Symbol -> BT
; (define (create-bst bt number symbol) (make-node 1 'a NONE NONE)) ;stub
(define (create-bst bt number symbol)
  (cond [(no-info? bt) NONE]
        [(< number (node-ssn bt))
         (make-node
          (node-ssn bt)
          (node-name bt)
          (if (no-info? (node-left bt))
              (make-node number symbol NONE NONE)
              (create-bst (node-left bt) number symbol))
          (node-right bt))]
        [(> number (node-ssn bt))
         (make-node
          (node-ssn bt)
          (node-name bt)
          (node-left bt)
          (if (no-info? (node-right bt))
              (make-node number symbol NONE NONE)
              (create-bst (node-right bt) number symbol)))]))

; repeatedly apply create-bst to the input list
; [List-of [List Number Symbol]] -> BST

(define input '((99 o)
                (77 l)
                (24 i)
                (10 h)
                (95 g)
                (15 d)
                (89 c)
                (29 b)
                (63 a)))

(define expect (make-node 63 'a
                          (make-node 29 'b
                                     (make-node 15 'd
                                                (make-node 10 'h NONE NONE)
                                                (make-node 24 'i NONE NONE))
                                     NONE)
                          (make-node 89 'c
                                     (make-node 77 'l NONE NONE)
                                     (make-node 95 'g NONE
                                                (make-node 99 'o NONE NONE)))))

(check-expect (create-bst-from-list input) expect)

; (define (create-bst-from-list lst) (make-node 'a 1 NONE NONE)) ;stub

(define (create-bst-from-list lst)
  (local ((define input (reverse lst))
          (define base-case (first input))
          (define list-input (reverse (rest input))))
    (foldr
     (lambda (v l)
       (create-bst l (first v) (second v)))
     (make-node (first base-case) (second base-case) NONE NONE)
     list-input)))

