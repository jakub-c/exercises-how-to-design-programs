;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |325|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define input (make-node 63 'a
                         (make-node 29 'b
                                    (make-node 15 'c
                                               (make-node 10 'h NONE NONE)
                                               (make-node 24 'i NONE NONE))
                                    NONE)
                         (make-node 89 'd
                                    (make-node 77 'e NONE NONE)
                                    (make-node 95 'f NONE
                                               (make-node 99 'g NONE NONE)))))

(check-expect (search-bst 24 input) 'i)
(check-expect (search-bst 99 input) 'g)
(check-expect (search-bst 63 input) 'a)
(check-expect (search-bst 101 input) NONE)


; if the tree contains a node whose ssn field is n,
; produce the value of the name field in that node othwrwise NONE

; Number BST -> Node
; (define (search-bst n bst) '()) ;stub

(define (search-bst n bst)
  (cond [(no-info? bst) NONE]
        [(= n (node-ssn bst)) (node-name bst)]
        [(< n (node-ssn bst)) (search-bst n (node-left bst))]
        [(> n (node-ssn bst)) (search-bst n (node-right bst))]))