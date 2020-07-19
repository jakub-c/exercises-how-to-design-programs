;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |324|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [number left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number BT BT)

(define input (make-node 63
               (make-node 29
                          (make-node 15
                                     (make-node 10 NONE NONE)
                                     (make-node 24 NONE NONE))
                          NONE)
               (make-node 89
                          (make-node 77 NONE NONE)
                          (make-node 95 NONE
                                     (make-node 99 NONE NONE)))))

(check-expect (inorder input) '(10 15 24 29 63 77 89 95 99))
; consume a BT and produce the sequence of all the numbers
; in the tree as they show up from left to right
; BT -> [List-of Number]
; (define (inorder bt) '()) ;stub

(define (inorder bt)
  (cond [(no-info? bt) '()]
        [else
         (append
          (inorder (node-left bt))
          (list (node-number bt))
          (inorder (node-right bt)))]))
