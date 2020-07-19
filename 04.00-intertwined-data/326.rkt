;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |326|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define expect (make-node 63 'a
                          (make-node 29 'b
                                     (make-node 15 'c
                                                (make-node 10 'h
                                                           (make-node 5 'z NONE NONE)
                                                           NONE)
                                                (make-node 24 'i NONE NONE))
                                     NONE)
                          (make-node 89 'd
                                     (make-node 77 'e NONE NONE)
                                     (make-node 95 'f NONE
                                                (make-node 99 'g NONE NONE)))))

(define expect2 (make-node 63 'a
                         (make-node 29 'b
                                    (make-node 15 'c
                                               (make-node 10 'h NONE NONE)
                                               (make-node 24 'i NONE NONE))
                                    NONE)
                         (make-node 89 'd
                                    (make-node 77 'e NONE NONE)
                                    (make-node 95 'f NONE
                                               (make-node 99 'g NONE
                                                          (make-node 101 'x NONE NONE))))))

(check-expect (create-bst input 5 'z) expect)
(check-expect (create-bst input 101 'x) expect2)


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