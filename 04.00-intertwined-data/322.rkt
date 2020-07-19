;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |322|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define input (make-node
               15
               'd
               NONE
               (make-node
                24 'i NONE NONE)))
(check-expect (contains-bt? input 15) #t)
(check-expect (contains-bt? input 24) #t)
(check-expect (contains-bt? input 25) #f)


; determine whether a given number occurs in some given BT
; BT Number -> Boolean
; (define (contains-bt? bt number) #f) ; stub
(define (contains-bt? bt number)
  (cond ([no-info? bt] #false)
        [else (or (= (node-ssn bt) number)
                    (contains-bt? (node-left bt) number)
                    (contains-bt? (node-right bt) number))]))