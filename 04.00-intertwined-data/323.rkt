;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |323|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define input2 (make-node
                15
                'd
                (make-node
                 24 'i NONE NONE)
                NONE))
(check-expect (search-bt input 15) 'd)
(check-expect (search-bt input 24) 'i)
(check-expect (search-bt input2 24) 'i)
(check-expect (search-bt input 25) #f)

; BT Number -> [Maybe Symbol]
; (define (search-bt bt number) #f) ;stub
(define (search-bt bt number)
  (cond [(no-info? bt) #false]
        [else
         (if (= (node-ssn bt) number)
             (node-name bt)
             (if (boolean? (search-bt (node-left bt) number))
                 (search-bt (node-right bt) number)
                 (search-bt (node-left bt) number)))]))

; determine whether a given number occurs in some given BT
; BT Number -> Boolean
; (define (contains-bt? bt number) #f) ; stub
(define (contains-bt? bt number)
  (cond [(no-info? bt) #false]
        [else (or (= (node-ssn bt) number)
                  (contains-bt? (node-left bt) number)
                  (contains-bt? (node-right bt) number))]))