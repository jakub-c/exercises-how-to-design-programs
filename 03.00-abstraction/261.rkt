;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |260|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An IR is a structure:
;   (make-ir String Number)
(define-struct ir (name price))

; List-of IR is one of:
; - '()
; - (cons IR [List-of IR])

(define loi1 `(,(make-ir "item1" 1.2) ,(make-ir "item2" 2.2) ,(make-ir "item3" 0.4)))

(check-expect (extract1 loi1) `(,(make-ir "item3" 0.4)))
; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else   
     (local ((define below-1 (extract1 (rest an-inv))))
       (cond
         [(<= (ir-price (first an-inv)) 1.0)
          (cons (first an-inv) below-1)]
         [else below-1]))]))

