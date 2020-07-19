;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |286|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Inventory is a structure:
;  (make-inventory String String Number Number)
(define-struct inventory [name desc aqprice slprice])

(define inv1 (make-inventory "item1" "desc" 5 10))
(define inv2 (make-inventory "item2" "desc" 3 37))
(define inv3 (make-inventory "item4" "desc" 1 2))

; [List-of Inventory] -> [List-of Inventory]
; sort inventory by difference between the aquisition price and sales price
; in descensing order
(check-expect (sort-inv (list inv1 inv2 inv3)) (list inv2 inv1 inv3))
; (define (sort-inv l) '()) ;stub

(define (sort-inv l)
  (local (; Inventory -> Number
          ; difference between sales and aquisition price
          (define (diff-price i) (- (inventory-slprice i)
                                    (inventory-aqprice i))))
    (sort l (lambda (i1 i2) (> (diff-price i1)
                               (diff-price i2))))))
