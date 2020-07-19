;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |287|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Inventory is a structure:
;  (make-inventory String String Number Number)
(define-struct inventory [name desc aqprice slprice])

(define inv1 (make-inventory "item1" "desc" 5 10))
(define inv2 (make-inventory "item2" "desc" 3 37))
(define inv3 (make-inventory "item4" "desc" 1 2))

; Number [List-of Inventory] -> [List-of Inventory]
; produces a list of all structures whose sales price is below ua
(check-expect (eliminate-expensive 10 (list inv1 inv2 inv3)) (list inv3))

; (define (eliminate-expensive ua l) '()) ;stub
(define (eliminate-expensive ul l)
  (filter (lambda (i) (< (inventory-slprice i) ul)) l))

; String [List-of Inventory] -> [List-of Inventory]
; produce a list of inventory records that do not use the name ty
(check-expect (recall "item1" (list inv1 inv2 inv3)) (list inv2 inv3))
; (define (recall ty l) '()) ;stub
(define (recall ty l)
  (filter (lambda (i) (not (string=? ty (inventory-name i)))) l))

; [List-of String] [List-of String] -> [List-of String]
; select names the second list that are also on the first one
(check-expect (select (list "a" "b") (list "a" "c" "d" "b"))
              (list "a" "b"))

; (define (select l1 l2) '()) ;stub

(define (select l1 l2)
  (cond [(empty? l1) '()]
        [else
         (append (filter  (lambda (n) (string=? (first l1) n)) l2)
                 (select (rest l1) l2))]))
