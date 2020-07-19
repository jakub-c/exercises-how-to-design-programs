;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 257-intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct address [first-name last-name street])
; An Addr is a structure: 
;   (make-address String String String)
; interpretation associates an address with a person's name

(define a1 (make-address "jakub" "c"  "street"))
(define a2 (make-address "john" "d"  "street"))
(define a3 (make-address "albert" "e"  "street"))

(define lst (list a1 a2 a3))

; extract the first names from the given list of Addr
; List-of Addr -> List-of String
;(check-expect (extract-fnames '()) '())
;(check-expect (extract-fnames lst) '("jakub" "john" "albert"))

; (define (extract-fnames l) l) ;stub
(define (extract-fnames l)
  (cond [(empty? l) '()]
        [else
         (cons (address-first-name (first l))
               (extract-fnames (rest l)))]))

; sorts names in alphabetical order
; List-of String -> List-of String
;(check-expect (sort-names '()) '())
;(check-expect (sort-names (list "a")) (list "a"))
;(check-expect (sort-names (list "a" "b" "c")) (list "a" "b" "c"))
;(check-expect (sort-names (list "c" "b" "a")) (list "a" "b" "c"))

; (define (sort-names l) l) ;stub
(define (sort-names l)
  (cond [(empty? l) '()]
        [(= 1 (length l)) (list (first l))]
        [else
         (insert (first l)
                 (sort-names (rest l)))]))
; String List-of String -> List-of String
; put a string in the proper place in the list
(check-expect (insert "a" '()) (list "a"))
(check-expect (insert "a" (list "b" "c")) (list "a" "b" "c"))
(check-expect (insert "b" (list "a" "c")) (list "a" "b" "c"))

;(define (insert s l) l) ;stub
(define (insert s l)
  (cond [(empty? l) (list s)]
        [else
         (if (string<? s (first l))
             (cons s l)
             (cons (first l) (cons s (rest l))))]))


; concatenate the list of String
; List-of String -> String
(check-expect (concat '()) "")
(check-expect (concat (list "a")) "a")
(check-expect (concat (list "a" "b")) "a b")

; (define (concat l) "") ;stub
(define (concat l)
  (cond
    [(empty? l) ""]
    [(= 1 (length l)) (first l)]
    [else
     (string-append (first l) " " (concat (rest l)))]))

; List-of Addr -> String
; turn a list of addresses into one string
(check-expect (listing '()) "")
(check-expect (listing lst) "albert jakub john")

; (define (listing l) "") ; stub

(define (listing l)
  (concat (sort-names (extract-fnames l))))