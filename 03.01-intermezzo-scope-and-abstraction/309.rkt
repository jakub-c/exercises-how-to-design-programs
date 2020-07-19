;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |309|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; determine the number of Strings per item in a list of list of strings
; [List-of [List-of String] -> [List-of Number]

; (define (words-on-line llos) '(0)) ;stub

(define input '((one two three)
                ()
                (four five six seven)))

(define expect '(3 0 4))

(check-expect (words-on-line input) expect)

#;(define (words-on-line llos)
  (for/list ([line llos])
    (length line)))

(define (words-on-line llos)
  (match llos
    [(cons lst '()) (list (length lst))]
    [(cons fst rst) (cons (length fst) (words-on-line rst))]))