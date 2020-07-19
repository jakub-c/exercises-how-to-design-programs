;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |257|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Function -> List

(check-expect (build-l*st 0 add1)
              (build-list 0 add1))
(check-expect (build-l*st 1 add1)
              (build-list 1 add1))
(check-expect (build-l*st 22 add1)
              (build-list 22 add1))

; (define (build-l*st n f) '()) ;stub
(define (build-l*st n f)
    (reverse (build-rev-list n f)))

; Number Function -> List
; builds a list in a reverse order
(define (build-rev-list n f)
    (cond
      [(= 0 n) '()]
      [else
       (cons
        (f (sub1 n))
        (build-rev-list (sub1 n) f))]))