;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |348|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Expression is one of:
; - Boolean
; - (make-bsl-and Expression Expression)
; - (make-bsl-or Expression Expression)
; - (make-bsl-not Expression)

; An And is a structure:
;  (make-bsl-and Expression Expression) 
(define-struct bsl-and [left right])

; An Or is a structure:
;  (make-bsl-or Expression Expression) 
(define-struct bsl-or [left right])

; An Or is a structure:
;  (make-bsl-or Expression) 
(define-struct bsl-not [left])

(check-expect (evaluate-bool-expression (make-bsl-not #true)) #false)
(check-expect (evaluate-bool-expression (make-bsl-or #true #false)) #true)
(check-expect (evaluate-bool-expression (make-bsl-and #true #false)) #false)

(check-expect (evaluate-bool-expression (make-bsl-and (make-bsl-not #true) #true)) #false)
(check-expect (evaluate-bool-expression (make-bsl-or (make-bsl-not #false)
                                                     (make-bsl-and #true #true))) #true)

(define (evaluate-bool-expression expr)
  (cond [(boolean? expr) expr]
        [else
         (cond [(bsl-not? expr) (not (bsl-not-left expr))]
               [(bsl-and? expr)
                (and
                 (evaluate-bool-expression (bsl-and-left expr))
                 (evaluate-bool-expression (bsl-and-right expr)))]
               [(bsl-or? expr)
                (or
                 (evaluate-bool-expression (bsl-or-left expr))
                 (evaluate-bool-expression (bsl-or-right expr)))])]))