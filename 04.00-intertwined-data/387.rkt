;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |387|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; ==================== Exercise 387 ====================

; [List-of Symbol] [List-of Number] -> [List-of '(Symbol Number))
; produces all possible ordered pairs of symbols and numbers
; (define (cross los lon) '(())) ;stub

(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

(define (cross los lon)
  (cond [(empty? los) '()]
        [else
         (local ((define (pair-with-numbers sym lon)
                   (cond [(empty? lon) '()]
                         [else
                          (cons (list sym (first lon))
                                (pair-with-numbers sym (rest lon)))])))
           (append (pair-with-numbers (first los) lon)
                   (cross (rest los) lon)))]))

; =================== End of exercise ==================
