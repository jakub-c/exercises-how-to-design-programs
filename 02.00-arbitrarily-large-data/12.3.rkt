;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.3|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; String -> List-of-strings
; finds all words that use the same letters as s
(check-expect (and
               (member "cat" (alternative-words "cat"))
               (member "cat" (alternative-words "act")))
              #true)

(define (alternative-words s) (in-dictionary (arrangements s)))
 
; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low) '())
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
;(define (in-dictionary los) '())(index "in-dictionary")

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
 
; A List-of-words is ...
 
; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements word)
  (list word))


; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "") '())
(check-expect (string->word "abc") (list "a" "b" "c"))

(define (string->word s) (explode s))
 
; Word -> String
; converts w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "a" "b" "c")) "abc")

(define (word->string w) (implode w))