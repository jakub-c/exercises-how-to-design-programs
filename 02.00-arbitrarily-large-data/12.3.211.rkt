;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.3.2011) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
; (define AS-LIST (read-lines LOCATION))

; String -> List-of-strings
; finds all words that use the same letters as s
(check-expect (and
               (member "cat" (alternative-words "cat" (list "cat" "act")))
               (member "cat" (alternative-words "act" (list "cat" "act"))))
              #true)

(define (alternative-words s dict)
  (in-dictionary
   (words->strings
    (arrangements (string->word s))) dict))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (and
               (member "cat" (in-dictionary (list "cat" "cta" "act" "tac") (list "cat" "act")))
               (member "act" (in-dictionary (list "cat" "cta" "act" "tac") (list "cat" "act"))))
              #true)

; (define (in-dictionary los dict) '()) ;stub
(define (in-dictionary los dict)
  (cond
    [(empty? los) '()]
    [else
     (if (member (first los) dict)
         (cons
          (first los)
          (in-dictionary (rest los) dict))
         (in-dictionary (rest los) dict))]))
  
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

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings '()) '())
(check-expect (words->strings (list (list "a" "b" "c") (list "b" "c" "d"))) (list "abc" "bcd"))

; (define (words->strings low) '()) ;stub
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else
     (cons (implode (first low))
           (words->strings (rest low)))]))
