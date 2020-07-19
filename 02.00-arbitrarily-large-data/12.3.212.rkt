;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 12.3.212) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

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
(check-expect (in-dictionary (list "cad") (list "cat" "act"))
              '())

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
(define WORD1 (list "c" "a" "t"))
(define WORD2 (list "c" "t" "a"))
(define WORD3 (list "a" "c" "t"))
(define WORD4 (list "a" "t" "c"))
(define WORD5 (list "t" "a" "c"))
(define WORD6 (list "t" "c" "a"))
 
; A List-of-words is one of:
; - '() or
; - (cons Word List-of-words)
(define LOW1 (list WORD1 WORD2 WORD3 WORD4 WORD5 WORD6))
 
; Word -> List-of-words
; creates all rearrangements of the letters in w
(check-expect (arrangements '()) (list '()))
(check-expect (and
               (member WORD1 (arrangements WORD1))
               (member WORD2 (arrangements WORD1))
               (member WORD3 (arrangements WORD1))
               (member WORD4 (arrangements WORD1))
               (member WORD5 (arrangements WORD1))
               (member WORD6 (arrangements WORD1)))
              #true)

#;(define (arrangements word)
    (list word)) ;stub

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

; List-of-strings -> Boolean 
(define (all-words-from-cat? w)
  (and (member? (list "c" "a" "t") w)
       (member? (list "a" "c" "t") w)
       (member? (list "a" "t" "c") w)
       (member? (list "c" "t" "a") w)
       (member? (list "t" "c" "a") w)
       (member? (list "t" "a" "c") w)))

; 1String List-of-words -> List-of-words
; produce a list of words like its second argument, but with the first argument inserted at the beginning,
; between all letters, and at the end of all words of the given list
(check-expect (insert-everywhere/in-all-words "c" (list '())) (list (list "c")))
(check-satisfied (insert-everywhere/in-all-words "c" (list (list "a" "t") (list "t" "a")))
                 all-words-from-cat?)

; (define (insert-everywhere/in-all-words letter words) (list '())) ;stub

(define (insert-everywhere/in-all-words letter words)
  (cond
    [(empty? words) '()]
    [else
     (append (insert-everywhere '() letter (first words))
             (insert-everywhere/in-all-words letter (rest words)))]))

; Word 1String Word -> List-of-words
; produce a list of words like its second argument, but with the first argument inserted at the beginning,
; between all letters, and at the end of all words of the given word
(check-expect (insert-everywhere '() "c" '()) (list (list "c")))
(check-expect (insert-everywhere '() "c" (list "a" "t"))
              (list
               (list "c" "a" "t")
               (list "a" "c" "t")
               (list "a" "t" "c")))

; (define (insert-everywhere pre letter post) (list '())) ;stub
(define (insert-everywhere pre letter post)
  (cond
    [(empty? post) (list (merge-word pre letter post))]
    [else
     (cons (merge-word pre letter post)
           (insert-everywhere (merge-word pre (first post) '())
                              letter
                              (rest post)))]))

; Word 1String Word -> Word
; insert letter between pre and post lists
(check-expect (merge-word '() "c" '()) (list "c"))
(check-expect (merge-word '() "c" (list "a" "t")) (list "c" "a" "t"))
(check-expect (merge-word (list "a") "c" (list "t")) (list "a" "c" "t"))
(check-expect (merge-word (list "a" "t") "c" '()) (list "a" "t" "c"))

 
; (define (merge-word pre letter rpost) '()) ;stub
(define (merge-word pre letter post)
  (append pre (cons letter '()) post))

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
