;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |307|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define LIST-OF-NAMES '("Albert" "Anders" "Adalbert"))

; String [List-of String] -> [Maybe String]
(check-expect (find-name "Albert" LIST-OF-NAMES) "Albert")
(check-expect (find-name "Adalbert" LIST-OF-NAMES) "Adalbert")
(check-expect (find-name "Ada" LIST-OF-NAMES) "Adalbert")
(check-expect (find-name "Henry" LIST-OF-NAMES) #false)
 
; (define (find-name name lon) #false) ;stub

(define (find-name search-term lon)
  (for/or ([name lon])
    (for/and ([1str search-term] [i (length (explode search-term))])
      (if (equal? 1str (list-ref (explode name) i))
          name
          #false))))

; Number [List-of Names] -> Boolean
(check-expect (validate-max-length 8 LIST-OF-NAMES) #true)
(check-expect (validate-max-length 6 LIST-OF-NAMES) #false)

; (define (validate-max-length n lon) #false) ;stub
(define (validate-max-length n lon)
  (for/and ([name lon])
    (<= (length (explode name)) n)))