;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |510|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

; ==================== Exercise 510 ====================

; Number [List-of String] -> [List-of [List-of String]]
; arrange words from los in the given order into lines of maximal width w
; (define (fmt w los) '(())) ;stub

(define input
  '("velit" "accusantium" "aut" "sit" "beatae" "nihil"
            "id" "consequuntur" "maiores" "reiciendis"))

(check-expect (fmt 20 input)
              '(("velit" "accusantium" "aut")
                ("sit" "beatae" "nihil" "id")
                ("consequuntur" "maiores")
                ("reiciendis")))
(check-expect (fmt 30 input)
              '(("velit" "accusantium" "aut" "sit" "beatae")
                ("nihil" "id" "consequuntur" "maiores")
                ("reiciendis")))

(define (fmt w los0)
  (local (; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
          ; accumulator result stores the lines of text
          ; per every step either function adds a word to the last line of the result
          ; or if the line is too long it creates a new line
          (define (fmt/a los current-line result)
            (cond [(empty? los) (append result (list current-line))]
                  [else
                   (local ((define new-word (first los))
                           (define candidate (cons new-word current-line)))
                     (if (> (line-length candidate) w)
                         (fmt/a (rest los)
                                (list new-word)
                                (append result (list current-line)))
                         (fmt/a (rest los)
                                (append current-line (list new-word))
                                result)))
                   ])))
    (fmt/a los0 '() '())))

; [List-of String] -> Number
; compute the number of 1Strings in the list
; (define (line-length los) 0) ;stub

;(check-expect (line-length '()) 0)
;(check-expect (line-length '("a")) 1)
;(check-expect (line-length '("abc")) 3)
;(check-expect (line-length '("abc" "def" "g")) 7)

(define (line-length los)
  (foldr (lambda (el acc)
           (+ (length (explode el)) acc))
         0
         los))

; =================== End of exercise ==================
