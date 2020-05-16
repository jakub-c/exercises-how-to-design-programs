;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 452-453) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; File -> [List-of Line]
; converts a file into a list of lines 
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))
 
; ==================== Exercise 452 ====================

; File -> Line
; produce a Line by cons'ing a list of input 1Strings
; if NEWLINE delimiter is encountered stop
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))
 
; File -> Line
; remove 1Strings from a list unless
; NEWLINE delimiter is encountered
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
 
(define NEWLINE "\n") ; the 1String 

; =================== End of exercise ==================

; ==================== Exercise 453 ====================

; Token is one of:
; - String
; - 1String
; condition: Token can only contain lowercase letters

; Line -> [List-of Token]
; turn a Line into a list of Tokens

; helper answers to generative recursion questions
; 1. The problem is trivially solvable if the Line is '() (empty)
; 2. In that case there are no Tokens in a line
; 3. Otherwise the Line contains at least one Token. The first Token
;    in the line needs to be found and excluded from the Line.
;    The remainder is the new problem of the same kind that needs to be
;    passed to the `tokenize` function again. 
; 4. It than suffices to append all found Tokens to get the final
;    result of the function.


; (define (tokenize l) '()) ;stub

(check-expect (tokenize (explode "hello"))
              '("hello"))
(check-expect (tokenize (explode "hello world"))
                '("hello" "world"))
(check-expect (tokenize (explode "hello   world. \n hello world!"))
                '("hello" "world." "hello" "world!"))

(define (tokenize l)
  (cond [(empty? l) '()]
        [else
         (append (get-first-token l)
                 (tokenize (drop-first-token l)))]))

; Line -> Token
; produce the first token encountered in a Line
; (define (get-first-token l) '()) ;stub

(check-expect (get-first-token (explode ""))
              '())
(check-expect (get-first-token (explode "hello   world. \n hello world!"))
              (list "hello"))

(define (get-first-token l)
  (local ((define (first-token l)
            (cond [(empty? l) ""]
                  [(string-whitespace? (first l)) ""]
                  [else
                   (string-append (first l)
                                  (first-token (rest l)))]))
          (define result (first-token l)))
    (if (string=? result "")
        '()
        (list result))))

; Line -> Line
; remove the first token encountered in a Line
; (define (drop-first-token l) '()) ;stub

(check-expect (drop-first-token (explode ""))
              (explode ""))
(check-expect (drop-first-token (explode "hello"))
              (explode ""))
(check-expect (drop-first-token (explode "hello   world. #\newline hello world!"))
              (explode "  world. #\newline hello world!"))


(define (drop-first-token l)
  (cond [(empty? l) '()]
        [(string-whitespace? (first l)) (rest l)]
        [else
         (drop-first-token (rest l))]))

; =================== End of exercise ==================
