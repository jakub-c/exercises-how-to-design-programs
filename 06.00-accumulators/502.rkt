;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |502|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ==================== Exercise 502 ====================

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
 (mirror (explode "abc")) (explode "abcba"))
(define (mirror s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))

; [NEList-of 1String] -> [NEList-of 1String]
; return all 1Strings but not the last one from s
; (define (all-but-last s) (explode "abc")) ;stub

(check-expect (all-but-last (explode "abc")) (explode "ab"))

(define (all-but-last s)
  (local ((define (all-but-last/a s a)
            (cond [(empty? (rest s)) a]
                  [else (all-but-last/a (rest s)
                                        (append a (list (first s))))])))
    (all-but-last/a s '())))

; [NEList-of 1String] -> 1String
; return the last 1String of s
; (define (last s) "a") ;stub

(check-expect (last (explode "abc")) "c")

(define (last s)
  (cond [(empty? (rest s)) (first s)]
        [else (last (rest s))]))

; =================== End of exercise ==================
