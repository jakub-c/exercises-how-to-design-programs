;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |331|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

(define sample-dir '(("part 1" "part 2" "part 3")
                     ("read!")
                     (("hang" "draw")
                      ("read!"))))

; determines how many files a given Dir.v1 contains
; Dir.v1 -> Number
(check-expect (how-many sample-dir) 7)

; (define (how-many dir) 0) ;stub

(define (how-many dir)
  (cond [(empty? dir) 0]
        [else
         (+ (if (string? (first dir))
                1
                (how-many (first dir)))
            (how-many (rest dir)))]))