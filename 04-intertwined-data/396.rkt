;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |396|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/abstraction)
(require 2htdp/batch-io)

; ==================== Exercise 396 ====================

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed

(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))


; HM-Word HM-Word HM-Word -> S?
; (define (compare-word to-be-guessed current-state current-guess) '()) ;stub
(check-expect (compare-word
               (explode "ok")
               (explode "o_")
               "k")
              (explode "ok"))
(check-expect (compare-word
               (explode "albert")
               (explode "_____t")
               "a")
              (explode "a____t"))
(check-expect (compare-word
               (explode "albert")
               (explode "______")
               "z")
              (explode "______"))
(check-expect (compare-word
               (explode "pass")
               (explode "____")
               "s")
              (explode "__ss"))


(define (compare-word to-be-guessed current-state current-guess)
  (local (; 1String HM-Word -> 1String
          ; check if the current-guess is behind the _ (blank)
          (define (check-blank char answer)
            (match char
              ("_" (if (string=? current-guess answer)
                       answer
                       "_"))
              (else char))))
    (map check-blank current-state to-be-guessed)))

; play the game:
; (define LOCATION "/usr/share/dict/words") ; on OS X
; (define AS-LIST (read-lines LOCATION))
; (define SIZE (length AS-LIST))
 
; (play (list-ref AS-LIST (random SIZE)) 20)

; =================== End of exercise ==================
