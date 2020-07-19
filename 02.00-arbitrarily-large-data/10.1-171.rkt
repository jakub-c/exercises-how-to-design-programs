;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.1-171) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; List-of-string is one of:
;;  - '()
;;  - (cons String List-of-string)

; String -> List-of-string
; produces the content of file f as a list of strings, 
; one per line
(read-lines "10.1-175.txt")

(read-words "10.1-175.txt")

;; List-of-list-of-strings is one of:
;;  - '()
;;  (cons List-of-string '())

 
; String -> List-of-list-of-string
; produces the content of file f as a list of list of
; strings, one list per line and one string per word 
(read-words/line "10.1-175.txt")