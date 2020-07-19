;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |335|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)
 
(define-struct file [name size content])

; A File.v3 is a structure: 
;   (make-file String N String)


(define sample-dir (make-dir.v3 "TS"
                                (list (make-dir.v3 "Text" '()
                                                   (list "part 1" "part 2" "part 3"))
                                      (make-dir.v3 "Libs"
                                                   (make-dir.v3 "Code" '() (list "hang" "draw"))
                                                   (make-dir.v3 "Docs" '() (list "read!"))))
                                (list "read!")))
