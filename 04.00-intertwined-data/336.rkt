;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |336|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
                                                   (list (make-dir.v3 "Code" '() (list "hang" "draw"))
                                                         (make-dir.v3 "Docs" '() (list "read!")))
                                                   '()))
                                (list "read!")))

; determines how many files a given Dir.v1 contains
; Dir.v3 -> Number
(check-expect (how-many sample-dir) 7)

; (define (how-many dir) 0) ;stub

(define (how-many dir)
  (local ((define (count-in-dirs lod)
            (cond [(empty? lod) 0]
                  [else (+ (length (dir.v3-files (first lod)))
                           (count-in-dirs (dir.v3-dirs (first lod)))
                           (count-in-dirs (rest lod)))])))
    (+ (count-in-dirs (dir.v3-dirs dir))
       (length (dir.v3-files dir)))))