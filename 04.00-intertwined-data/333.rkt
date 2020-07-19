;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |333|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

(define sample-dir (make-dir "TS"
                             (list (make-dir "Text" (list "part 1" "part 2" "part 3"))
                                   "read!"
                                   (make-dir "Libs"
                                             (list (make-dir "Code" (list "hang" "draw"))
                                                   (make-dir "Docs" (list "read!")))))))


; determines how many files a given Dir.v1 contains
; Dir.v2 -> Number
(check-expect (how-many (make-dir "a" '("file1" "file2"))) 2)
(check-expect (how-many sample-dir) 7)

; (define (how-many dir) 0) ;stub

#;(define (how-many dir)
    (local (; LOFD -> Number
            (define (count-lofd l)
              (cond [(empty? l) 0]
                    [(string? l) 1]
                    [else (+ (if (dir? (first l))
                                 (how-many (first l))
                                 (count-lofd (first l)))
                             (count-lofd (rest l)))])))
      (count-lofd (dir-content dir))))

(define (how-many dir)
            (for/sum ([el (dir-content dir)])
              (if (dir? el)
                  (how-many el)
                  1)))
    