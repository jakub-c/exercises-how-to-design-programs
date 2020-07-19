;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.1-172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An LLS is one of: 
; – '()
; – (cons Los LLS)
; interpretation a list of lines, each is a list of Strings
 
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

#; (define (fn-for-lls lls)
  (cond
    [(empty? lls) ...]
    [else
     (... (first lls)
          (fn-for-lls (rest lls)))]))

; LLS -> String
; convert a list of lines into a string
(check-expect (collapse lls0) "\n")
(check-expect (collapse lls1) "hello world \n")

; (define (collapse lls) " ") ;stub

(define (collapse lls)
  (cond
    [(empty? lls) "\n"]
    [else
     (string-append
      (append-line (first lls))
      (collapse (rest lls)))]))

; Los -> String
; append strings in a line
(check-expect (append-line line1) "")
(check-expect (append-line line0) "hello world ")

; (define (append-line los) " ") ;stub

(define (append-line los)
  (cond
    [(empty? los) ""]
    [else
     (string-append
      (first los)
      " "
      (append-line (rest los)))]))