;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |271|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String List-of-String -> Boolean
; find name in a given list
(check-expect (find-name "albert" (list "albert" "alfred" "anders" "albert")) #true)

; (define (find-name n los) #false) ;stub
(define (find-name n los)
  (local (; String -> String
          ; check if name equals n
          (define (is-n? name) (string=? n name)))
    (ormap is-n? los)))


; 1String List-of-Names -> Boolean
; checks if all names on a list of names start with the letter n
(check-expect (check-if-starts-with "a" (list "albert" "anders" "bert")) #false)
(check-expect (check-if-starts-with "a" (list "albert" "anders")) #true)

; (define (check-if-starts-with n los) #false) ;stub
(define (check-if-starts-with n los)
  (local (; 1String String -> Boolean
          (define (word-starts-with-n? string)
            (string=? n (first (explode string)))))
    (andmap word-starts-with-n? los)))


; one should use andmap to o define a function that
; ensures that no name on some list exceeds a given width
; because we want to make sure that this check applies to ALL items on the list