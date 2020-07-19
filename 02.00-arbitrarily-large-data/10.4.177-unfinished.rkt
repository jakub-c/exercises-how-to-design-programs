;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.4.177) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))
 
; data example 1:
; (make-editor all good)
 
; data example 2:
; (make-editor lla good)

; String String -> Editor

(define (create-editor s1 s2) (make-editor s1 s2))

(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; renders an editor as an image of the two texts
; separated by the cursor
(define (editor-render e) MT)
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))

(check-expect
  (editor-kh (create-editor "cd" "fgh") "\b")
  (create-editor "c" "fgh"))

(check-expect
  (editor-kh (create-editor "" "fgh") "\b")
  (create-editor "" "fgh"))

(check-expect
  (editor-kh (create-editor "cde" "") "\b")
  (create-editor "cd" ""))

(check-expect
  (editor-kh (create-editor "cde" "fgh") "left")
  (create-editor "cd" "efgh"))

(check-expect
  (editor-kh (create-editor "cde" "") "left")
  (create-editor "cd" "e"))

(check-expect
  (editor-kh (create-editor "" "cde") "left")
  (create-editor "" "cde"))

(check-expect
  (editor-kh (create-editor "cde" "fgh") "right")
  (create-editor "cdef" "gh"))

(check-expect
  (editor-kh (create-editor "cde" "") "right")
  (create-editor "cde" ""))

(check-expect
  (editor-kh (create-editor "" "cde") "right")
  (create-editor "c" "de"))

; (define (editor-kh ed ke) ed)(index "editor-kh") ;stub

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))
 
; Editor -> Editor
; move the character from pre to post
;(check-expect (editor-lft (make-editor

; (define (editor-lft ed) (create-editor "" "")) ;stub

(define (editor-lft ed)
  

; Editor 1String -> Editor
; insert the 1String k between pre and post
(define (editor-ins ed k)
  ed)

; main : String -> Editor
; launches the editor given some initial string
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))
