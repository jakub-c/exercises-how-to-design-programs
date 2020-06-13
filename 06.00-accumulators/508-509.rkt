;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |508|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right 

; ==================== Exercise 508 / 509 ====================

; 508 asks for the structural recursion and 509 asks for the
; accumulator style function.
; I ended up using an Editor as an argument of the function
; the authors might have expected using plain lists though.
; Anyway the current sollution can be considered and accumulator style.

; [List-of 1String] Number -> Editor
;(define (split-structural ed x) (make-editor "" "")) ;stub

(check-expect (split-structural (explode "aa") 8)
              (make-editor "a" "a"))
(check-expect (split-structural (explode "hello") 18)
              (make-editor "hell" "o"))
(check-expect (split-structural (explode "hello") 1)
              (make-editor "" "hello"))
(check-expect (split-structural (explode "hello") 22)
              (make-editor "hell" "o"))
(check-expect (split-structural (explode "hello") 25)
              (make-editor "hello" ""))

(define (split-structural ed0 x)
  (local (; Editor -> Editor
          ; accumulator ed keeps the current state of search
          ; between pre and post, we move a letter from
          ; post to pre per step
          (define (find-split ed)
            (local ((define p (editor-pre ed))
                    (define s (editor-post ed)))
              (cond [(<= (image-width (editor-text p))
                         x
                         (image-width (editor-text (append p (list (first s))))))
                     (make-editor (implode p) (implode s))]
                    [else
                     (find-split (make-editor
                                  (append p (list (first s)))
                                  (rest s)))]))))
    (cond [(> x (image-width (editor-text ed0))) (make-editor (implode ed0) "")]
          [else
           (find-split (make-editor '() ed0))])))

; =================== End of exercise ==================
