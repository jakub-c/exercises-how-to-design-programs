;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 5.10-graphical-editor) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

; Editor -> Image
; render text stored in Editor pre and post
; add a cursor in between
(check-expect (render (make-editor "hello" "world"))
              (overlay
               (beside
                (text "hello" 12 "black")
                (rectangle 1 20 "solid" "red")
                (text "world" 12 "black"))
               (empty-scene 200 20)))

(check-expect (render (make-editor "" "world"))
              (overlay
               (beside
                (text "" 12 "black")
                (rectangle 1 20 "solid" "red")
                (text "world" 12 "black"))
               (empty-scene 200 20)))

; (define (render e) empty-image) ;stub
 
; (define (render e)
;  (... (editor-pre e) ... (editor-post e) ...)) ; template

(define (render e)
  (overlay
   (beside
    (text (editor-pre e) 12 "black")
    (rectangle 1 20 "solid" "red")
    (text (editor-post e) 12 "black"))
   (empty-scene 200 20)))

; Editor KeyEvent -> Editor
; add a character to pre field of Editor
; when "\b" (backspace) is pressed remove a character from pre field
; igrnore "\t" (tab) and "\r" return
(check-expect (edit (make-editor "hello" "world") " ")
              (make-editor "hello " "world"))
(check-expect (edit (make-editor "" "") "1")
              (make-editor "1" ""))
(check-expect (edit (make-editor "hello" "world") "\b")
              (make-editor "hell" "world"))
(check-expect (edit (make-editor "hello" "world") "\r")
              (make-editor "hello" "world"))

; (define (edit ed ke) (make-editor "hello" "world")) ;stub

(define (edit ed ke)
  (cond [(or
          (string=? ke "\t")
          (string=? ke "\r")
          (string=? ke "left")
          (string=? ke "right")) ed]
[(string=? ke "\b") (make-editor (remove-last-char (editor-pre ed)) (editor-post ed))]
[else (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]))

; Editor -> Editor
; remove the last character from the given string
(check-expect (remove-last-char "hello") "hell")
(check-expect (remove-last-char "h") "")
(check-expect (remove-last-char "") "")

; (remove-last-char ed) ed) ;stub

(define (remove-last-char ed)
  (if (= (string-length ed) 0)
      ed
      (substring
       ed
       0
       (- (string-length ed) 1))))


(define (main ed)
  (big-bang ed
    (on-key edit)
    (to-draw render)))

(main (make-editor "" ""))