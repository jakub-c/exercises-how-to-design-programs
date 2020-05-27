;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |109|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; ==================== Exercise 109 ====================

(require 2htdp/image)

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

(define MTS (empty-scene 100 100))

; Sequence is one of:
; - AA
; - BB
; - DD
; - ER
; interpretation the state of the sequence
#; (define (sq-templete sq)
     (cond [(string=? sq AA) ...]
           [(string=? sq BB) ...]
           [(string=? sq DD) ...]
           [(string=? sq ER) ...]))

; Sequence KeyEvent -> Sequence
; handle the keyboard input
(check-expect (handle-sequence AA "a") BB)
(check-expect (handle-sequence AA "b") ER)
(check-expect (handle-sequence AA " ") ER)
(check-expect (handle-sequence BB "b") BB)
(check-expect (handle-sequence BB "c") BB)
(check-expect (handle-sequence BB "d") DD)
(check-expect (handle-sequence BB "e") ER)
(check-expect (handle-sequence BB " ") ER)
(check-expect (handle-sequence BB "1") ER)

; (define (handle-sequence sq ke) sq) ;stub

(define (handle-sequence sq key)
  (cond [(string=? sq AA)
         (if (string=? key "a")
             BB
             ER)]
        [(string=? sq BB)
         (cond [(or
                 (string=? key "b")
                 (string=? key "c")) BB]
               [(string=? key "d") DD]
               [else ER])]))

; Sequence -> Image
; render white BG by default
; - yellow on the first proper letter
; - green of the final letter
; - red on error
(check-expect (render AA) (overlay
                           (square 100 "solid" "white")
                           MTS))
(check-expect (render BB) (overlay
                           (square 100 "solid" "yellow")
                           MTS))
(check-expect (render DD) (overlay
                           (square 100 "solid" "green")
                           MTS))

(check-expect (render ER) (overlay
                           (square 100 "solid" "red")
                           MTS))

; (define (render sq) MTS) ;stub
(define (render sq)
  (overlay
   (square 100 "solid"
           (cond [(string=? sq AA) "white"]
                 [(string=? sq BB) "yellow"]
                 [(string=? sq DD) "green"]
                 [(string=? sq ER) "red"]))
   MTS))

; =================== End of exercise ==================
