;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |258|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define WIDTH 100)
(define HEIGHT 100)
(define MT (empty-scene WIDTH HEIGHT))

;(render-polygon (square 10 10 "blue") (list (make-posn 10 50) (make-posn 10 5) (make-posn 20 5) (make-posn 40 20)))

; Image Polygon -> Image 
; adds an image of p to MT
(define (render-polygon img p)
  (render-line (connect-dots img p) (first p) (last p)))
 
; Image NELoP -> Image
; connects the Posns in p in an image
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) MT]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))
 
; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
 
; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))


;;;;;;;;;;;;;;; refactor to local

; Image Polygon -> Image 
; adds an image of p to MT
(define (render-polygon-v2 img p)
  (local (; Image NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots-loc img p)
            (cond
              [(empty? (rest p)) MT]
              [else (render-line-loc (connect-dots-loc img (rest p))
                                     (first p)
                                     (second p))]))
          ; Image Posn Posn -> Image
          ; draws a red line from Posn p to Posn q into im
          (define (render-line-loc im p q)
            (scene+line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red")) 
          ; Polygon -> Posn
          ; extracts the last item from p
          (define (last-loc p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last-loc (rest p))])))
    (render-line-loc (connect-dots-loc img p) (first p) (last-loc p))))

(render-polygon-v2 (square 10 10 "blue") (list (make-posn 10 50) (make-posn 10 5) (make-posn 20 5) (make-posn 40 20)))