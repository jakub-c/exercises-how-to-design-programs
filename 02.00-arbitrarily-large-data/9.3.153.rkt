;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 9.3.153) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)

(define SQR (square 10 "outline" "black"))

; Natural Img -> Img
; place the given image in a column n times
(check-expect (col 0 SQR) empty-image)
(check-expect (col 2 SQR) (beside SQR (beside SQR empty-image)))

; (define (col n img) SQR) ;stub
(define (col n img)
   (cond
     [(zero? n) empty-image]
     [else
        (beside img (col (sub1 n) img))]))

; Natural Img -> Img
; place the given image in a row n times
(check-expect (row 0 SQR) empty-image)
(check-expect (row 2 SQR) (above SQR (above SQR empty-image)))

; (define (col n img) SQR) ;stub
(define (row n img)
   (cond
     [(zero? n) empty-image]
     [else
        (above img (row (sub1 n) img))]))

; ListOfPosns is one of:
;  - '()
;  - (cons Posn ListOfPosns)

(define EMPTY-POSNS '())
(define LIST-OF-POSN (cons (make-posn 10 40) '()))

#;(define (fn-for-lop lop)
     (cond
       [(empty? lop) ...]
       [else
        (...
         (posn-x (first lop)) (posn-y (first lop))
         ...
         (fn-for-lop (rest lop) img))]))


(define GRID (col 10 (row 18 SQR)))

; ListOfPosns Img -> Img
; draw red dots from the list on the picture provided
(check-expect (add-balloons '() GRID) GRID)
(check-expect (add-balloons LIST-OF-POSN GRID)
              (place-image (circle 5 "solid" "red")
                           10 40
                           GRID))
(check-expect (add-balloons
               (cons (make-posn 10 40)
                     (cons (make-posn 40 80) '()))
               GRID)
              (place-image (circle 5 "solid" "red")
                           10 40
                           (place-image
                            (circle 5 "solid" "red")
                            40 80
                            GRID)))

; (define (add-balloons lop img) empty-image) ;stub

(define (add-balloons lop img)
     (cond
       [(empty? lop) img]
       [else
        (place-image
         (circle 5 "solid" "red")
         (posn-x (first lop)) (posn-y (first lop))
         (add-balloons (rest lop) img))]))