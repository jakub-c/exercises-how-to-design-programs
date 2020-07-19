;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |295|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(check-satisfied (random-posns 30000)
                 (n-inside-playground? 30000))

(define (random-posns n)
  (build-list
   n
   (lambda (i)
     (make-posn (random WIDTH) (random HEIGHT)))))

(define (n-inside-playground? n)
  (lambda (lop)
    (local ((define (check-list l)
              (cond [(empty? l) #t]
                    [else
                     (and
                      (and (and
                            (< (posn-x (first l)) WIDTH)
                            (< (posn-y (first l)) HEIGHT))
                           (and
                            (>= (posn-x (first l)) 0)
                            (>= (posn-y (first l)) 0)))
                      (check-list (rest l)))])))
      (and
       (check-list lop)
       (= (length lop) n)))))


(define (random-posns/bad n)
  (build-list
   n
   (lambda (i)
     (make-posn (* (random WIDTH) -1) (random HEIGHT)))))

(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))
