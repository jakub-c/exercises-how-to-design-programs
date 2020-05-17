;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |457|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Number [List-of-Number] -> Number
; computes how many months it takes to double a given amount of money
; when a savings account pays interest at a fixed rate on a monthly basis
; (define (double-amount amount interest months) 1) ;stub

(check-expect (double-amount  100 100 '(100)) 1)
(check-expect (double-amount  100 50 '(100)) 2)
(check-expect (double-amount  100 10 '(100)) 8)

(define (double-amount amount interest list-of-months)
  (local ((define (calc-interest val) (* val (/ interest 100)))
          (define sum-interest-so-far (foldr + 0 list-of-months))
          (define new-months-interest (cons (calc-interest sum-interest-so-far)
                                            list-of-months))
          (define new-sum (foldr + 0 new-months-interest)))
    (if (>= new-sum (* 2 amount))
        (- (length new-months-interest) 1) ; subtract the first amount from list of interests
        (double-amount amount interest new-months-interest))))