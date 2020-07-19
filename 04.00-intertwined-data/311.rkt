;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |311|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT (short for family tree) is one of:
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> ???
(define (fun-FT an-ftree)
  (cond
    [(no-parent? an-ftree) ...]
    [else (... (fun-FT (child-father an-ftree)) ...
               ... (fun-FT (child-mother an-ftree)) ...
               ... (child-name an-ftree) ...
               ... (child-date an-ftree) ...
               ... (child-eyes an-ftree) ...)]))


; consume a family tree and the current year.
; producee the average age of all child structures in the family tree

(check-expect (average-age 2020 Carl) (- 2020 (child-date Carl)))
(check-expect (average-age 2020 Adam) (/ (+ (- 2020 (child-date Carl))
                                            (- 2020 (child-date Bettina))
                                            (- 2020 (child-date Adam)))
                                         3))

; FT -> Number
(define (average-age current-year an-ftree)
  (local ((define (age-sum current-year an-ftree)
            (cond
              [(no-parent? an-ftree) 0]
              [else (+ (age-sum current-year (child-father an-ftree))
                       (age-sum current-year (child-mother an-ftree))
                       (- current-year (child-date an-ftree)))]))
          (define (number-of-people an-ftree)
            (cond
              [(no-parent? an-ftree) 0]
              [else (+ (number-of-people (child-father an-ftree))
                       (number-of-people (child-mother an-ftree))
                       1)])))
     (/ (age-sum current-year an-ftree)
        (number-of-people an-ftree))))
