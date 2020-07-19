;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 293-prework-ch17.4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [Number Number -> Boolean] 
; -> [List-of Number]
; produces a version of alon, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon) (isort (rest alon)))]))
 
          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon 
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))

(check-satisfied (sort-cmp '("c" "b") string<?)
                 (sorted string<?))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <)
                 (sorted <))

; [X X -> Boolean] -> [ [List-of X] -> Boolean ]
; produces a function that determines whether 
; some list is sorted according to cmp
#;(define (sorted cmp)
  (lambda (l)
    #true)) ; stub

(define (sorted cmp)
  (lambda (l0)
    (local ((define (sorted/l l) ...))
      ...)))


(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)
