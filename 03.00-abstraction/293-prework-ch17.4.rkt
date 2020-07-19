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
    (local ((define (sorted/l l)
              (cond [(empty? (rest l)) #t]
                    [else
                     (and (cmp (first l) (second l))
                          (sorted/l (rest l)))])))
      (if (empty? l0)
          #t
          (sorted/l l0)))))


(check-expect [(sorted string<?) '("b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
 
#;(define (sorted? cmp l)
    #false) ; stub

(define (sorted? cmp l)
  (cond [(empty? (rest l)) #true]
        [else
         (and (cmp (first l) (second l))
              (sorted? cmp (rest l)))]))

; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
 
(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)

; (define (contains? l1 l2) #f) ;stub

(define (contains? original list)
  (andmap (lambda (el)
            (member? el original)) list))

; [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0
#;(define (sorted-variant-of k cmp)
    (lambda (l0) #false)) ;stub

(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? k l0)
         (contains? l0 k))))


(check-expect [(sorted-variant-of '(3 2) <) '(2 3)]
              #true)
(check-expect [(sorted-variant-of '(3 2) <) '(3)]
              #false)

; [List-of Number] -> [List-of Number] 
; produces a sorted version of l
(define (sort-cmp/worse l)
  (local ((define sorted (sort-cmp l <)))
    (cons (- (first sorted) 1) sorted)))

; (check-expect (sort-cmp/worse '(1 2 3)) '(1 2 3))

(check-satisfied (sort-cmp/worse '(1 2 3))
                 (sorted-variant-of '(1 2 3) <))

