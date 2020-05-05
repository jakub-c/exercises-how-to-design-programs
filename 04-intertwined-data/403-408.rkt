;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |403|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a structure:
;   (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

; figure 137 can be translated to:
(define school-schema `(("Name" ,string?)
                        ("Age" ,integer?)
                        ("Present" ,boolean?)))
(define school-content '(("Alice" 35 #t)
                         ("Bob" 25 #f)
                         ("Carol" 30 #t)
                         ("Dave" 32 #f)))

(define school-db
  (make-db school-schema
           school-content))

(define presence-schema `(("Present" ,boolean?)
                          ("Descripton" ,string?)))
(define presence-content '((#t "presence")
                           (#f "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))


; ==================== Exercise 403 ====================

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)

(define school-schema-struct `(,(make-spec "Name" string?)
                               ,(make-spec "Age" integer?)))

(define presence-schema-struct `(,(make-spec "Present" boolean?)
                                 ,(make-spec "Descripton" string?)))

; =================== End of exercise ==================

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
 
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
 
; (define (integrity-check db) #false) ;stub

; ==================== Exercise 404 ====================

; [X Y] [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; (define (andmap2 f l1 l2) #f) ;stub

(check-expect (andmap2 (lambda (x y) (and (number? x) (number? y)))
                       '(1 2) '(1 2))
              #true)

(define (andmap2 f x y)
  (cond [(not (equal? (length x) (length y))) (error "list length error")]
        [(or (empty? x)
             (empty? y)) #true]
        [else
         (and (f (first x) (first y))
              (andmap2 f (rest x) (rest y)))]))

; =================== End of exercise ==================

(define (integrity-check db)
  (local (; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row)
                    (length (db-schema db)))
                 (andmap (lambda (s c) [(second s) c])
                         (db-schema db)
                         row))))
    (andmap row-integrity-check (db-content db))))

(check-expect (integrity-check
               (make-db `(("Name" ,string?)
                          ("Age" ,number?))
                        `((30 "Alice"))))
              #false)

; ==================== Exercise 405 ====================

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 
(define projected-db
  (make-db projected-schema projected-content))

(check-expect
 (db-content (project school-db '("Name" "Present")))
 projected-content)

#;(define (project db labels)
    (local ((define schema  (db-schema db))
            (define content (db-content db))
            ; Spec -> Boolean
            ; does this spec belong to the new schema
            (define (keep? c)
              (member? (first c) labels))
            ; Row -> Row
            ; retains those columns whose name is in labels
            (define (row-project row)
              (row-filter row (map first schema)))
            ; Row [List-of Label] -> Row
            ; check if each Cell's label is a member of 'labels' filter argument
            (define (row-filter row cell-labels)
              (cond [(empty? row) '()]
                    [else
                     (if (member? (first cell-labels) labels)
                         (cons (first row)
                               (row-filter (rest row) (rest cell-labels)))
                         (row-filter (rest row) (rest cell-labels)))])))
      (make-db (filter keep? schema)
               (map row-project content))))

; =================== End of exercise ==================

; ==================== Exercise 406 ====================

#;(define (project db labels)
    (local ((define schema  (db-schema db))
            (define content (db-content db))
            (define original-table-labels
              (map first schema))
            ; Spec -> Boolean
            ; does this spec belong to the new schema
            (define (keep? c)
              (member? (first c) labels))
            ; Row -> Row
            ; retains those columns whose name is in labels
            (define (row-project row)
              (row-filter row original-table-labels))
            ; Row [List-of Label] -> Row
            ; check if each Cell's label is a member of 'labels' filter argument
            (define (row-filter row cell-labels)
              (cond [(empty? row) '()]
                    [else
                     (if (member? (first cell-labels) labels)
                         (cons (first row)
                               (row-filter (rest row) (rest cell-labels)))
                         (row-filter (rest row) (rest cell-labels)))])))
      (make-db (filter keep? schema)
               (map row-project content))))

; =================== End of exercise ==================

; ==================== Exercise 407 ====================

#;(define (project db labels)
    (local ((define schema  (db-schema db))
            (define content (db-content db))
            (define original-table-labels
              (map first schema))
            ; Spec -> Boolean
            ; does this spec belong to the new schema
            (define (keep? c)
              (member? (first c) labels))
            ; Row -> Row
            ; retains those columns whose name is in labels
            (define (row-project row)
              (row-filter row original-table-labels))
            ; Row [List-of Label] -> Row
            ; check if each Cell's label is a member of 'labels' filter argument
            (define (row-filter row cell-labels)
              (foldr
               (lambda (row-val cell-val acc)
                 (if (member? cell-val labels)
                     (cons row-val acc)
                     acc))
               '() row cell-labels)
              ))
      (make-db (filter keep? schema)
               (map row-project content))))

; =================== End of exercise ==================

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))

; ==================== Exercise 408 ====================

(define content-for-select
  '(("Alice" 35 #t)
    ("Bob" 25 #f)
    ("Carol" 30 #t)
    ("Dave" 32 #f)))
 
(define schema-for-select
  `(("Name" ,string?) ("Age" ,number?) ("Present" ,boolean?)))
 
(define db-for-select
  (make-db schema-for-select content-for-select))


; DB [List-of Label] Predicate -> Content
; (define (select db labels pred) '()) ;stub

(check-expect
 (select db-for-select '("Name" "Age") (lambda (r) (string=? (first r) "Alice")))
 '(("Alice"35)))

(check-expect
 (select db-for-select '("Name" "Age") (lambda (r) (third r)))
 '(("Alice" 35)
   ("Carol" 30)))

(check-expect
 (select db-for-select '("Name" "Age") (lambda (r) (> (second r) 60)))
 '())

(define (select db labels pred)
  (local ((define content (db-content db))
          (define schema (db-schema db))
          (define filtered-db
            (make-db schema (filter pred content)))
          (define projected-filtered-db
            (project filtered-db labels)))
    (db-content projected-filtered-db)))

; =================== End of exercise ==================
