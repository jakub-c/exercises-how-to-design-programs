;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 403-408) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; ==================== Exercise 409 ====================

(require racket/list)

(define content-for-reorder
  '(("Alice" 35 #t)
    ("Bob" 25 #f)
    ("Carol" 30 #t)
    ("Dave" 32 #f)))
 
(define schema-for-reorder
  `(("Name" ,string?) ("Age" ,number?) ("Present" ,boolean?)))
 
(define db-for-reorder
  (make-db schema-for-reorder content-for-reorder))

; DB [List-of Label] -> DB
; produce a database like db
; but with its columns reordered according to lol
;(define (reorder db lol) db) ;stub

(check-expect (db-content (reorder db-for-reorder '("Present" "Age" "Name")))
              '((#t 35 "Alice")
                (#f 25 "Bob")
                (#t 30 "Carol")
                (#f 32 "Dave")))

(check-expect (db-content (reorder db-for-reorder '("Age" "Name")))
              '((35 "Alice")
                (25 "Bob")
                (30 "Carol")
                (32 "Dave")))

(define (reorder db lol)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          ; produce a list of db's column number indexes according to lol
          (define sorted-col-positions
            (map (lambda (label)
                   (index-of schema (assoc label schema)))
                 lol))
          (define sorted-schema
            (map (lambda (index)
                   (list-ref schema index))
                 sorted-col-positions))
          (define (sort-row row)
            (map (lambda (index)
                   (list-ref row index))
                 sorted-col-positions))
          (define sorted-content
            (map sort-row content)))
    (make-db sorted-schema sorted-content)))

; =================== End of exercise ==================

; ==================== Exercise 410 ====================

(define schema-for-union
  `(("Name" ,string?) ("Age" ,number?) ("Present" ,boolean?)))

(define content-for-union1
  '(("Alice" 35 #t)
    ("Bob" 25 #f)
    ("Carol" 30 #t)
    ("Dave" 32 #f)))

(define content-for-union2
  '(("Albert" 30 #f)
    ("Bob" 25 #f)
    ("Anders" 34 #t)
    ("Dave" 32 #f)))

(define db-for-union1
  (make-db schema-for-union content-for-union1))

(define db-for-union2
  (make-db schema-for-union content-for-union2))

; DB DB -> DB
; consume two databases with the exact same schema
; and produce a new database with this schema and the joint content of both
; eliminate rows with the exact same content
; (define (db-union db1 db2) db1) ;stub

(check-expect (db-content
               (db-union db-for-union1 db-for-union2))
              `(("Albert" 30 #f)
                ("Anders" 34 #t)
                ("Alice" 35 #t)
                ("Bob" 25 #f)
                ("Carol" 30 #t)
                ("Dave" 32 #f)))

(define (db-union db1 db2)
  (local((define schema  (db-schema db1))
         (define content1 (db-content db1))
         (define content2 (db-content db2))
         (define union-content
           (foldr (lambda (val acc)
                    (if (member? val acc)
                        acc
                        (cons val acc)))
                  content1 content2)))
    (make-db schema union-content)))

; =================== End of exercise ==================
