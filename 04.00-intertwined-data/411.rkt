;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |411|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)

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


; ==================== Exercise 411 ====================

; ===================== Problem 1 ======================

(define school-schema.v2 `(("Name" ,string?)
                           ("Age" ,integer?)
                           ("Present" ,boolean?)))
(define school-content.v2 '(("Alice" 35 #t)
                            ("Bob" 25 #f)
                            ("Carol" 30 #t)
                            ("Dave" 32 #f)))

(define school-db.v2
  (make-db school-schema.v2
           school-content.v2))

(define presence-schema.v2 `(("Present" ,boolean?)
                             ("Descripton" ,string?)
                             ("Signature", string?)))
(define presence-content.v2 '((#t "presence" "self signed")
                              (#f "absence" "auto signed")))

(define presence-db.v2
  (make-db presence-schema.v2
           presence-content.v2))

; DB DB -> DB
; create a database from db-1 by replacing the last cell in each row
; with the translation of the cell in db-2
; (define (join db1 db2) db1) ;stub

(check-expect (db-content
               (join school-db.v2 presence-db.v2))
              `(("Alice" 35 "presence" "self signed")
                ("Bob" 25 "absence" "auto signed")
                ("Carol" 30 "presence" "self signed")
                ("Dave" 32 "absence" "auto signed")))

(define (join db1 db2)
  (local (; helper constants
          (define schema1  (db-schema db1))
          (define schema2  (db-schema db2))
          (define content1 (db-content db1))
          (define content2 (db-content db2))
          ; helper functions
          (define (drop-last x) (reverse (rest (reverse x))))
          (define (merge-rows r1 r2)
            (append (drop-last r1)
                    (rest r2)))
          ; db join functions
          (define joined-schema (merge-rows schema1 schema2))
          (define joined-content
            (map (lambda (row)
                   (local ((define search-term (last row))
                           (define found-db2-row (assoc search-term content2)))
                     (merge-rows row found-db2-row)))
                 content1)))
    (make-db joined-schema
             joined-content)))

; ===================== Problem 2 ======================

(require 2htdp/abstraction)

(define school-schema.v3 `(("Name" ,string?)
                           ("Age" ,integer?)
                           ("Present" ,boolean?)))
(define school-content.v3 '(("Alice" 35 #t)
                            ("Bob" 25 #f)
                            ("Carol" 30 #t)
                            ("Dave" 32 #f)))

(define school-db.v3
  (make-db school-schema.v3
           school-content.v3))

(define presence-schema.v3 `(("Present" ,boolean?)
                             ("Descripton" ,string?)))
(define presence-content.v3 '((#t "presence")
                              (#t "self signed")
                              (#f "absence")
                              (#f "auto signed")))

(define presence-db.v3
  (make-db presence-schema.v3
           presence-content.v3))

; DB DB -> DB
; create a database from db-1 by replacing the last cell in each row
; with the translation of the cell in db-2
; (define (join db1 db2) db1) ;stub

(check-expect (db-content
               (join.v2 school-db.v3 presence-db.v3))
              `(("Alice" 35 "presence")
                ("Alice" 35 "self signed")
                ("Bob" 25 "absence")
                ("Bob" 25 "auto signed")
                ("Carol" 30 "presence")
                ("Carol" 30 "self signed")
                ("Dave" 32 "absence")
                ("Dave" 32 "auto signed")))

(define (join.v2 db1 db2)
  (local (; helper constants
          (define schema1  (db-schema db1))
          (define schema2  (db-schema db2))
          (define content1 (db-content db1))
          (define content2 (db-content db2))
          ; helper functions
          (define (drop-last x) (reverse (rest (reverse x))))
          (define (merge-rows r1 r2)
            (append (drop-last r1)
                    (rest r2)))
          ; db join functions
          (define joined-schema (merge-rows schema1 schema2))
          (define (merge-append-rows row acc)
            (local ((define search-term (last row))
                    (define found-db2-rows
                      (filter (lambda (db2-row)
                                (equal? search-term (first db2-row)))
                              content2))
                    (define merged-db2-rows
                      (map (lambda (db2-row)
                             (merge-rows row db2-row))
                           found-db2-rows)))
              (append merged-db2-rows acc)))
          (define joined-content
            (foldr merge-append-rows '() content1)))
    (make-db joined-schema
             joined-content)))
              
; =================== End of exercise ==================
