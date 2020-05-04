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

