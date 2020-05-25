;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 471-475) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)
(require 2htdp/abstraction)

; ==================== Exercise 471 ====================

(define sample-graph
  '((A B E)
    (B E F)
    (C D)
    (D)
    (E C F)
    (F D G)
    (G)))

; A Node is a Symbol
; A Graph is a [List-of [List-of Node]]

; Node Graph -> [List-of Node]
; produce the list of immediate neighbors of n in g
; (define (neighbors n g) '()) ;stub

(check-expect (neighbors 'E sample-graph)
              '(C F))
(check-expect (neighbors 'B sample-graph)
              '(E F))

(define (neighbors n g)
  (foldr (lambda (current-list return-val)
           (if (equal? n (first current-list))
               (append (rest current-list) return-val)
               return-val))
         '()
         g))

; =================== End of exercise ==================

; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 
 
; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
#; (define (find-path origination destination G)
     #false)

(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
#;(define (find-path/list lo-originations destination G)
    #false)

(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

; ==================== Exercise 472 ====================

; Q: Use the function to find a path from 'A to 'G in sample-graph.
;    Which one does it find? Why?

; A: (find-path 'A 'G sample-graph)
;      returns (list 'A 'B 'E 'F 'G)
;    I think the answer includes 'E because once the function
;    reaches 'B it runs the neighbors function which lists
;    'E before 'F, it's added to the path and returned

; Graph -> Boolean
; determine whether there is a path between any pair of nodes
; (define (test-on-all-nodes G) #false) ;stub

(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes '((A B) (B A)) ) #true)

(define (test-on-all-nodes G)
  (local ((define all-nodes (for/list ([node-def G]) (first node-def)))
          (define nodes (remove-duplicates (flatten G))))
    (for/and ([org nodes])
      (for/and ([dest nodes])
        (if (boolean? (find-path org dest G))
            #false
            #true)))))

; =================== End of exercise ==================

(define cyclic-graph
  '((A B E)
    (B E F)
    (C D B)
    (D)
    (E C F)
    (F D G)
    (G)))

; ==================== Exercise 473 ====================

; (find-path 'B 'C cyclic-graph)
; returns (list 'B 'E 'C)

; (test-on-all-nodes cyclic-graph)
; goes into infinite loop

; =================== End of exercise ==================

; ==================== Exercise 474 ====================

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
#; (define (find-path.v2 origination destination G)
     #false)

(check-expect (find-path.v2 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path.v2 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path.v2 'C 'G sample-graph)
              #false)

(define (find-path.v2 origination destination G)
  (local ((define (find-path.v2/list lo-Os D G)
            (cond
              [(empty? lo-Os) #false]
              [else (local ((define candidate
                              (find-path.v2 (first lo-Os) D G)))
                      (cond
                        [(boolean? candidate)
                         (find-path.v2/list (rest lo-Os) D G)]
                        [else candidate]))])))
    (cond
      [(symbol=? origination destination) (list destination)]
      [else (local ((define next (neighbors origination G))
                    (define candidate
                      (find-path.v2/list next destination G)))
              (cond
                [(boolean? candidate) #false]
                [else (cons origination candidate)]))])))

; =================== End of exercise ==================
