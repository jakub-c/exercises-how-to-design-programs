;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |512|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/match)

; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) x) (λ (x) x)))
(define ex5 '((λ (x) (x x)) (λ (x) (x x))))
(define ex6 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))

; ==================== Exercise 512 ====================

; Lam -> Boolean
; check is the argument is a variable
; (define (is-var? x) #f) ;stub

(check-expect (is-var? 'x) #t)
(check-expect (is-var? '(λ (x) x)) #f)
(check-expect (is-var? '(x 5)) #f)

(define (is-var? x) (symbol? x))

; Lam -> Boolean
; check is the argument is a lambda expression
; (define (is-λ? x) #f) ;stub

(check-expect (is-λ? 'x) #f)
(check-expect (is-λ? '(λ (x) x)) #t)
(check-expect (is-λ? '((λ (x) x) (λ (x) x))) #f)
(check-expect (is-λ? '('x 5)) #f)
(check-expect (is-λ? '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))
              #f)

(define (is-λ? x)
  (match x
    [(list 'λ (list param) body) #t]
    [_ #f]))

; Lam -> Boolean
; check is the argument is a function application
; (define (is-app? x) #f) ;stub

(check-expect (is-app? ex1) #f)
(check-expect (is-app? ex3) #f)
(check-expect (is-app? ex4) #t)
(check-expect (is-app? ex6) #t)


(define (is-app? lad)
  (match lad
    [(list a b) #t]
    [_ #f]))

; Lam -> Symbol
; extract the parameter from a λ expression
; (define (λ-para lam) 'a) ;stub

(check-expect (λ-para ex1) 'x)
(check-expect (λ-para ex2) 'x)
(check-expect (λ-para ex6) '((λ (y) (λ (x) y)) (λ (z) z)))


(define (λ-para lam)
  (match lam
    [(list body param) (first lam)]
    [(list 'λ (list param) body) param]))

; Lam -> Symbol
; extract the body from a λ expression
; (define (λ-body lam) 'a) ;stub

(check-expect (λ-body ex1) 'x)
(check-expect (λ-body ex2) 'y)
(check-expect (λ-body ex3) '(λ (x) y))
(check-expect (λ-body ex6) '(λ (w) w))


(define (λ-body lam)
  (match lam
    [(list body param) (second lam)]
    [(list 'λ (list param) body) body]))

; Lam -> [Maybe Lam]
; extract the function from an application
; (define (app-fun lam) 'x) ;stub

(check-expect (app-fun ex4) '(λ (x) x))
(check-expect (app-fun ex6)
              '((λ (y) (λ (x) y)) (λ (z) z)))

(define (app-fun lam)
  (match lam
    [(list a b) a]
    [_ #false]))

; Lam -> [Maybe Lam]
; extract the argument from an application
; (define (app-arg lam) 'x) ;stub

(check-expect (app-arg ex4) '(λ (x) x))
(check-expect (app-arg ex6)
              '(λ (w) w))

(define (app-arg lam)
  (match lam
    [(list a b) b]
    [_ #false]))

; Lam -> [List-of Symbol]
; produce the list of all symbols used as λ parameters in a λ term
; (define (declareds lam) '(a)) ;stub

(check-expect (declareds ex1) '(x x))
(check-expect (declareds ex6) '(y x y z z w w))

(define (declareds lam0)
  (local ((define (search/a lam result)
            (cond [(is-var? lam) (list lam)]
                  [(is-λ? lam) (append (search/a (λ-para lam) result)
                                       (search/a (λ-body lam) result)
                                       result)]
                  [else
                   (append (search/a (app-fun lam) result)
                           (search/a (app-arg lam) result))])))
    (search/a lam0 '()))) 

; =================== End of exercise ==================

; ==================== Exercise 513 ====================

(define-struct lam [param body])
; Lambda-struct is one of:
; - Symbol
; - (make-lam [List-of Symbol] Expression-struct)

(define-struct appl [fun arg])
; Application-struct is:
;  - (make-appl Lambda-struct Lambda-strcut)

; Expression-struct is one of:
; Lambda-struct
; Application-struct


(define ex1-str (make-lam (list 'x) 'x))
(define ex2-str (make-lam (list 'x) 'y))
(define ex3-str (make-lam (list 'y)
                          (make-lam (list 'x) 'y)))

(define ex4-str (make-appl
                 (make-lam (list 'x) 'x)
                 (make-lam (list 'x) 'x)))

(define ex5-str (make-appl
                 (make-lam (list 'x) '(x x))
                 (make-lam (list 'x) '(x x))))

(define ex6-str (make-appl
                 (make-appl (make-lam 'y
                                      (make-lam 'x 'y))
                            (make-lam 'z 'z))
                 (make-lam 'w 'w)))

; =================== End of exercise ==================

; Lam -> Lam 
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ 
; expression whose parameter is s
 
(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)

; (define (undeclareds le0) le0) ;stub

; Lam -> Lam 
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; ==================== Exercise 514 ====================

; Q: Make up an ISL+ expression in which x occurs both free and bound.
; A: I'm not sure if this is the right answer but given the hints from the
;     next question I think the possible answer could be:

(define input-514 '(λ (x) (x x)))
; (undeclareds input-514) returns:
#;(list 'λ (list 'x) (list 'x 'x))
; works as expected
; I might be wrong about this exercise though

; =================== End of exercise ==================

; ==================== Exercise 515 ====================

(define input-515 '(λ (*undeclared) ((λ (x) (x *undeclared)) y)))

; (undeclareds input-515) produces:
#;(list
   'λ
   (list '*undeclared)
   (list (list 'λ (list 'x) (list 'x '*undeclared)) '*undeclared))

; =================== End of exercise ==================

; ==================== Exercise 516 ====================

; Expression-struct -> Expression-stuct
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ 
; expression whose parameter is s
 
(check-expect (undeclareds-str ex1-str) ex1-str)
(check-expect (undeclareds-str ex2-str) (make-lam (list 'x) '*undeclared))
(check-expect (undeclareds-str ex3-str) ex3-str)
(check-expect (undeclareds-str ex4-str) ex4-str)

; (define (undeclareds-str le0) le0) ;stub

(define (undeclareds-str le0)
  (local (; Lam-strcut [List-of Symbol] -> Lam-struct
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(lam? le)
               (local ((define para (first (lam-param le)))
                       (define body (lam-body le))
                       (define newd (cons para declareds)))
                 (make-lam (list para)
                           (undeclareds/a body newd)))]
              [(appl? le)
               (local ((define fun (appl-fun le))
                       (define arg (appl-arg le)))
                 (make-appl (undeclareds/a fun declareds)
                            (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; =================== End of exercise ==================

; ==================== Exercise 517 ====================

; A Lam-distance is one of: 
; – a Number
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

; Lam -> Lam-distance
; replaces all occurrences of variables with a natural number
; that represents how far away the declaring λ is
; (define (static-distance lam) '(λ (a) 0)) ;stub

(check-expect (static-distance ex1) '(λ (x) 0))
(check-expect (static-distance '(λ (x) (λ (y) (x y))))
              '(λ (x) (λ (y) (1 0))))
(check-expect (static-distance
                 '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
                '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))

(define (static-distance le0)
  (local
    (; Lam ??? -> Lam
     ; accumulator a represents ... 
     (define (static-distance/a le params-depth)
       (cond
         [(is-var? le) (second (assoc le params-depth))]
         [(is-λ? le)
          (local ((define para (λ-para le))
                  (define body (λ-body le))
                  (define depth-map
                    (cons (list para 0)
                          (map (lambda (el)
                                 (list (first el)
                                       (add1 (second el))))
                               params-depth)))
                    #;(if (list? (assoc para params-depth))
                        (map (lambda (el)
                               (if (symbol=? (first el) para)
                                   (list
                                    (first el)
                                    (+ 1 (second el)))
                                   el))
                             params-depth)
                        (cons (list para 0) params-depth)))
            (list 'λ
                  (list (λ-para le))
                  (static-distance/a (λ-body le) depth-map)))]
         [(is-app? le)
          (local ((define fun (app-fun le))
                  (define arg (app-arg le)))
            (list (static-distance/a fun params-depth)
                  (static-distance/a arg params-depth)))])))
    (static-distance/a le0 '())))

; =================== End of exercise ==================
