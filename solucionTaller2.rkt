#lang eopl

;; Solución taller #2 - Fundamentos de lenguajes de programación
;; Domingo, 23 de abril de 2023
;; Hecho por:
;; Janiert Sebastián Salas - 1941265
;; Jhon Alexander Valencia - 2042426
;; Diego Fernando Victoria - 2125877

;; Inicio punto #1

;;    ---- EXTRACTORES ----


(define (fnc->vars exp)
    (cond ((null? exp) '())
          ((number? exp) (list exp))
          ((and (eqv? (car exp) 'fnc) (number? (cadr exp))) append (fnc->vars (caddr exp)))
          ((is-or? exp) (append (fnc->vars (or->left exp))
                             (fnc->vars (or->right exp))))
          ((is-and? exp) (append (fnc->vars (and->left exp))
                              (fnc->vars (and->right exp))))
          )
    )


(define (and->left and)
  (car and))

(define (and->right and)
  (caddr and))

(define (or->left or)
  (car or))

(define (or->right or)
  (caddr or))

(define is-and?
  (lambda (exp)
    (eqv? (cadr exp) 'and)
    )
  )

(define is-or?
  (lambda (exp)
    (eqv? (cadr exp) 'or)
    )
  )

(define vars-valid?
  (lambda(exp vars)
    (cond
      ((null? vars) #t)
      ((> (car vars) (cadr exp)) #f)
      (else(vars-valid? exp (cdr vars)))
      )
  )
  )

;;    ---- CONSTRUCTORES ----

;;    ---- AND ----


(define cons-and
  (lambda (left right)
    (list left 'and right))
  )

;;    ---- OR ----

(define cons-or
  (lambda(left right)
    (list left 'or right))
  )

;;    ---- FNC ----

(define cons-fnc
  (lambda(n clauses)
    (list 'fnc n clauses)))
  
(define (fnc exp)
  (cond
    ((vars-valid? exp (fnc->vars exp)) exp)
    (else((eopl:error "Syntax error" )))
    ))


;; Pruebas punto #1

(define example (fnc(cons-fnc 2 (cons-and (cons-or 1 2) (cons-or 1 1)))))

;; Fin punto #1

;; Inicio punto #2

(define (PARSEBNF lst)
  (cond
    [(null? lst) '()]
    [(not (list? lst)) lst]
    [(eq? (car lst) '()) '()]
    [(eq? (car lst) '::) (list 'or (PARSEBNF (cdr lst)))]
    [(eq? (car lst) '*) (list 'and (PARSEBNF (cdr lst)))]
    [else (cons (PARSEBNF (car lst))
                (PARSEBNF (cdr lst)))]
  )
)

(define (UNPARSEBNF lst)
  (cond
    [(null? lst) '()]
    [(not (list? lst)) lst]
    [(eq? (car lst) '()) '()]
    [(eq? (car lst) 'or) (cons ':: (UNPARSEBNF (cdr lst)))]
    [(eq? (car lst) 'and) (cons '* (UNPARSEBNF (cdr lst)))]
    [(and (list? (car lst)) (not (eq? (car lst) '())))
     (cons (UNPARSEBNF (car lst)) (UNPARSEBNF (cdr lst)))]
    [else (cons (UNPARSEBNF (car lst))
                (UNPARSEBNF (cdr lst)))]
  )
)

;; Pruebas punto #2

(display "Pruebas PARSEBNF:\n")
(display (PARSEBNF '(a :: b ( * c :: d) * e)))
(newline)
(display (PARSEBNF '(a * (b c) :: ( (d * e) :: f) * g)))
(display "\n\nPruebas UNPARSEBNF:\n")
(display (UNPARSEBNF '(a or (b (and (c or (d))) and (e)))))
(newline)
(display (UNPARSEBNF '(a and ((b c) or (((d and (e)) or (f)) and (g))))))

;; Fin punto #2

;; Inicio punto #3



;; Pruebas punto #3



;; Fin punto #3