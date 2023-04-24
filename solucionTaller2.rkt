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

(define fnc
  (lambda(n clauses)
    (list 'fnc n clauses)))
  



;; Pruebas punto #1

(define example (fnc 2 (cons-and (cons-or 1 2) (cons-or -1 -2))))

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

;; Pruebas punto #2
(PARSEBNF '(a :: b ( * c :: d) * e))
(PARSEBNF '(a * (b c) :: ( (d * e) :: f) * g))

;; Fin punto #2

;; Inicio punto #3



;; Pruebas punto #3



;; Fin punto #3