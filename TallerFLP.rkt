#lang eopl

;; Solución taller #1 - Fundamentos de lenguajes de programación
;; Jueves, 30 de marzo de 2023
;; Hecho por:
;; Janiert Sebastián Salas - 1941265
;; Jhon Alexander Valencia - 2042426
;; Diego Fernando Victoria - 2125877

;; Inicio punto #1

;; multiplo5?:
;; Propósito:
;;
(define multiplo5? (lambda (x) (if(eqv? (modulo x 5) 0)
                                  #t
                                  #f)))

;; invert-aux: 
;; Propósito:
;;
(define invert-aux
  (lambda(l)
    (cond
      [(null? l) '()]
      [else (cons (list (car (cdr (car l))) (car (car l))) (invert-aux (cdr l)))]
    ))
)

;; invert:
;; Propósito:
;; L x P -> L' : Procedimiento que invierte las tuplas de una lista L y devuelve otra lista L' aplicandole el predicado P
(define invert 
  (lambda(l p)
    (cond
      [(null? l) '()]
      [else(cond
             [(and (p (caar(invert-aux l))) (p (car(cdr(car(invert-aux l)))))) (cons (car l) (invert (cdr(invert-aux l)) p))]
             [else (invert (cdr(invert-aux l)) p)]
             )]
    )
  )
)

;; Pruebas punto #1
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((5 9) (10 90) (82 7) ) multiplo5? )
(invert '((6 9) (10 90) (82 7) ) odd? )

;; Fin punto #1

;; Inicio punto #2

;; down: 
;; Propósito:
;;
(define down
  (lambda (l)
    (if (null? l)
      '()
      (cons (cons  (car l) '()) (down (cdr l))))))

;; Pruebas punto #2
(display "\nPunto #2\n\nLlamada #1\nEntrada        : (1 2 3)\nSalida esperada: ((1) (2) (3))\nResultado      : ")
(display (down '(1 2 3)))
(display "\n\nLlamada #2\nEntrada        : ((una) (buena) (idea))\nSalida esperada: (((una)) ((buena)) ((idea)))\nResultado      : ")
(display (down '((una) (buena) (idea))))
(display "\n\nLlamada #3\nEntrada        : (un (objeto (mas)) complicado)\nSalida esperada: ((un) ((objeto (mas))) (complicado))\nResultado      : ")
(display (down '(un (objeto (mas)) complicado)))

;; Fin punto #2

;; Inicio punto #3

;; mayor5?: 
;; Propósito:
;;
(define mayor5? (lambda (x) (if(> x  5)
                            #t
                            #f)))                      

;; list-set: 
;; Propósito:
;;
(define list-set
  (lambda (lst n x p)
    (letrec
        (
         (list-aux
          (lambda (lstaux naux xaux acc)
            (cond
              [(eqv? lstaux '()) empty]
              [(and(= acc naux) (p (car lstaux))) (cons xaux (list-aux (cdr lstaux) naux xaux (+ 1 acc)))]
              [else (cons (car lstaux) (list-aux (cdr lstaux) naux xaux (+ 1 acc)))]
              )
            )
          )
         )
      (list-aux lst n x 0))
    )
  )

;; Pruebas punto #3
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)
(list-set '(5 8 7 6) 3 '(1 5 10) mayor5? )
(list-set '(5 8 7 6) 0 '(1 5 10) mayor5? ) 

;; Fin punto #3

;; Inicio punto #4

;; filter-in: 
;; Propósito:
;;
(define filter-in
  (lambda (p l)
    (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (filter-in p (cdr l)))
          (filter-in p (cdr l))))))

;; Pruebas punto #4
(display "\n\nPunto #4\n\nLlamada #1\nEntrada        : (a 2 (1 3) b 7)\nSalida esperada: (2 7)\nResultado      : ")
(display (filter-in number? '(a 2 (1 3) b 7)))
(display "\n\nLlamada #2\nEntrada        : (a (b c) 17 foo)\nSalida esperada: (a foo)\nResultado      : ")
(display (filter-in symbol? '(a (b c) 17 foo)))
(display "\n\nLlamada #3\nEntrada        : (a b u univalle racket flp 28 90 (1 2 3))\nSalida esperada: (univalle racket flp)\nResultado      : ")
(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3))))

;; Fin punto #4

;; Inicio punto #5

;; list-index: 
;; Propósito:
;;
(define (list-index p l)
  (define (aux l n)
    (if (null? l)
        #f
        (if (p (car l))
            n
            (aux (cdr l) (+ n 1)))))
  (aux l 0))

;; Pruebas punto #5
(display "\n\nPunto #5\n\nLlamada #1\nEntrada        : (a 2 (1 3) b 7)\nSalida esperada: 1\nResultado      : ")
(display (list-index number? '(a 2 (1 3) b 7)))
(display "\n\nLlamada #2\nEntrada        : (a (b c) 17 foo)\nSalida esperada: 0\nResultado      : ")
(display (list-index symbol? '(a (b c) 17 foo)))
(display "\n\nLlamada #3\nEntrada        : (1 2 (a b) 3)\nSalida esperada: #f\nResultado      : ")
(display (list-index symbol? '(1 2 (a b) 3)))

;; Fin punto #5

;; Inicio punto #6

;; swapper: 
;; Propósito:
;;
(define (swapper e1 e2 l)
  (if (null? l)
      '()
      (cons (if (equal? e1 (car l))
                e2
                (if (equal? e2 (car l))
                    e1
                    (car l)))
            (swapper e1 e2 (cdr l)))) )

;; Pruebas punto #6
(display "\n\nPunto #6\n\nLlamada #1\nEntrada        : 'a 'd '(a b c d)\nSalida esperada: (d b c a)\nResultado      : ")
(display (swapper 'a 'd '(a b c d)))
(display "\n\nLlamada #2\nEntrada        : 'a 'd '(a d () c d)\nSalida esperada: (d a () c a)\nResultado      : ")
(display (swapper 'a 'd '(a d () c d)))
(display "\n\nLlamada #3\nEntrada        : 'x 'y '(y y x y x y x x y)\nSalida esperada: (x x y x y x y y x)\nResultado      : ")
(display (swapper 'x 'y '(y y x y x y x x y)))

;; Fin punto #6

;; Inicio punto #7

;; cartesian-product: 
;; Propósito:
;;
(define (cartesian-product l1 l2)
  (cond ((null? l1) '())
        ((null? l2) '())
        (else (append (aux (car l1) l2)
                      (cartesian-product (cdr l1) l2)))))

;; aux: 
;; Propósito:
;;
(define (aux x l)
  (cond ((null? l) '())
        (else (cons (list x (car l))
                    (aux x (cdr l))))))

;; Pruebas punto #7
(display "\n\nPunto #7\n\nLlamada #1\nEntrada        : '(a b c) '(x y)\nSalida esperada: ((a x) (a y) (b x) (b y) (c x) (c y))\nResultado      : ")
(display (cartesian-product '(a b c) '(x y)))
(display "\n\nLlamada #2\nEntrada        : '(p q r) '(5 6 7)\nSalida esperada: ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))\nResultado      : ")
(display (cartesian-product '(p q r) '(5 6 7)))

;; Fin punto #7

;; Inicio punto #8

;; mapping: 
;; Propósito:
;;
(define mapping
  (lambda (f l1 l2)
    (cond
      [(or (null? l1) (null? l2)) '()]
      [(equal? (f (car l1)) (car l2))
       (cons (list (car l1) (car l2))
             (mapping f (cdr l1) (cdr l2)))]
      [else (mapping f (cdr l1) (cdr l2))])))

;; mapping-filtered: 
;; Propósito:
;;
(define (mapping-filtered func L1 L2)
  (define (aux F L1 L2 prev)
    (cond
      [(or (null? L1) (null? L2)) '()]
      [(equal? (F (car L1)) (car L2))
       (if (equal? prev (car L2))
           (aux F (cdr L1) (cdr L2) prev)
           (cons (list (car L1) (car L2))
                 (aux F (cdr L1) (cdr L2) (car L2))))] 
      [else (aux F (cdr L1) (cdr L2) prev)]))
  (aux func L1 L2))

;; Pruebas punto #8
(display "\n\nPunto #8\n\nLlamada #1\nEntrada        : (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))\nSalida esperada: ((1 2) (2 4) (3 6))\nResultado      : ")
(display (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6)))
(display "\n\nLlamada #2\nEntrada        : (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))\nSalida esperada: ((2 6))\nResultado      : ")
(display (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6)))
(display "\n\nLlamada #3\nEntrada        : (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))\nSalida esperada: ()\nResultado      : ")
(display (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12)))

;; Fin punto #8

;; Inicio punto #9

;; recorre-lista: 
;; Propósito:
;;
(define recorre-lista
  (lambda (l head acum)
    (cond
      [(null? l) acum]
      [(> head (car l)) (recorre-lista (cdr l) head (+ acum 1))]
      [else (recorre-lista (cdr l) head acum )]
    )
  )
)

;; inversions:
;; Propósito:
;;
(define inversions
  (lambda (l)
    (letrec ((inversions-aux
              (lambda (lista acum)
                (cond
                  [(null? lista) acum]
                  [else (inversions-aux (cdr lista) (+ acum (recorre-lista (cdr lista) (car lista) 0)))]
                )
              )
              )
            )
      (inversions-aux l 0)
    )
  )
)

;; Pruebas punto #9
(inversions '(2 3 8 6 1)) ;; -> Retorna 5
(inversions '(1 2 3 4)) ;; -> Retorna 0
(inversions '(3 2 1)) ;; -> Retorna 3

;; Fin punto #9

;; Inicio punto #10

;; up: 
;; Propósito:
;;
(define (up l)
  (if (null? l)
      '()
      (if (list? (car l))
          (append (car l) (up (cdr l)))
          (cons (car l) (up (cdr l))))))

;; Pruebas punto #10
(display "\n\nPunto #10\n\nLlamada #1\nEntrada        : '((1 2) (3 4))\nSalida esperada: (1 2 3 4)\nResultado      : ")
(display (up '((1 2) (3 4))))
(display "\n\nLlamada #2\nEntrada        : '((x (y)) z)\nSalida esperada: (x (y) z)\nResultado      : ")
(display (up '((x (y)) z)))

;; Fin punto #10

;; Inicio punto #11

;; zip: 
;; Propósito:
;;
(define zip
  (lambda (f lst1 lst2)
    (cond
      [(eqv? lst1 '()) empty]
      [(eqv? lst2 '()) empty]
      [else (cons(f (car lst1) (car lst2))(zip f (cdr lst1)(cdr lst2)))])))

;; Pruebas punto #11
(display "\n\nPunto #11\n\nLlamada #1\nEntrada        : (zip + '(1 4) '(6 2))\nSalida esperada: (7 6)\nResultado      : ")
(display (zip + '(1 4) '(6 2)))
(display "\n\nLlamada #2\nEntrada        : (zip * ’(11 5 6) ’(10 9 8))\nSalida esperada: (110 45 48)\nResultado      : ")
(display  (zip * '(11 5 6) '(10 9 8)))

;; Fin punto #11

;; Inicio punto #12

;; filter-acum: 
;; Propósito:
;;
(define filter-acum
  (lambda (a b f acum filt)
    (cond
      [(> a b) acum]
      [(filt a) (filter-acum (+ a 1) b f (f acum a) filt)]
      [else (filter-acum (+ a 1) b f acum filt)])))

;; Pruebas punto #12
(display "\n\nPunto #12\n\nLlamada #1\nEntrada        : (filter-acum 1 10 + 0 odd?)\nSalida esperada: 25 \nResultado      : ")
(display (filter-acum 1 10 + 0 odd?))
(display "\n\nLlamada #2\nEntrada        : (filter-acum 1 10 + 0 even?)\nSalida esperada: 30\nResultado      : ")
(display  (filter-acum 1 10 + 0 even?))

;; Fin punto #12

;; Inicio punto #13

;; operate: 
;; Propósito:
;;
(define operate
  (lambda (lrators lrands)
    (cond
      [(null? lrators) (car lrands)]
      [else (operate (cdr lrators) (cons (apply (car lrators) (list (car lrands) (cadr lrands))) (cddr lrands)))])))

;; Pruebas punto #13
(display "\n\nPunto #13\n\nLlamada #1\nEntrada        : (operate (list + * + - *) '(1 2 8 4 11 6))\nSalida esperada: 102 \nResultado      : ")
(display (operate (list + * + - *) '(1 2 8 4 11 6)))
(display "\n\nLlamada #2\nEntrada        : (operate (list *) '(4 5))\nSalida esperada: 20\nResultado      : ")
(display  (operate (list *) '(4 5)))

;; Fin punto #13

;; Inicio punto #14



;; Pruebas punto #14



;; Fin punto #14

;; Inicio punto #15



;; Pruebas punto #15



;; Fin punto #15

;; Inicio punto #16



;; Pruebas punto #16



;; Fin punto #16

;; Inicio punto #17

;; prod-scalar-matriz: 
;; Propósito:
;;
(define
  (prod-scalar-matriz mat vec)
  (cond
    [(or (null? mat) (null? vec)) '()]
    [else (cons (sum-prod (car mat) vec)(prod-scalar-matriz (cdr mat) vec))]))

;; sum-prod: 
;; Propósito:
;;
(define
  (sum-prod fil vec)
  (cond
    [(or (null? fil) (null? vec)) '()]
    [else (cons (* (car fil) (car vec))(sum-prod (cdr fil) (cdr vec)))]))

;; Pruebas punto #17
(display "\n\nPunto #17\n\nLlamada #1\nEntrada        : (prod-scalar-matriz '((1 1) (2 2)) '(2 3))\nSalida esperada: ((2 3) (4 6)) \nResultado      : ")
(display (prod-scalar-matriz '((1 1) (2 2)) '(2 3)))
(display "\n\nLlamada #2\nEntrada        : (prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))\nSalida esperada: ((2 3) (4 6) (6 9))\nResultado      : ")
(display  (prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3)))

;; Fin punto #17

;; Inicio punto #18

;; pascal: 
;; Propósito:
;;
(define
  (pascal n)
  (cond
    [(= n 1) '(1)]
    [else (agregar-extremos (pascal (- n 1)))]))

;; agregar-extremos: 
;; Propósito:
;;
(define
  (agregar-extremos lst-e)
  (cons 1 (agregar-interiores lst-e)))

;; agregar-interiores: 
;; Propósito:
;;
(define
  (agregar-interiores lst-i)
  (cond
    [(null? (cdr lst-i)) '(1)]
    [else (cons (+ (car lst-i) (cadr lst-i)) (agregar-interiores (cdr lst-i)))]))

;; Pruebas punto #18
(display "\n\nPunto #18\n\nLlamada #1\nEntrada        : (pascal 5)\nSalida esperada: (1 4 6 4 1) \nResultado      : ")
(display (pascal 5))
(display "\n\nLlamada #2\nEntrada        : (pascal 1)\nSalida esperada: (1)\nResultado      : ")
(display   (pascal 1))

;; Fin punto #18