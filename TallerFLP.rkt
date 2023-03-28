#lang eopl

(define suma (lambda (x y) (+ x y)))

;; Punto 1
;; invert :
;; Proposito:
;; L x P -> L' : Procedimiento que invierte las tuplas de una lista L y devuelve otra lista L'
;; aplicandole el predicado P
;;

(define multiplo5? (lambda (x) (if(eqv? (modulo x 5) 0)
                                  #t
                                  #f)))

(define invert-aux
  (lambda(l)
    (cond
      [(null? l) '()]
      [else (cons (list (car (cdr (car l))) (car (car l))) (invert-aux (cdr l)))]
    ))
)

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

;; Pruebas
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((5 9) (10 90) (82 7) ) multiplo5? )
(invert '((6 9) (10 90) (82 7) ) odd? )

(define down
  (lambda (l)
    (if (null? l)
      '()
      (cons (cons  (car l) '()) (down (cdr l))))))

(display "\nPunto #2\n\nLlamada #1\nEntrada        : (1 2 3)\nSalida esperada: ((1) (2) (3))\nResultado      : ")
(display (down '(1 2 3)))
(display "\n\nLlamada #2\nEntrada        : ((una) (buena) (idea))\nSalida esperada: (((una)) ((buena)) ((idea)))\nResultado      : ")
(display (down '((una) (buena) (idea))))
(display "\n\nLlamada #3\nEntrada        : (un (objeto (mas)) complicado)\nSalida esperada: ((un) ((objeto (mas))) (complicado))\nResultado      : ")
(display (down '(un (objeto (mas)) complicado)))

(define filter-in
  (lambda (p l)
    (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (filter-in p (cdr l)))
          (filter-in p (cdr l))))))

(display "\n\nPunto #4\n\nLlamada #1\nEntrada        : (a 2 (1 3) b 7)\nSalida esperada: (2 7)\nResultado      : ")
(display (filter-in number? '(a 2 (1 3) b 7)))
(display "\n\nLlamada #2\nEntrada        : (a (b c) 17 foo)\nSalida esperada: (a foo)\nResultado      : ")
(display (filter-in symbol? '(a (b c) 17 foo)))
(display "\n\nLlamada #3\nEntrada        : (a b u univalle racket flp 28 90 (1 2 3))\nSalida esperada: (univalle racket flp)\nResultado      : ")
(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3))))

(define (list-index p l)
  (define (aux l n)
    (if (null? l)
        #f
        (if (p (car l))
            n
            (aux (cdr l) (+ n 1)))))
  (aux l 0))

(display "\n\nPunto #5\n\nLlamada #1\nEntrada        : (a 2 (1 3) b 7)\nSalida esperada: 1\nResultado      : ")
(display (list-index number? '(a 2 (1 3) b 7)))
(display "\n\nLlamada #2\nEntrada        : (a (b c) 17 foo)\nSalida esperada: 0\nResultado      : ")
(display (list-index symbol? '(a (b c) 17 foo)))
(display "\n\nLlamada #3\nEntrada        : (1 2 (a b) 3)\nSalida esperada: #f\nResultado      : ")
(display (list-index symbol? '(1 2 (a b) 3)))

(define (swapper e1 e2 l)
  (if (null? l)
      '()
      (cons (if (equal? e1 (car l))
                e2
                (if (equal? e2 (car l))
                    e1
                    (car l)))
            (swapper e1 e2 (cdr l)))) )

(display "\n\nPunto #6\n\nLlamada #1\nEntrada        : 'a 'd '(a b c d)\nSalida esperada: (d b c a)\nResultado      : ")
(display (swapper 'a 'd '(a b c d)))
(display "\n\nLlamada #2\nEntrada        : 'a 'd '(a d () c d)\nSalida esperada: (d a () c a)\nResultado      : ")
(display (swapper 'a 'd '(a d () c d)))
(display "\n\nLlamada #3\nEntrada        : 'x 'y '(y y x y x y x x y)\nSalida esperada: (x x y x y x y y x)\nResultado      : ")
(display (swapper 'x 'y '(y y x y x y x x y)))

(define (cartesian-product l1 l2)
  (cond ((null? l1) '())
        ((null? l2) '())
        (else (append (aux (car l1) l2)
                      (cartesian-product (cdr l1) l2)))))

(define (aux x l)
  (cond ((null? l) '())
        (else (cons (list x (car l))
                    (aux x (cdr l))))))

(display "\n\nPunto #7\n\nLlamada #1\nEntrada        : '(a b c) '(x y)\nSalida esperada: ((a x) (a y) (b x) (b y) (c x) (c y))\nResultado      : ")
(display (cartesian-product '(a b c) '(x y)))
(display "\n\nLlamada #2\nEntrada        : '(p q r) '(5 6 7)\nSalida esperada: ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))\nResultado      : ")
(display (cartesian-product '(p q r) '(5 6 7)))

(define (up l)
  (if (null? l)
      '()
      (if (list? (car l))
          (append (car l) (up (cdr l)))
          (cons (car l) (up (cdr l))))))

(display "\n\nPunto #10\n\nLlamada #1\nEntrada        : '((1 2) (3 4))\nSalida esperada: (1 2 3 4)\nResultado      : ")
(display (up '((1 2) (3 4))))
(display "\n\nLlamada #2\nEntrada        : '((x (y)) z)\nSalida esperada: (x (y) z)\nResultado      : ")
(display (up '((x (y)) z)))

(define zip
  (lambda (f lst1 lst2)
    (cond
      [(eqv? lst1 '()) empty]
      [(eqv? lst2 '()) empty]
      [else (cons(f (car lst1) (car lst2))(zip f (cdr lst1)(cdr lst2)))])))

(display "\n\nPunto #11\n\nLlamada #1\nEntrada        : (zip + '(1 4) '(6 2))\nSalida esperada: (7 6)\nResultado      : ")
(display (zip + '(1 4) '(6 2)))
(display "\n\nLlamada #2\nEntrada        : (zip * ’(11 5 6) ’(10 9 8))\nSalida esperada: (110 45 48)\nResultado      : ")
(display  (zip * '(11 5 6) '(10 9 8)))

(define filter-acum
  (lambda (a b f acum filt)
    (cond
      [(> a b) acum]
      [(filt a) (filter-acum (+ a 1) b f (f acum a) filt)]
      [else (filter-acum (+ a 1) b f acum filt)])))
(display "\n\nPunto #12\n\nLlamada #1\nEntrada        : (filter-acum 1 10 + 0 odd?)\nSalida esperada: 25 \nResultado      : ")
(display (filter-acum 1 10 + 0 odd?))
(display "\n\nLlamada #2\nEntrada        : (filter-acum 1 10 + 0 even?)\nSalida esperada: 30\nResultado      : ")
(display  (filter-acum 1 10 + 0 even?))

