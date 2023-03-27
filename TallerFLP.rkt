#lang eopl

(define suma (lambda (x y) (+ x y)))

(define invert
  (lambda (l p)
    (if (null? l)
        '()
        (append (if ())))))

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