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