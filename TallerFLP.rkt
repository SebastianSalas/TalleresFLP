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