#lang eopl


(define invert 
  (lambda(l)
    (if(null? l)
      '()
      (append invert-aux(car lista) invert(l))
    )
  )
)

(define invert-aux
  (lambda(l)
    (cond[(null? l) '()]
    [else cons(cons((cdr (car l)) (car l)) (invert-aux (cdr l)))]
    ))
)


(define suma (lambda (x y) (+ x y)))

(define invert
  (lambda (l p)
    (if (null? l)
        '()
        (append (if ())))))