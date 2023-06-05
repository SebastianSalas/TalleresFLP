#lang eopl

;; Final project solution - Fundamentos de lenguajes de programación
;; Friday June 9, 2023
;; Made by:
;; Janiert Sebastián Salas - 1941265
;; Jhon Alexander Valencia - 2042426
;; Diego Fernando Victoria - 2125877

;; Performs the lexical specification, which refers the way in wich the program is divided into lexical units
(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("->" (arbno (or digit letter #\newline whitespace))) skip)
    (letter("'" letter "'") symbol)
    (identificador ("$" letter (arbno (or letter digit))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto (letter (arbno (or letter ":" "?" "=" "'" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)))

;; Grammar of the language, which describes the rules of the language, used to define the terminal symbols, non-terminal symbols, and production rules.
(define gramatica
'(
  ;; Program
  (programa (expresion) un-programa)

  ;; Body
  (cuerpo (expresion (arbno expresion)) cuerpoc)

  ;; EXPRESSIONS
  
  (expresion (numero) numero-lit)
  (expresion ("\"" texto "\"") texto-lit)
  (expresion (identificador) id-exp)
  (expresion (boolean) expr-bool)
  (expresion ("(" expresion primitiva-bin expresion ")") primapp-bin-exp)
  (expresion (primitiva-un "(" expresion ")") primapp-un-exp)
  (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)
  (expresion ("if" expresion "then" expresion "[" "else" expresion "]" "end") condicional-exp)
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") proc-exp)
  (expresion ("eval" expresion "[" (separated-list expresion ",") "]") app-exp)
  (expresion ("def-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) defrec-exp)
  (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
  (expresion ("set" identificador "=" expresion) set-exp)
  (expresion ("var" (arbno identificador "=" expresion)"," "in" expresion ";") var-exp)
  (expresion ("const" (arbno identificador "=" expresion)"," "in" expresion ";")const-exp)
  (expresion (prim-lista "(" (separated-list expresion ",") ")") lista-exp)
  (expresion ("set-lista(" expresion "," expresion "," expresion ")") set-list)
  (expresion ("ref-lista(" expresion "," expresion ")") ref-list)
  (boolean (bool) trueFalse-exp)
  (expresion (pred-prim "(" expresion "," expresion ")") comparacion-exp)
  (boolean (oper-bin-bool "(" boolean "," boolean ")") op-log-exp)
  (boolean (oper-un-bool "(" boolean ")")  oper-un-bool-exp)
  (bool ("True") true-exp)
  (bool ("False") false-exp)


  ;; UNARY AND BINARY PRIMITIVES

  ;; Boolean
  (pred-prim ("<") menor-bool)
  (pred-prim (">") mayor-bool)
  (pred-prim ("<=") menorIgual-bool)
  (pred-prim (">=") mayorIgual-bool)
  (pred-prim ("=") igual-bool)
  (pred-prim ("<>") noIgual-bool)

  ;; Boolean (Logical)
  (oper-bin-bool ("and") and-bool)
  (oper-bin-bool ("or") or-bool)
  (oper-un-bool ("not") not-bool)

  ;; Lists
  (prim-lista ("crear-lista") crea-list-prim)
  (prim-lista ("'") lista-prim)
  (prim-lista ("append") append-prim)
  (prim-lista ("vacio")  vacio-prim)
  (prim-lista ("cabeza")  car-prim)
  (prim-lista ("cola")  cdr-prim)
  (prim-lista ("vacio?") null?-prim)
  (prim-lista ("lista?") list?-prim)

  ;; 
  (primitiva-bin ("+") primitiva-suma)
  (primitiva-bin ("~") primitiva-resta)
  (primitiva-bin ("/") primitiva-div)
  (primitiva-bin ("*") primitiva-multi)
  (primitiva-bin ("concat") primitiva-concat)

  ;; 
  (primitiva-un ("longitud") primitiva-longitud)
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1)))

;; INTERPRETER

;; Data types for the abstract syntax of the grammar built automatically:
(sllgen:make-define-datatypes especificacion-lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes especificacion-lexica gramatica)))

;; Parser, Scanner, Interface
;; The frontend (Integrated lexical analysis (scanner) and syntactic analysis (parser))
(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica))

;; The lexical analyzer (Scanner)
(define just-scan
  (sllgen:make-string-scanner especificacion-lexica gramatica))

;; The Interpreter (Frontend + Evaluation + Signal for reading)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (programa) (evaluar-programa programa))
    (sllgen:make-stream-parser 
      especificacion-lexica
      gramatica)))

;; EVALS

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (evaluar-expresion cuerpo (init-amb))))))
                 
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (datum) datum)

      (id-exp (id) (apply-env amb id))

      (texto-lit (texto) texto)

      (primapp-bin-exp (exp1 prim exp2)
                   (apply-prim-bin  exp1 prim exp2 amb))

      (lista-exp (prim rands)
                 (let ((args (eval-rands-list rands amb)))
                   (apply-prim-list prim args)))

      (set-list (lista pos dato)
                (let ((lista (evaluar-expresion lista amb))
                      (pos (evaluar-expresion pos amb))
                      (dato (evaluar-expresion dato amb)))
                      (set-position-list lista pos dato)))

      (ref-list (lista pos)
                (let ((lista (evaluar-expresion lista amb)))
                  (get-position-list lista (evaluar-expresion pos amb))))

      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-let-exp-rands exps amb)))
                           (evaluar-expresion cuerpo
                           (extend-amb ids args amb))))

      (condicional-exp (test-exp true-exp false-exp)
                        (if (valor-verdad? (evaluar-expresion test-exp amb))
                            (evaluar-expresion true-exp amb)
                            (evaluar-expresion false-exp amb)))

      (proc-exp (ids cuerpo) (closure ids cuerpo amb))

      (primapp-un-exp (prim exp) (apply-prim-un prim exp amb))

      (app-exp (exp exps)
               (let ((proc (evaluar-expresion exp amb))
                     (args (eval-exps exps amb)))

                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'evaluar-expresion
                                 "no es un procedimiento" proc))))

      (defrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-amb-recursively proc-names idss bodies amb)))

      (var-exp (ids rands body)
               (let ((args (eval-let-exp-rands rands amb)))
                 (evaluar-expresion body (extend-amb ids args amb))))

      (const-exp (ids rands body)
                 (begin
                   (eval-rands rands)
                   (cases expresion body
                     (set-exp (id exp) (eopl:error 'evaluar-expresion
                                 "No es posible modificar una constante" ))                     
                     (else (let ((args (eval-let-exp-rands rands amb)))
                         (evaluar-expresion body (extend-amb ids args amb))))
                   ))
               )

      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref amb id)
                  (evaluar-expresion rhs-exp amb))
                 0))

      (begin-exp (exp exps)
                 (let loop ((acc (evaluar-expresion exp amb))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (evaluar-expresion (car exps) 
                                               amb)
                              (cdr exps)))))

      (expr-bool (bool) (eval-bool bool amb))

      (comparacion-exp ( prim exp1 exp2)
                      (apply-comparacion-exp prim exp1 exp2  amb))
      
      )))

;; Eval-bool: evaluates all types of booleans in the program
(define eval-bool
  (lambda (expr-bool amb)
    (cases boolean expr-bool
      (trueFalse-exp (valor) (extr-bool-exp valor))
      (op-log-exp (op-log bool1 bool2)
                  (apply-oplog-exp op-log bool1 bool2 amb ))
      (oper-un-bool-exp (un prim)
                        (apply-un-exp un (eval-bool prim amb)))
      )
    )
  )

;; LISTS
;funcion auxiliar para obtener elemento en una posicion de una lista
(define get-position-list
  (lambda (lista pos)
    (list-ref lista pos)))

;funcion auxiliar para cambiar elemento en una posicion de una lista
(define set-position-list
  (lambda (lista n x)
    (letrec
        (
         (new-list
          (lambda (listaaux pos dato count)
            (cond
              [(eqv? listaaux '()) empty]
              [(eqv? count pos) (cons dato (new-list (cdr listaaux) pos dato (+ 1 count)))]
              [else (cons (car listaaux) (new-list (cdr listaaux) pos dato (+ 1 count)))]
              )
            )
          )
         )
      (new-list lista n x 0))
    )
  )


;; REFERENCES:
(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (list? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;; Targets and references

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

;; AUXILIARY EVALUATION FUNCTIONS
(define eval-rands
  (lambda (rands)
    (cond
      [(null? rands) #true]
      [else
       (cases expresion (car rands)
                     (set-exp (id exp) (eopl:error 'evaluar-expresion
                                 "No es posible modificar una constante" ))
                     (else (eval-rands (cdr rands))))]
      )))

;; Helper function that evaluates the rands of a list
(define eval-rands-list
  (lambda (exps env)
    (map
      (lambda (exp) (evaluar-expresion exp env))
      exps)))

;; Helper function that takes a list of expressions and an environment and evaluates each exp using eval-exp
(define eval-exps
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;; This function calls evaluate-expression with the current environment to determine the values ​​of the variables.
(define eval-exp
  (lambda (rand env)
    (cases expresion rand
      (id-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (evaluar-expresion x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (evaluar-expresion rand env))))

;; PRIMITIVES

;; Makes the application specification of the binary primitives
(define apply-prim-bin
  (lambda (exp1 prim exp2 amb)
    (cases primitiva-bin prim
      (primitiva-suma () (+ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-resta () (- (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-multi () (* (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-div () (/ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-concat () (string-append (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb))))))

;; Makes the application specification of the unary primitives
(define apply-prim-un
  (lambda (prim arg amb)
    (cases primitiva-un prim
      (primitiva-longitud () (string-length(evaluar-expresion arg amb)))
      (primitiva-add1 () (+ (evaluar-expresion arg amb ) 1))
      (primitiva-sub1 () (- (evaluar-expresion arg amb ) 1)))))

;; Control of boolean primitives
(define apply-comparacion-exp
  (lambda (prim exp1 exp2 amb)
    (cases pred-prim prim
      (menor-bool () (<= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (mayor-bool () (>= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (menorIgual-bool () (< (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (mayorIgual-bool () (> (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (noIgual-bool () (not (= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb))))
      (igual-bool () (= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb))))))

;; Control of boolean primitives (logical)
(define apply-oplog-exp
  (lambda (op-log bool1 bool2 amb)
    (cases oper-bin-bool op-log
      (and-bool () (and (eval-bool  bool1 amb) (eval-bool  bool2 amb)))
      (or-bool () (or (eval-bool  bool1 amb) (eval-bool  bool2 amb)))
      )))

(define extr-bool-exp
  (lambda (valor)
    (cases bool valor
      (true-exp () #t)
      (else #f)
      )
    )
  )

(define apply-un-exp
  (lambda (un prim)
    (cases oper-un-bool un
      [not-bool () (not prim)]
      )))

;; Makes the application specification of the primitives for lists
(define apply-prim-list
  (lambda (prims args)
    (cases prim-lista prims
      (crea-list-prim () args)
      (lista-prim () args)
      (vacio-prim () '())
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (append-prim () (cons (car args) (cadr args)))
      (null?-prim () (if (null? (car args)) 1 0))
      (list?-prim () (list? (car args)))
      )))

;; AMBIENTES

(define init-amb
  (lambda ()
     (extend-amb
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-amb))))

;; 
(define-datatype ambiente ambiente?
  (empty-amb-record)
  (extended-amb-record
   (syms (list-of symbol?))
   (vec vector?)
   (env ambiente?)))

(define scheme-value? (lambda (v) #t))

(define empty-amb  
  (lambda ()
    (empty-amb-record)))    

(define extend-amb
  (lambda (syms vals env)
    (extended-amb-record syms (list->vector vals) env)))

(define extend-amb-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-amb-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases ambiente env
      (empty-amb-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-amb-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;; PROCEDURES

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))

;; GENERAL AUXILIARY FUNCTIONS

;; To find the position of a symbol in the symbol list of an environment
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

;; Used to search for a variable in functions used in the language
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; Used to find the position of the given symbol in a given list
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;; Determines how to apply a value of type procedure
 (define apply-procedure
   (lambda (proc exps)
     (cases procval proc
      (closure (ids cuerpo amb)
               (evaluar-expresion cuerpo (extend-amb ids exps amb))))))

;; Function that returns a list of numbers from 0 to end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define valor-verdad?
  (lambda (x)
    (not (zero? x))))