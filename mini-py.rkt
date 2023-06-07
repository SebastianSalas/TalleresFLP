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
  (programa ((arbno class-decl) expresion) un-programa)

  ;; Body
  (cuerpo (expresion (arbno expresion)) cuerpoc)

  ;; EXPRESSIONS
  
  (expresion (numero) numero-lit)
  (expresion ("mostrar") mostrar-exp)
  (expresion ("\"" texto "\"") texto-lit)
  (expresion (identificador) id-exp)
  (expresion (boolean) expr-bool)
  (expresion (crea-bignum "(" (arbno numero) ")") bignum-exp)
  (expresion ("(" expresion primitiva-bin-entero expresion ")") primapp-bin-exp)
  (expresion (primitiva-un-entero "(" expresion ")") primapp-un-exp)
  (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)
  (expresion ("if" expresion "then" expresion "[" "else" expresion "]" "end") condicional-exp)
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") proc-exp)
  (expresion ("eval" expresion "[" (separated-list expresion ",") "]") app-exp)
  (expresion ("def-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) defrec-exp)
  (expresion ("while" boolean  "do" expresion "done") while-exp)
  (expresion ("for" identificador "=" expresion "to"  expresion "do" expresion "done") for-exp)
  (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
  (expresion ("set" identificador "=" expresion) set-exp)
  (expresion ("var" (arbno identificador "=" expresion)"," "in" expresion ";") var-exp)
  (expresion ("const" (arbno identificador "=" expresion)"," "in" expresion ";")const-exp)
  (expresion (prim-lista "(" (separated-list expresion ",") ")") lista-exp)
  (expresion ("set-lista(" expresion "," expresion "," expresion ")") set-list)
  (expresion ("ref-lista(" expresion "," expresion ")") ref-list)
  (expresion (primitiv-tupla "tupla" "[" (separated-list expresion ";") "]") tupla-exp)
  (expresion ("ref-tuple(" expresion "," expresion ")") ref-tupla)
  (expresion (prim-registro) reg-exp)
  (expresion (primbin-bignum "(" expresion "," "(" (arbno numero) ")" ")") controlbin-bignum)
  (expresion (primun-bignum "(" expresion ")" ) controlun-bignum)
  (expresion (prim-string) string-exp)

  (boolean (bool) trueFalse-exp)
  (boolean (pred-prim "(" expresion "," expresion ")") comparacion-exp)
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
  (prim-lista ("cons") cons-prim)
  (prim-lista ("vacio")  vacio-prim)
  (prim-lista ("cabeza")  car-prim)
  (prim-lista ("cola")  cdr-prim)
  (prim-lista ("vacio?") null?-prim)
  (prim-lista ("lista?") list?-prim)

  ;; Tuples
  (primitiv-tupla ("crear-tupla") primitiva-crear-tupla)
  (primitiv-tupla ("tupla?") primitiva-?tupla)
  (primitiv-tupla ("tvacio") primitiva-tvacio)
  (primitiv-tupla ("tvacio?") primitiva-?tvacio)
  (primitiv-tupla ("tcabeza") primitiva-tcabeza)
  (primitiv-tupla ("tcola") primitiva-tcola)

  ;; Tuples primitives
  (prim-registro ( "crear-registro" "{" (separated-list identificador "=" expresion ",") "}") primitiva-crearRegistro)
  (prim-registro ("registro?" "(" expresion ")") primitiva-registro?)
  (prim-registro ("ref-registro" "(" expresion "," identificador")") primitiva-refRegistro)
  (prim-registro ("set-registro" "("expresion "," identificador "," expresion ")") primitiva-setRegistro)

  ;; 
  (primitiva-bin-entero ("+") primitiva-suma)
  (primitiva-bin-entero ("-") primitiva-resta)
  (primitiva-bin-entero ("/") primitiva-div)
  (primitiva-bin-entero ("*") primitiva-multi)
  (primitiva-bin-entero ("%") primitiva-mod)

  ;;
  (primitiva-un-entero ("++") primitiva-add1)
  (primitiva-un-entero ("--") primitiva-sub1)

  ;; Bignum
  (crea-bignum ("x8") octa-exp)
  (crea-bignum ("x16") hexa-exp)
  (crea-bignum ("x32") triges-exp)
  (primbin-bignum ("sum-bignum") sum-bignum)
  (primbin-bignum ("sub-bignum") sub-bignum)
  (primbin-bignum ("mult-bignum") mult-bignum)
  (primbin-bignum ("pot-bignum") pot-bignum)
  (primun-bignum ("succes") succes)
  (primun-bignum ("predes") predes)

  ;; Strings
  (prim-string ("concat" "(" expresion "," expresion ")") concat-exp)
  (prim-string ("longitud" "(" expresion ")") longitud-exp)
  
  (class-decl                         
      ("class" identificador 
        "extends" identificador                   
         (arbno "field" identificador)
         (arbno method-decl)
         )
      a-class-decl)

  (method-decl
      ("method" identificador 
        "("  (separated-list identificador ",") ")" ; method ids
        expresion 
        )
      a-method-decl)

  (expresion 
      ("new" identificador "(" (separated-list expresion ",") ")")
      new-object-exp)

    (expresion
      ("send" expresion identificador
        "("  (separated-list expresion ",") ")")
      method-app-exp)

    (expresion                                
      ("super" identificador    "("  (separated-list expresion ",") ")")
      super-call-exp)))

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
      (un-programa (c-decls exp)
                   (elaborate-class-decls! c-decls)
                   (and
                   (set! lista-constantes '())
                   (evaluar-expresion exp (init-amb))
                   )))))
                 
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (datum) datum)

      (mostrar-exp () the-class-env)

      (id-exp (id) (apply-env amb id))

      (texto-lit (texto) texto)

      (primapp-bin-exp (exp1 prim exp2)
                   (apply-prim-bin  exp1 prim exp2 amb))

      (bignum-exp (exponente numeros) numeros)

      (controlbin-bignum (operador rands1 rands2) (apply-prim-bin-bignum operador (get-Bignum-estruct rands1) rands1  rands2 amb))

      (controlun-bignum (operador bignums) (apply-prim-una-bignum operador (get-Bignum-estruct bignums) (evaluar-expresion bignums amb)))

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

      (tupla-exp (prim rands)
                 (let ((args(eval-rands-list rands amb)))
                 (apply-prim-tupla prim args)))

      (ref-tupla (tupla pos)
                 (let ((tupla (evaluar-expresion tupla amb)))
                   (get-position-list tupla (evaluar-expresion pos amb))))

      (reg-exp (objeto) (eval-registro objeto amb))

      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-let-exp-rands exps amb)))
                           (evaluar-expresion cuerpo
                           (extend-amb ids args amb))))

      (condicional-exp (test-exp true-exp false-exp)
                        (if (evaluar-expresion test-exp amb)
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
                   (set! lista-constantes (append lista-constantes ids))
                   (let ((args (eval-let-exp-rands rands amb)))
                     (evaluar-expresion body (extend-amb ids args amb)))
                   )
               )

      (set-exp (id rhs-exp)
               (begin
                 (cond
                   [(buscar-elemento lista-constantes id) (eopl:error 'evaluar-expresion
                                 "No es posible modificar una constante" )]
                   [else (setref!
                  (apply-env-ref amb id)
                  (evaluar-expresion rhs-exp amb))])
                  1
                 ))

      (begin-exp (exp exps)
                 (let loop ((acc (evaluar-expresion exp amb))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (evaluar-expresion (car exps) 
                                               amb)
                              (cdr exps)))))

      (expr-bool (bool) (eval-bool bool amb))
      
      (string-exp (exp)
                  (cases prim-string exp
                    (concat-exp (exp1 exp2) (string-append (evaluar-expresion exp1 amb ) (evaluar-expresion exp2 amb )))
                    (longitud-exp (exp) (string-length (evaluar-expresion exp amb )))))
      (while-exp (exp-bool body)
                 (eval-while-exp exp-bool body amb ))
      
      (for-exp ( ids desde hasta cuerpo)
         (let
             ((to (evaluar-expresion desde amb))
                   (downto (evaluar-expresion hasta amb)))
            (let   loop ((i to))
                   (when (< i downto)
                      (evaluar-expresion cuerpo (extend-amb (list ids) (list i) amb))
                      (loop (+ 1 i))))))

      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands amb))
              (obj (new-object class-name)))
          (find-method-and-apply
            '$initialize class-name obj args)
          obj))

      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands amb))
              (obj (evaluar-expresion obj-exp amb)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))

      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands amb))
              (obj (apply-env amb 'self)))
          (find-method-and-apply
            method-name (apply-env amb '%super) obj args)))

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
      (comparacion-exp ( prim exp1 exp2)
                      (apply-comparacion-exp prim exp1 exp2  amb))
      )
    )
  )

;------------Eval-while --------------------------
(define eval-while-exp
  (lambda (exp-bool body amb )
      (let
          ((condicion (evaluar-expresion (expr-bool exp-bool) amb )))

        (if condicion
            (begin
              (evaluar-expresion body amb )
              (eval-while-exp expr-bool body amb ))
            1))))



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
    (or (number? x) (procval? x) (list? x) (object? x))))

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
    (if (target? (primitive-deref ref))
      (cases target (primitive-deref ref)
        (direct-target (expval) expval)
        (indirect-target (ref1)
                        (cases target (primitive-deref ref1)
                          (direct-target (expval) expval)
                          (indirect-target (p)
                                            (eopl:error 'deref
                                                        "Illegal reference: ~s" ref1)))))
    (primitive-deref ref))))

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
    (if (target? (primitive-deref ref))
      (let ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))
      (primitive-setref! ref expval))))

;; AUXILIARY EVALUATION FUNCTIONS

(define eval-rands
  (lambda (rands amb)
    (map (lambda (x) (evaluar-expresion x amb)) rands)))

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
    (cases primitiva-bin-entero prim
      (primitiva-suma () (+ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-resta () (- (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-multi () (* (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-div () (/ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-mod () (modulo (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      )))

;; Makes the application specification of the unary primitives
(define apply-prim-un
  (lambda (prim arg amb)
    (cases primitiva-un-entero prim
      (primitiva-add1 () (+ (evaluar-expresion arg amb ) 1))
      (primitiva-sub1 () (- (evaluar-expresion arg amb ) 1)))))

(define lista-constantes '())

(define buscar-elemento
  (lambda (lista elemento)
    (cond
      [(null? lista) #f]
      [else
       (if(eqv? (car lista) elemento) #t
          (buscar-elemento (cdr lista) elemento))])))

;; BIGNUM

(define get-Bignum-estruct
  (lambda (exp)
    (cases expresion exp
      (bignum-exp (exponente numeros) (get-exponente exponente))
      (else (eopl:error 'get-Bignum "No es un exponente ~s" exp)))))

(define get-exponente
  (lambda (estruct)
    (cases crea-bignum estruct
                    (octa-exp () 8)
                    (hexa-exp () 16)
                    (triges-exp () 32))))

(define apply-prim-una-bignum
  (lambda (oper exp numeros)
    (cases primun-bignum oper
      (predes () (predecessor numeros exp))
      (succes () (successor numeros exp)))))

(define apply-prim-bin-bignum
  (lambda (oper exp lista1 lista2 amb)
    (cases primbin-bignum oper
      (sum-bignum () (suma-bignum (evaluar-expresion lista1 amb) lista2 exp))
      (sub-bignum () (resta-bignum (evaluar-expresion lista1 amb) lista2 exp))
      (mult-bignum () (multi-bignum (evaluar-expresion lista1 amb) lista2 exp))
      (pot-bignum () (potencia-bignum (evaluar-expresion lista1 amb) lista2 exp)))))

(define successor (lambda (n max)
                   (cond
                     [(null? n) (cons 1 empty)]
                     [(< (car n) max) (cons (+ (car n) 1)(cdr n))]
                     [else (cons 1 (successor (cdr n) max))]
                     )))

(define predecessor (lambda (n max)
                      (cond
                        [(eqv? n empty) eopl:error 'top "No tiene predecesor"]
                        [(and (eqv? (car n) 1) (eqv? (cdr n) empty)) empty]
                        [(> (car n) 1) (cons (- (car n) 1)(cdr n))]
                        [else (cons max (predecessor (cdr n) max))]
                        )))

(define suma-bignum
  (lambda (x y exp)
    (if (null? x)
        y
        (successor (suma-bignum (predecessor x exp) y exp) exp))))

(define resta-bignum
  (lambda (x y exp)
    (if (null? y)
        x
        (predecessor (resta-bignum  x (predecessor y exp) exp) exp))))

(define multi-bignum
  (lambda (x y exp)
    (if (null? x)
        ('())
        (suma-bignum (multi-bignum (predecessor x exp) y exp) y exp))
    ))

(define potencia-bignum
  (lambda (x y exp)
    (if (null? y)
        (successor y exp)
        (multi-bignum (potencia-bignum x (predecessor y exp) exp) x exp))))

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
      (append-prim () (append (car args) (cadr args)))
      (cons-prim () (cons (car args) (cons (cadr args) '())))
      (null?-prim () (if (null? (car args)) #true #false))
      (list?-prim () (list? (car args)))
      )))

;; Tuples
(define apply-prim-tupla
  (lambda (prim-tupla args)
    (cases primitiv-tupla prim-tupla
      (primitiva-crear-tupla () args)
      (primitiva-?tupla () (if(null? args) #t (pair? args)))
      (primitiva-tvacio () "tupla[]")
      (primitiva-?tvacio () (if (null? args) #t #f))
      (primitiva-tcabeza () (car args))
      (primitiva-tcola () (cdr args))
      )))

;;REGISTRES
(define eval-registro
  (lambda (registro-exp amb)
    (cases prim-registro registro-exp
      [primitiva-crearRegistro (key list-exp)
                    (list (list->vector key)(list->vector (eval-rands list-exp amb)))]

      [primitiva-registro? (registro) (let (( registro (evaluar-expresion registro amb)))
                  (if (list? registro)
                      (and (vector? (car registro)) (vector? (cadr registro )))
                      #f))]

      [primitiva-refRegistro (registro key) (buscar-key key (car (evaluar-expresion registro amb)) (cadr (evaluar-expresion registro amb)) amb)]

      [primitiva-setRegistro (rgstr name value) (let ((array (evaluar-expresion rgstr amb))
                                       (newitem (evaluar-expresion value amb)))
                                   (vector-set! (cadr array) (- (length (member name (reverse (vector->list (car array))))) 1) newitem))]      
    )))

(define buscar-key
  (lambda (key list-keys list-values amb)
    (cond
      [(null? list-keys) (eopl:error buscar-key "Is not found ~s" key)]
      [(equal? key (car (vector->list list-keys))) (car (vector->list list-values))] 
      [else (buscar-key key (list->vector (cdr (vector->list list-keys))) (list->vector (cdr (vector->list list-values))) amb)]
      )
    ))

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

(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

(define the-class-env '())

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))

(define lookup-class                    
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) (eopl:error 'lookup-class
                       "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

(define elaborate-class-decl!
  (lambda (c-decl)
    (let ((super-name (class-decl->super-name c-decl)))
      (let ((field-ids  (append
                          (class-name->field-ids super-name)
                          (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            super-name
            (length field-ids)
            field-ids
            (roll-up-method-decls
              c-decl super-name field-ids)))))))

(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl super-name field-ids))
      (class-decl->method-decls c-decl))))

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (make-vector (class-name->field-length class-name)))))

(define-datatype method method?
  (a-method
    (method-decl method-decl?)
    (super-name symbol?)
    (field-ids (list-of symbol?))))

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let loop ((host-name host-name))
      (if (eqv? host-name '$object)
          (eopl:error 'find-method-and-apply
            "No method for name ~s" m-name)
          (let ((method (lookup-method m-name
                          (class-name->methods host-name))))
            (if (method? method)
                (apply-method method host-name self args)
                (loop (class-name->super-name host-name))))))))

(define apply-method
  (lambda (method host-name self args)
    (let ((ids (method->ids method))
          (body (method->body method))
          (super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self)))
      (evaluar-expresion body
        (extend-amb
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (extend-env-refs field-ids fields (empty-amb)))))))

(define method-environment? (list-of method?)) 

(define lookup-method                   
  (lambda (m-name methods)
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

(define extend-env-refs
  (lambda (syms vec env)
    (extended-amb-record syms vec env)))

(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;; 
(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        class-name))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        super-name))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-length))))

(define class->field-ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-ids))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        methods))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define object->class-decl
  (lambda (obj)
    (lookup-class (object->class-name obj))))

(define object->field-ids
  (lambda (object)
    (class->field-ids
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (if (eqv? class-name '$object) '()
      (class->field-ids (lookup-class class-name)))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name '$object) '()
      (class->methods (lookup-class class-name)))))

(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name '$object)
        0
        (class->field-length (lookup-class class-name)))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) meth-decl))))

(define method->super-name
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl super-name field-ids) field-ids))))

(define method->method-name
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))

;; GENERAL AUXILIARY FUNCTIONS

;; To find the position of a symbol in the symbol list of an environment
(define rib-find-position 
  (lambda (sym los)
    (list-find-last-position sym los)))

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

(interpretador)