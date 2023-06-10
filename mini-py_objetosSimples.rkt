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
  (expresion (letter) letter-lit)
  (expresion ("\"" texto "\"") texto-lit)
  (expresion (printf) printf-exp)
  (printf ("printf" "(" expresion ")") printf-def)
  (expresion ("printObj" "(" expresion ")") printPbj-exp)
  (expresion (identificador) id-exp)
  (expresion (boolean) expr-bool)
  (expresion (crea-bignum "(" (arbno numero) ")") bignum-exp)
  (expresion ("(" expresion primitiva-bin-entero expresion ")") primapp-bin-exp)
  (expresion (primitiva-un-entero "(" expresion ")") primapp-un-exp)
  (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)
  (expresion ("if" expresion "then" expresion "[" "else" expresion "]" "end") condicional-exp)
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") proc-exp)
  (expresion ("eval" expresion "[" (separated-list expresion ",") "]") app-exp)
  (expresion ("def-rec" (separated-list identificador "(" (separated-list identificador ",") ")" "=" expresion ",")  "in" expresion) defrec-exp)
  (expresion ("while" boolean  "do" expresion "done") while-exp)
  (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "done") for-exp)
  (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
  (expresion ("set" identificador "=" expresion) set-exp)
  (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion ";") var-exp)
  (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion ";")const-exp)
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
  (prim-lista ("tamano") tamano-prim)

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

      (letter-lit (letra) letra)
      
      (texto-lit (texto) texto)

      (printf-exp (message) (eval-printf message amb))

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

      (expr-bool (boolean) (eval-bool boolean amb))
      
      (string-exp (exp)
                  (cases prim-string exp
                    (concat-exp (exp1 exp2) (string-append (evaluar-expresion exp1 amb ) (evaluar-expresion exp2 amb )))
                    (longitud-exp (exp) (string-length (evaluar-expresion exp amb )))))
       (while-exp (boolean exp)
                  (let   loop ((i 0))
                   (when (eval-bool boolean amb)
                      (evaluar-expresion exp amb)
                      (loop (+ 1 i)))))
      
      (for-exp ( exp desde hasta cuerpo)
         (let
             ((de (evaluar-expresion desde amb))
                   (to (evaluar-expresion hasta amb)))
           (let   loop ((i de))
              (when (<= i to)
                      (evaluar-expresion cuerpo (extend-amb (list exp) (list i) amb))
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
              (obj (apply-env amb '$self)))
          (find-method-and-apply
            method-name (apply-env amb '%super) obj args)))
      (printPbj-exp (obj) (print-objeto (evaluar-expresion obj amb)))

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

(define eval-printf
  (lambda (msg amb)
    (cases printf msg
      (printf-def (message) (display (evaluar-expresion message amb))))))



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
    (or (number? x) (procval? x) (list? x) (part? x)
        (string? x) (vector? x) (pair? x))))

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
      (lista-exp (prim id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion rand env)))))
  )

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
        x
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
      (menor-bool () (< (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (mayor-bool () (> (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (menorIgual-bool () (<= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (mayorIgual-bool () (>= (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
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

;; AMBIENTS

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

;;

;;SIMPLE OBJECTS

(define print-objeto
  (lambda (value)
    (map
     (lambda (x)
       (cases part x
         (a-part (class-name fields) fields)
        ) 
     )
       value )

   )
)

(define aux
   (lambda (x)
     x))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name '$object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (class-decl->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))))))

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name '$object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (method host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (evaluar-expresion body
        (extend-amb
          (cons '%super (cons '$self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-amb)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

(define extend-env-refs
  (lambda (syms vec env)
    (extended-amb-record syms vec env)))

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

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

;; GENERAL AUXILIARY FUNCTIONS

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