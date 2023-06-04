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
    (comentario ("->" (arbno (or digit letter #\newline whitespace))) skip)  ;; The comments starts with ->
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
  (expresion (variable) varLet-exp)
  (bool ("true") true-exp)
  (bool ("false") false-exp)

  ;; Identifier
  (expresion (identificador) var-exp)
  
  ; Definitions
  (variable ("var" (separated-list identificador "=" expresion ",") ";" "in"  expresion ) variable-def);Hacer el manejo de los valores mutables
  ;(expression ("const" (separated-list identifier "=" expression ",") ";" "in" "{" expression "}") const-exp)
  ;(expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" "{" expression "}") rec-exp)
  ;(expresion ("const" (arbno identificador "=" (expresion))"," "in" expresion ";")const-exp)
  ;(expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) rec-exp)


  ;; Data constructors
  ;(expresion ("[" (separated-list expresion ";") "]") list-exp)
  ;(expresion ("tupla" "[" (separated-list expresion ";") "]") tupla-exp)
  ;(expresion ("{" (identificador "=" expresion (arbno ";" identificador "=" expresion)) "}") registro-exp)
  ;(expr-bool ("comparar" "(" expresion pred-prim expresion ")") comparar-exp)
  ;(expr-bool ("(" expr-bool oper-bin-bool expr-bool ")") oper-bin-exp)
  ;(expr-bool (oper-un-bool "(" expr-bool ")") oper-un-exp)
  ;(expr-bool (bool) bool-exp)


  ;; Control structure
  ;(expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
  (expresion ("if" expresion "then" expresion "[" "else" expresion "]" "end") condicional-exp)
  ;(expresion ("while" expr-bool "do" expresion "done") while-exp)
  ;(expresion ("for" identificador "=" expresion (or "to" "downto") expresion "do" expresion "done") for-exp)
  ;(expresion ("set" identificador "=" expresion) set-exp)

  ;; Others
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") def-exp) ;create procedure
  (expresion ("eval" expresion "[" (separated-list expresion ",") "]") app-exp);invoke procedure
  (expresion ("(" expresion primitiva-bin-entero expresion ")") primapp-bin-exp)
  (expresion (primitiva-un-entero "(" expresion ")") primapp-un-exp)
  (expresion ("def-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) defrec-exp)

;; UNARY AND BINARY PRIMITIVES

;; Integers
;; binary primitive
  (primitiva-bin-entero ("+") primitiva-suma)
  (primitiva-bin-entero ("-") primitiva-resta)
  (primitiva-bin-entero ("/") primitiva-div)
  (primitiva-bin-entero ("*") primitiva-multi)
  (primitiva-bin-entero ("%") primitiva-mod)

;; unary primitive
  (primitiva-un-entero ("++") primitiva-add1)
  (primitiva-un-entero ("--") primitiva-sub1)

;; Hexadecimal
;; binary primitive
  ;(primitiva-bin-hexa ("+_hexa") primitiva-suma-hex)
  ;(primitiva-bin-hexa ("-_hexa") primitiva-resta-hex)
  ;(primitiva-bin-hexa ("*_hexa") primitiva-multi-hex)

;; unary primitive
  ;(primitiva-un-hexa ("++_hexa") primitiva-add1-hex)
  ;(primitiva-un-hexa ("--_hexa") primitiva-sub1-hex)

;; Boolean
  (pred-prim ("<") menor-prim)
  (pred-prim (">") mayor-prim)
  (pred-prim ("<=") menor_igual-prim)
  (pred-prim (">=") mayor_igual-prim)
  (pred-prim ("==") igual-prim)
  (pred-prim ("<>") noIgual-prim)
  (oper-bin-bool ("and") and-op)
  (oper-bin-bool ("or") or-op)
  (oper-un-bool ("not") not-op)

;; String
  ;(primitiva-bin ("concat") primitiva-concat)
  ;(primitiva-un ("longitud") primitiva-longitud)

;; List
  ;(primitiva-un ("vacio?") primitiva-vacio?)
  ;(primitiva-un ("vacio") primitiva-vacio)
  ;(primitiva-un ("crear-lista") primitiva-crear-lista)
  ;(primitiva-un ("lista?") primitiva-lista)
  ;(primitiva-un ("cabeza") primitiva-cabeza)
  ;(primitiva-un ("cola") primitiva-cola)
  ;(primitiva-un ("ref-list") primitiva-refList)
  ;(primitiva-bin ("set-list") primitiva-setList)
  ;(primitiva-bin ("append") primitiva-append)

;; Tuples
  ;(primitiva-un ("crear-tupla") primitiva-crear-tupla)
  ;(primitiva-un ("tupla?") primitiva-tupla?)
  ;(primitiva-bin ("ref-tuple") primitiva-refTuple)

;; Registers
  ;(primitiva-un ("registros?") primitiva-registros?)
  ;(primitiva-un ("crear-resgistro") primitiva-crearRegistro)
  ;(primitiva-bin ("ref-resgistro") primitiva-refRegistro)
  ;(primitiva-bin ("set-registro") primitiva-setRegistro)
  ))

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

;; *****************************  EVALS ***************************************

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (evaluar-expresion cuerpo (init-amb))))))
                 
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (datum) datum)

      (var-exp (id) (buscar-variable amb id))

      (texto-lit (texto) texto)

      (primapp-bin-exp (exp1 prim exp2)
                   (apply-prim-bin  exp1 prim exp2 amb))

      (condicional-exp (test-exp true-exp false-exp)
                        (if (valor-verdad? (evaluar-expresion test-exp amb))
                            (evaluar-expresion true-exp amb)
                            (evaluar-expresion false-exp amb)))

      (def-exp (ids cuerpo) (closure ids cuerpo amb))

      (primapp-un-exp (prim exp) (apply-prim-un prim exp amb))

      (app-exp (exp exps)
               (let ((proc (evaluar-expresion exp amb))
                     (args (eval-exps exps amb)))

                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'evaluar-expresion
                                 "no es un procedimiento" proc))))

      (defrec-exp (proc-names idss bodies defrec-body)
                  (evaluar-expresion defrec-body
                                   (extend-amb-recursively proc-names idss bodies amb)))

      ;VarLet-exp
      (varLet-exp (variables) (evaluar-var variables amb))

      ;exp-bool 
      ;(exp-bool (exp) (eval-bool exp amb))

;       ;while-exp
;      (while-exp (expr-bool cuerpo)
;                 (evaluar-while-exp expr-bool cuerpo amb ))
;      
;      ;for-exp
;      (for-exp (identificador numero-lit expr-bool cuerpo)
;               (evaluar-for-exp identificador numero-lit expr-bool cuerpo amb))
      )))

;eval-var
(define evaluar-var
  (lambda (struct amb)
    (cases variable struct
      (variable-def (ids exps cuerpo)
               (let ((args (eval-var-exp-rands exps amb)))
                 (evaluar-expresion cuerpo (extend-amb ids args amb))))
      )
    )
  )
;---------------------------------------------------------------------------
;; Eval-bool
;(define eval-bool
;  (lambda (exp amb )
;    (cases expr-bool amb 
;      [comparar-exp (a prim b) (primitiva-comparativa (if (expresion? exp) (evaluar-expresion a amb ) ) prim (eval-exp b amb ))]  
;      [oper-bin-exp (a op b) (apply-operate (eval-bool (evaluar-expresion a amb ) amb ) op (eval-bool (evaluar-expresion b amb ) amb ))]
;      [oper-un-exp (un prim) (apply-un-exp un (eval-bool prim amb ))]
;      [bool-exp (bool-prim) (cases bool bool-prim
;                              [true-exp () #t]
;                              [false-exp () #f])]  
;      [else #f]
;      )
;    ))

;; evaluar-while-exp
;(define evaluar-while-exp
;  (lambda (expr-bool cuerpo amb)
;      (let
;          ((condicion (evaluar-expresion (evaluar-expresion expr-bool) amb )))
;
;        (if condicion
;            (begin
;              (evaluar-expresion cuerpo amb)
;              (evaluar-while-exp expr-bool cuerpo amb ))
;            1))))


;; Define reference and target datatypes

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype referencia referencia?
  (a-ref (position integer?)
         (vec vector?)))

;; References and targets

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (string? x) (not (boolean? x)) (symbol? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (referencia? x)
         (cases referencia x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitiva-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitiva-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Referencia ilegal: ~s" ref1)))))))

(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitiva-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitiva-setref! ref (direct-target expval)))))

(define primitiva-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))
;; ************************* ENVIRONMENTS ****************************

(define init-amb
  (lambda ()
     (empty-amb)))

(define-datatype ambiente ambiente?
  (empty-amb-record)
  (extended-amb-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (amb ambiente?))
  (recursively-extended-amb-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (amb ambiente?)))

(define scheme-value? (lambda (v) #t))


(define empty-amb  
  (lambda ()
    (empty-amb-record)))    


(define extend-amb
  (lambda (syms vals amb)
    (extended-amb-record syms vals amb)))


(define extend-amb-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-amb-record
     proc-names idss bodies old-env)))


(define buscar-variable
  (lambda (amb sym)
    (cases ambiente amb
      (empty-amb-record ()
                        (eopl:error 'apply-amb "Error, la variable no existe" sym))
      (extended-amb-record (syms vals amb)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable amb sym))))
      (recursively-extended-amb-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      amb)
                                             (buscar-variable old-env sym)))))))
;; ******************** BOOLEAN PRIMITIVES **********************
(define apply-un-exp
  (lambda (un prim)
    (cases oper-un-bool un
      [not-op () (not prim)]
      )))

(define apply-operate
  (lambda (a op b)
    (cases oper-bin-bool op
      [and-op () (and a b)]
      [or-op () (or a b)])))

(define primitiva-comparativa
  (lambda (a prim b)
    (cases pred-prim prim
      [mayor-prim () (> a b)]
      [mayor_igual-prim () (>= a b)]
      [menor-prim () (< a b)]
      [menor_igual-prim () (<= a b)]
      [igual-prim () (= a b)]
      [noIgual-prim () (not (= a b))]
      )))

;; ***************************** INTEGERS PRIMITIVES ****************************
;apply-prim-un
;Used to know the length of an expression
(define apply-prim-un
  (lambda (prim arg amb)
    (cases primitiva-un-entero prim
      ;(primitiva-longitud () (string-length(evaluar-expresion arg amb)))
      (primitiva-add1 () (+ (evaluar-expresion arg amb ) 1))
      (primitiva-sub1 () (- (evaluar-expresion arg amb ) 1)))))

;apply-primitive
;Used to +,-,* and / of the numbers
(define apply-prim-bin
  (lambda (exp1 prim exp2 amb)
    (cases primitiva-bin-entero prim
      (primitiva-suma () (+ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-resta () (- (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-multi () (* (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-div () (/ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-mod () (modulo (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      ;(primitiva-concat () (string-append (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      )))

;; ****************************** PROCEDURES *********************************

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))


 (define apply-procedure
   (lambda (proc exps)
     (cases procval proc
      (closure (ids cuerpo amb)
               (evaluar-expresion cuerpo (extend-amb ids exps amb))))))
; ****************************** AUXILIARY FUNCTIONS *********************************

(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))


(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define eval-exps
  (lambda (exps amb)
    (map (lambda (x) (eval-exp x amb)) exps)))

(define eval-exp
  (lambda (exp amb)
    (cases expresion exp
      (varLet-exp (id)
               (indirect-target
                (let ((ref (buscar-variable amb id)))
                  (cases target (primitiva-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion exp amb))))))

(define eval-var-exp-rands
  (lambda (exps amb)
    (map (lambda (x) (eval-var-exp-rand x amb))
         exps)))

(define eval-var-exp-rand
  (lambda (exp amb)
    (direct-target (evaluar-expresion exp amb))))
    