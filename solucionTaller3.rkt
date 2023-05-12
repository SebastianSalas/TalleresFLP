#lang eopl

;; Solución taller #3 - Fundamentos de lenguajes de programación
;; Jueves, 11 de mayo de 2023
;; Hecho por:
;; Janiert Sebastián Salas - 1941265
;; Jhon Alexander Valencia - 2042426
;; Diego Fernando Victoria - 2125877

;; Performs the lexical specification, which refers the way in wich the program is divided into lexical units
(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("->" (arbno (or digit letter #\newline whitespace))) skip)  ;The comments starts with ->
    (identificador ("@" letter (arbno (or letter digit))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto (letter (arbno (or letter ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)))

;; Grammar of the language, which describes the rules of the language, used to define the terminal symbols, non-terminal symbols, and production rules.
(define gramatica
'(
  ;; Program
  (programa (expresion) un-programa)

  ;; Body
  (cuerpo (expresion (arbno expresion)) cuerpoc)

  ;; Expressions
  (expresion (numero) numero-lit)
  (expresion ("\"" texto "\"") texto-lit)
  (expresion (identificador) var-exp)
  (expresion ("(" expresion primitiva-bin expresion ")") primapp-bin-exp)
  (expresion (primitiva-un "(" expresion ")") primapp-un-exp)
  (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)
  (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
  (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc") proc-exp)
  (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
  (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) letrec-exp)

  ;; Binary primitive
  (primitiva-bin ("+") primitiva-suma)
  (primitiva-bin ("~") primitiva-resta)
  (primitiva-bin ("/") primitiva-div)
  (primitiva-bin ("*") primitiva-multi)
  (primitiva-bin ("concat") primitiva-concat)

  ;; Unary primitive
  (primitiva-un ("longitud") primitiva-longitud)
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1)))

;; Data types for the abstract syntax of the grammar built automatically:
(sllgen:make-define-datatypes especificacion-lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes especificacion-lexica gramatica)))

;; Parser, Scanner, Interface
;; The Frontend (Integrated lexical analysis (scanner) and syntactic analysis (parser))

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica))

;; The Lexical Analyzer (Scanner)

(define just-scan
  (sllgen:make-string-scanner especificacion-lexica gramatica))

;; The Interpreter (Frontend + Evaluation + Signal for Reading)

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (programa) (evaluar-programa  programa))
    (sllgen:make-stream-parser 
      especificacion-lexica
      gramatica)))

;; *****************************

;; Define data type ambiente
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

;; Creates empty environment
(define empty-amb  
  (lambda ()
    (empty-amb-record)))       ;llamado al constructor de ambiente vacío 

;; Receives an environment and extends it with the new syms and vals with the environment.
(define extend-amb
  (lambda (syms vals amb)
    (extended-amb-record syms vals amb)))

(define extend-amb-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-amb-record
     proc-names idss bodies old-env)))

;; Performs the search for a symbol in an environment
;; Used in the evaluation of an expression, to find a given variable in a given environment. If the variable is not found, it returns an error message
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

;; evaluar-programa
;; This is the main procedure, it takes an abstract syntax tree and returns a value.
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 (evaluar-expresion cuerpo (init-amb))))))

;; init-amb
;; Es una funcion cuyo dominio es un conjunto finito de variables y cuyo rango es el conjunto de todos los valores de Scheme, es usado usado para asociar las variables con sus valores en la implementacion de un lenguaje de programacion.
(define init-amb
  (lambda ()
    (extend-amb
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-amb))))

;; evaluar-expresion: <expresion> <ambiente> -> numero
;; Evaluates the expression in the input environment
;; This function uses an expression and an environment, and returns the value of the expression using that environment to find the values of the variables.
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (datum) datum)

      (var-exp (id) (buscar-variable amb id))

      (texto-lit (texto) texto)

      (primapp-bin-exp (exp1 prim exp2)
                   (apply-prim-bin  exp1 prim exp2 amb))

      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-exps exps amb)))
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

      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-amb-recursively proc-names idss bodies amb)))

      )))

;; eval-exps
;; This helper function takes a list of expressions and an environment and evaluates each exp using eval-exp
(define eval-exps
  (lambda (exps amb)
    (map (lambda (x) (eval-exp x amb)) exps)))

;; eval-exp
;; This function calls evaluate-expression with the current environment to determine the values ​​of the variables.
(define eval-exp
  (lambda (exp amb)
    (evaluar-expresion exp amb)))

;; apply-prim-bin
;; Performs the specification of binary primitives application
;; Used for addition, subtraction, multiplication, and division of defined numbers, and the concatenation of two expressions.
(define apply-prim-bin
  (lambda (exp1 prim exp2 amb)
    (cases primitiva-bin prim
      (primitiva-suma () (+ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-resta () (- (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-multi () (* (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-div () (/ (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb)))
      (primitiva-concat () (string-append (evaluar-expresion exp1 amb) (evaluar-expresion exp2 amb))))))

;; apply-prim-un
;; Performs the specification of unary primitives application
;; Used to determine the length of an expression, as well as to add and subtract one unit from a defined number.
(define apply-prim-un
  (lambda (prim arg amb)
    (cases primitiva-un prim
      (primitiva-longitud () (string-length(evaluar-expresion arg amb)))
      (primitiva-add1 () (+ (evaluar-expresion arg amb ) 1))
      (primitiva-sub1 () (- (evaluar-expresion arg amb ) 1)))))

;; Auxiliary

;; Auxiliary functions to find the position of a symbol in an environment's symbol list

;; Performs a search for the position of a symbol
;; Used for the search of a variable in functions used in the language
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; Performs the search for the index of a symbol in a list
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

;; valor-verdad?
;; This function takes an argument and determines whether it corresponds to the boolean value false (equals zero) or the boolean value true (any other value).
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;; Procedimientos
;; This is a constructor of the procedures, which are used to assign the ids, body and the environment of the procedures that we use in this language
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (cuerpo expresion?)
   (amb ambiente?)))

;; app-exp(exp exps)
;; Determines how to apply a value of type procedure
 (define apply-procedure
   (lambda (proc exps)
     (cases procval proc
      (closure (ids cuerpo amb)
               (evaluar-expresion cuerpo (extend-amb ids exps amb))))))


;********************************************************************************************************************************
;Pruebas Evaluables

;;a)El procedimiento areaCirculo permite calcular el area de un circulo dado un radio (A=PI*r*r). 

;declarar (
;
;      @radio=2.5;
;      @areaCirculo= procedimiento(@r) haga (3.1416*(@r*@r)) finProc
;
;     ) { 
;
;         evaluar @areaCirculo (@radio) finEval  
;
;       }
;------------------------------------------------------------------------------------------------------------------------------
;;b) Factorial de 5 y 10 de forma resursiva
;Uso con factorial de 5
;letrec
 ;     @factorial(@numero) = Si @numero entonces (@numero * evaluar @factorial(sub1(@numero)) finEval) sino 1 finSI
  ;    in evaluar @factorial(5) finEval
;Uso con factorial de 10
;letrec
 ;     @factorial(@numero) = Si @numero entonces (@numero * evaluar @factorial(sub1(@numero)) finEval) sino 1 finSI
  ;    in evaluar @factorial(10) finEval
;-------------------------------------------------------------------------------------------------------------
;;c) calcula la suma de dos numeros forma recursiva con las primitivas add1 y sub1. 
;letrec
;       @sumar(@a,@b) = Si @a entonces add1(evaluar @sumar(sub1(@a),@b)finEval) sino @b finSI
;       in
;       evaluar @sumar(4,5) finEval
;------------------------------------------------------------------------------------------------------------------------------

;;d)

;d-resta: calcula la resta de dos numeros de forma recursiva con las primitivas add1 y sub1. 

;letrec
;       @resta(@a,@b) = Si @b entonces sub1(evaluar @resta(@a,sub1(@b))finEval) sino @a finSI
;       in
;       evaluar @resta(10,3) finEval



;;d-multiplicacion: calcula la multiplicación de dos numeros de forma recursiva con las primitivas add1 y sub1. 
;  letrec
;    @restar(@a,@b) = Si @b entonces evaluar @restar(sub1(@a),sub1(@b)) finEval sino @a finSI
;
;    @suma(@a,@b) = Si @b entonces evaluar @suma(add1(@a),sub1(@b)) finEval sino @a finSI

;    @multiplicacion(@a,@b) = Si @b entonces evaluar @suma(@a , evaluar @multiplicacion(@a,sub1(@b)) finEval ) finEval sino evaluar @restar(@a,@a) finEval finSI 

;    in evaluar @multiplicacion(10,3) finEval

;-------------------------------------------------------------------------------------------------------------------------------

;;e) Crea una función @integrantes que muestre los nombres de los integrantes del grupo y adicionalmente crea un decorador que al invocarlo salude a los integrantes

; declarar(
;
;         @integrantes = procedimiento() haga "Sebastian-Diego-y-Alexander" finProc ;
;         @saludar = procedimiento (@proced) haga procedimiento() haga ("Hola:" concat evaluar @proced() finEval) finProc finProc
;
;         )
;
;         {
;          declarar(
;                   @decorate = evaluar @saludar (@integrantes) finEval
;                   )
;                  {
;                   evaluar @decorate () finEval
;                  }
;
;          }

;---------------------------------------------------------------------------------------------------------------------------------------

;;f)  Modifique el ejercicio anterior para que el decorador reciba como parámetro otro mensaje que debe ponerse al final de todo el string

; declarar(
;         @integrantes = procedimiento(@tx) haga ("Sebastian-Diego-Alexander-" concat @tx) finProc ;
;         @saludar = procedimiento (@proced) haga procedimiento(@tx) haga ("Hola:" concat evaluar @proced(@tx) finEval) finProc finProc 
;         )
;
;         {
;          declarar(
;
;                   @decorate = evaluar @saludar(@integrantes) finEval
;                             )
;                  {
;                   evaluar @decorate("EstudiantesCursoFLP") finEval
;                  }
;
;          }

;----------------------------------------------------------------------------------------------------------------------------------------
