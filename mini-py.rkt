#lang eopl

;; Solución proyecto final - Fundamentos de lenguajes de programación
;; Viernes, 9 de junio de 2023
;; Hecho por:
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
    (bool (or "@true" "@false") boolean)
    (texto (letter (arbno (or letter ":" "?" "=" "'" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)))

;; Grammar of the language, which describes the rules of the language, used to define the terminal symbols, non-terminal symbols, and production rules.
(define gramatica
'(
  ;; Program
  (programa (expresion) un-programa)

  ;; Body
  (cuerpo (expresion (arbno expresion)) cuerpoc)

  ;; Expressions
  (expresion (identificador) var-exp)
  (expresion ("var" (arbno identificador "=" (expresion))"," "in" expresion ";") varLet-exp)
  (expresion ("const" (arbno identificador "=" (expresion))"," "in" expresion ";") const-exp)
  (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) rec-exp)
  (expresion (numero) numero-lit)
  (expresion ("\"" texto "\"") cadena-lit)
  (expresion (bool) bool-lit)
  (expresion ("[" (separated-list expresion ";") "]") list-exp)
  (expresion ("tupla" "[" (separated-list expresion ";") "]") tupla-exp)
  (expresion ("{" (identificador "=" expresion (arbno ";" identificador "=" expresion)) "}") registro-exp)
  (expr-bool (pred-prim "(" expresion "," expresion ")") pred-exp)
  (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-bin-exp)
  (expr-bool (bool) bool-exp)
  (expr-bool (oper-un-bool "(" expr-bool ")" ) oper-un-exp)
  (expression ("begin" expresion (arbno ";" expression) "end") begin-exp)
  (expresion ("if" expr-bool "then" expresion ("[" "else" expresion "]")"end") condicional-exp)
  (expresion ("while" expr-bool "do" expresion "done") while-exp)
  (expresion ("for" identificador "=" expresion (or "to" "downto") expresion "do" expresion "done") for-exp)
  (expresion ("def" "(" (separated-list identificador ",") ")" "{" expresion "}") def-exp)
  (expresion ("eval" expresion "(" (separated-list expresion ",") ")") app-exp)
  (expresion ("(" expresion primitiva-bin expresion ")") primapp-bin-exp)
  (expresion (primitiva-un "(" expresion ")") primapp-un-exp)
  (expresion ("def-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) defrec-exp)

;; Integers
  (primitiva-bin ("+") primitiva-suma)
  (primitiva-bin ("-") primitiva-resta)
  (primitiva-bin ("/") primitiva-div)
  (primitiva-bin ("*") primitiva-multi)
  (primitiva-bin ("%") primitiva-mod)
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1))

;; Hexadecimal
  (primitiva-bin ("+") primitiva-suma-hex)
  (primitiva-bin ("-") primitiva-resta-hex)
  (primitiva-bin ("*") primitiva-multi-hex)
  (primitiva-un ("add1") primitiva-add1-hex)
  (primitiva-un ("sub1") primitiva-sub1-hex)

;; Boolean
  (pred-prim ("<") menor)
  (pred-prim (">") mayor)
  (pred-prim ("<=") menor_igual)
  (pred-prim (">=") mayor_igual)
  (pred-prim ("==") igual)
  (oper-bin-bool ("and") and-op)
  (oper-bin-bool ("or") or-op)
  (oper-un-bool ("not") not)

;; String
 (primitiva-bin ("concat") primitiva-concat)
 (primitiva-un ("longitud") primitiva-longitud)

;; List
  (primitiva-un ("vacio?") primitiva-vacio?)
  (primitiva-un ("vacio") primitiva-vacio)
  (primitiva-un ("crear-lista") primitiva-crear-lista)
  (primitiva-un ("lista?") primitiva-lista)
  (primitiva-un ("cabeza") primitiva-cabeza)
  (primitiva-un ("cola") primitiva-cola)
  (primitiva-un ("ref-list") primitiva-refList)
  (primitiva-bin ("set-list") primitiva-setList)
  (primitiva-bin ("append") primitiva-append)

;; Tuples
  (primitiva-un ("crear-tupla") primitiva-crear-tupla)
  (primitiva-un ("tupla?") primitiva-tupla?)
  (primitiva-bin ("ref-tuple") primitiva-refTuple)

;; Registers
  (primitiva-un ("registros?") primitiva-registros?)
  (primitiva-un ("crear-resgistro") primitiva-crearRegistro)
  (primitiva-bin ("ref-resgistro") primitiva-refRegistro)
  (primitiva-bin ("set-registro") primitiva-setRegistro))

;; Interpreter

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
    (lambda (programa) (evaluar-programa  programa))
    (sllgen:make-stream-parser 
      especificacion-lexica
      gramatica)))

;; *****************************