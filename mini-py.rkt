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
    (identificador ( letter (arbno (or letter digit))) symbol)
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
  (expresion (identificador) var-exp)
  (expresion ("var" (arbno indentificador "=" (expresion))"," "in" expresion ";")varLet-exp)
  (expresion ("const" (arbno indentificador "=" (expresion))"," "in" expresion ";")const-exp)
  (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) rec-exp)
  (expresion (numero) numero-lit)
  (expresion (" " "" texto " " "") cadena-lit)
  (expresion (bool) bool-lit)
  (expression ("begin" expresion (arbno ";" expression) "end") begin-exp)
  (expresion ("if" expr-bool "then" expresion ("[" "else" expresion "]")"end") condicional-exp)
  (expresion ("while" expr-bool "do" expresion "done") while-exp)
  (expresion ("for" identificador "=" expresion (or "to" "downto") expresion "do" expresion "done") for-exp)
  (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc") proc-exp)
  (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
  (expresion ("(" expresion primitiva-bin expresion ")") primapp-bin-exp)
  (expresion (primitiva-un "(" expresion ")") primapp-un-exp)
  (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}" ) variableLocal-exp)

;; Integers
  (primitiva-bin ("+") primitiva-suma)
  (primitiva-bin ("-") primitiva-resta)
  (primitiva-bin ("/") primitiva-div)
  (primitiva-bin ("*") primitiva-multi)
  ;;(primitiva-bin ("%") primitiva-modulo)
  (primitiva-un ("longitud") primitiva-longitud)
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1)))

;; Hexadecimal
  (primitiva-bin ("+") primitiva-suma-ex)
  (primitiva-bin ("-") primitiva-resta)
  (primitiva-bin ("*") primitiva-multi)
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1)

;; String
 (primitiva-bin ("concat") primitiva-concat)
 (primitiva-un ("longitud") primitiva-longitud)

;; List
  (primitiva-un ("vacio?") primitiva-vacio?)
  (primitiva-un ("vacio") primitiva-vacio)
  (primitiva-un ("crear") primitiva-crear)
  (primitiva-un ("cabeza") primitiva-cabeza)
  (primitiva-un ("cola") primitiva-cola)
  (primitiva-bi ("append") primitiva-append)
  (primitiva-un ("ref-list") primitiva-refList)
  (primitiva-bi ("set-list") primitiva-setList)

;; List / Tuples
  (primitiva-un ("vacio?") primitiva-vacio?)
  (primitiva-un ("vacio") primitiva-vacio)
  (primitiva-un ("crear") primitiva-crear)
  (primitiva-un ("cabeza") primitiva-cabeza)
  (primitiva-un ("cola") primitiva-cola)

;; Tuples
  (primitiva-un ("tupla?") primitiva-tupla?)
  (primitiva-bi ("ref-tuple") primitiva-refTuple)

;; Registers
  (primitiva-un ("registros?") primitiva-registros?)
  (primitiva-un ("crear-resgistro") primitiva-crearRegistro)
  (primitiva-bi ("ref-resgistro") primitiva-refRegistro)
  (primitiva-bi ("set-registro") primitiva-setRegistro)