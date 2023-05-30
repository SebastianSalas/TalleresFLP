#lang eopl
;; Solución proyecto final - Fundamentos de lenguajes de programación
;; Viernes, 9 de junio de 2023
;; Hecho por:
;; Janiert Sebastián Salas - 1941265
;; Jhon Alexander Valencia - 2042426
;; Diego Fernando Victoria - 2125877

;**************** ESPECIFICACION LEXICA *****************
(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("->" (arbno (or digit letter #\newline whitespace))) skip)  ;The comments starts with ->
    (identificador ("@" letter (arbno (or letter digit))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto (letter (arbno (or letter ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)
    (booleano ("false") symbol)
    (booleano ("true") symbol)
    )
  )


;******************* GRAMATICA **************************

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

  ;; Binary primitive
  (primitiva-bin ("+") primitiva-suma)
  (primitiva-bin ("~") primitiva-resta)
  (primitiva-bin ("/") primitiva-div)
  (primitiva-bin ("*") primitiva-multi)
  (primitiva-binaria ("%") primitiva-modulo)


  (primitiva-binaria ("and") primitiva-and)
  (primitiva-binaria ("or") primitiva-or)
  (primitiva-bin ("concat") primitiva-concat)
  (primitiva-binaria ("append") primitiva-append)
  
  (primitiva-binaria (">") primitiva-mayorque)
  (primitiva-binaria ("<") primitiva-menorque)
  (primitiva-binaria (">=") primitiva-mayorigual)
  (primitiva-binaria ("<=") primitiva-menorigual)
  (primitiva-binaria ("<>") primitiva-diferente)
  (primitiva-binaria ("==") primitiva-igualque)
  ;; Unary primitive
  (primitiva-un ("longitud") primitiva-longitud)
  (primitiva-un ("add1") primitiva-add1)
  (primitiva-un ("sub1") primitiva-sub1)
  (primitiva-unaria ("not") primitiva-not)
  (primitiva-unaria ("empty?") primitiva-empty?)
  (primitiva-unaria ("head") primitiva-cabeza)
  (primitiva-unaria ("tail") primitiva-col)
  ))
;****************** CREACION DE LOS DATATYPES ***********************
(sllgen:make-define-datatypes especificacion-lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes especificacion-lexica gramatica)))

;*************** PARSER, SCANNER E INTERPRETADOR *************************
;Parser
(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica))

;Scanner
(define just-scan
  (sllgen:make-string-scanner especificacion-lexica gramatica))

;Interpretador
(define interpretador
  (sllgen:make-rep-loop  "-->"
                         (lambda (pgm) (eval-program  pgm)) 
                         (sllgen:make-stream-parser 
                          especificacion-lexica
                          gramatica)))

;********************* EVALUACION DEL PROGRAMA ****************************
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (cuerpo)
                 "eval-exp"))))
