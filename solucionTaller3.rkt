#lang eopl

;; Solución taller #3 - Fundamentos de lenguajes de programación
;; Domingo, 11 de mayo de 2023
;; Hecho por:
;; Janiert Sebastián Salas - 1941265
;; Jhon Alexander Valencia - 2042426
;; Diego Fernando Victoria - 2125877

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