#lang racket #| CSC324 Fall 2020: Project 2 |#

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide typeof typeo)

;-------------------------------------------------------------------------------
; * Task 1: A Simple Type Inferencer *
;-------------------------------------------------------------------------------

#|
(typeof expr typeenv)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types

  Returns the type of the expression expr: 'num, 'str, 'bool, 'error
|#
(define/match (typeof expr typeenv)

  ; Constants
  [((? number? expr) typeenv) 'num]
  [((? boolean? expr) typeenv) 'bool]
  [((? string? expr) typeenv) 'str]
    
  ; Identifiers
  [((? symbol? expr) typeenv)
   (let* ([result (lookup expr typeenv)])
     (if (equal? result #f) 'error result))]
  
  ; Builtins
  ;[(lst typeenv)
  ; (let* ([fn-symbol (first lst)])
  ;   (cond
  ;     [(and (equal? fn-symbol '+) (equal? 'num (typeof (second lst) typeenv)) ]))]


  ;;
  [((list '+ (? equal? 'num (typeof a typeenv)) (? equal? 'num (typeof b typeenv)) typeenv) 'num]
  [((list '-  (? number? a) (? number? b)) typeenv) 'num]
  [((list '* (? number? a) (? number? b)) typeenv) 'num]
  [((list '/ (? number? a) (? number? b)) typeenv) 'num]
  [((list '> (? number? a) (? number? b)) typeenv) 'bool]
  [((list '>= (? number? a) (? number? b)) typeenv) 'bool]
  [((list '= (? number? a) (? number? b)) typeenv) 'bool]
  [((list '! (? number? a)) typeenv) 'bool]
  [((list '++ (? string? a) (? string? b)) typeenv) 'str]
  [((list 'num->str (? number? a)) typeenv) 'str]
  [((list 'len (? string? a)) typeenv) 'num]
  [(_ typenv) 'error]

  ; Function Calls
  ; TODO
  )

; Helper functions for Task 1

#|
(lookup key alst)
  elem: A key in the association list
  alst: An association list 

  Returns the value corresponding to the first time the key appears in the
  association list, or #f if the key does not exist.

  Examples:
  > (lookup 'b '((a . 3) (b . 4)))
  4
  > (lookup 'b '((a . 3) (b . 4) (b . 5)))
  4
  > (lookup 'c '((a . 3) (b . 4) (b . 5)))
  #f
|#
(define (lookup expr alst)
  (cond
    [(equal? alst '()) #f]
    [else
     ;check first element
     (let* ([first-entry (first alst)])
       (if (equal? expr (car first-entry))
           (cdr first-entry)
           (lookup expr (rest alst))))]))

; Add your helper functions here

;-------------------------------------------------------------------------------
; * Task 2: A Type Inferencer Relation in miniKanren
;-------------------------------------------------------------------------------

#|
(typeo expr typeenv type)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types
  type: The type of the expression

  The relational form of the `typeof` function
|#
(define (typeo expr env type)
  (conde
   ; constants: numbero, stringo, and boolo are miniKanren builtin relations
   ((numbero expr)
    (== type 'num))
   ; TODO

   ; identifier: symbolo is a miniKanren builtin relation
   ; TODO

   ; builtins
   ; TODO

   ; function calls
   ; TODO

   ; function definitions
   ; TODO
   ))


; Helper functions for Task 2

#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#
(define (lookupo key alst value)
  (void))


; Add your helper functions here

