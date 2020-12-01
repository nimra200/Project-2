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

  ; Builtin
  [((list '+ a b) typeenv) (typehelp a b 'num 'num typeenv)]
  [((list '- a b) typeenv) (typehelp a b 'num 'num typeenv)]
  [((list '* a b) typeenv) (typehelp a b 'num 'num typeenv)]
  [((list '/ a b) typeenv) (typehelp a b 'num 'num typeenv)]
  [((list '> a b) typeenv) (typehelp a b 'num 'bool typeenv)]
  [((list '> a b) typeenv) (typehelp a b 'num 'bool typeenv)]
  [((list '>= a b) typeenv) (typehelp a b 'num 'bool typeenv)]
  [((list '= a b) typeenv) (typehelp a b 'num 'bool typeenv)] 

  [((list '! a) typeenv) 
   (if (equal? 'num (typeof a typeenv))
       'bool 'error)]

  [((list '++ a b) typeenv) (typehelp a b 'str typeenv)]

  [((list 'num->str a) typeenv)
   (if (equal? 'num (typeof a typeenv))
       'str 'error)]

  [((list 'len a) typeenv)
   (if (equal? 'num (typeof a typeenv))
       'num 'error)]

  
  ; Function Calls
  [(expr typeenv) ;expr must be a list
   (let* ([funcid (first expr)]
          [functype (typeof funcid typeenv)]
          [restexpr (funcall (rest expr) typeenv functype)]) 
     restexpr)]
         
  [(_ typenv) 'error]
  
  )

; Helper functions for Task 1
#|
   Takes in two args with the expected type, and a function output type.
   If the params have the same type, then return output type. Else, return error.
|#
(define (typehelp a b expected-input expected-output env)
  (if (and (equal? expected-input (typeof a env))
           (equal? expected-input (typeof b env)))
      expected-output 'error))

#|
  Recurse through args to see if types match with the function input.
  Returns the desired output type of function if successful otherwise error.
|#
(define (funcall funcargs typeenv functype)
  (let* ([expected (first functype)]
         [actualinputs (map (lambda (arg) (typeof arg typeenv)) funcargs)])
    (if (and (equal? (length expected) (length actualinputs))
             (comparelst expected actualinputs))
        (second functype)
        'error)))

#|
  Function that takes in two lists and conpares to see if equal,
  return #f if not.
  lst 1 = '(num num)
  lst 2 = '((num) (num))
|#
  
(define (comparelst lst1 lst2)
  (if (and (empty? lst1) (empty? lst2)) #t
      (and (equal? (first lst1) (first lst2)) (comparelst (rest lst1) (rest lst2))) 
      ))

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

