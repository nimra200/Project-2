#lang racket #| CSC324 Fall 2020: Project 2 |#

(require "mk.rkt")
;(require racket/pretty) ; you might find the prettyprint function useful

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
  [(expr typeenv)
   (let* ([funcid (first expr)]
          [functype (typeof funcid typeenv)]
          [restexpr (funcall (rest expr) typeenv functype)]) 
     restexpr)]
         
  [(_ typenv) 'error]
  
  )

; Helper functions for Task 1
#|
   (typehelp a b expected-input expected-ouput env)

   <a>, <b>, <expected-input>, <expected-output> are types. The environment is <env>. 
   If <a> and <b> have the expected input type, then return the expected output type.
|#
(define (typehelp a b expected-input expected-output env)
  (if (and (equal? expected-input (typeof a env))
           (equal? expected-input (typeof b env)))
      expected-output 'error))

#|
(funcall funcargs typeenv functype)
  funcargs: a list of arguments
  typeenv: the type environment
  functype: a function type

  Recurse through the given arguments to see if types match with the function input.
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
  (comparelst lst1 lst2)
  Takes in two lists and conpares them. Returns true if equal, false otherwise. 
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
   ((stringo expr)
    (== type 'str))
   ((boolo expr)
    (== type 'bool))

   ; identifier: symbolo is a miniKanren builtin relation
   ((symbolo expr)
    (lookupo expr env type))

   ((fresh (a b)
           (conde ((== expr (list '+ a b))
                   (typeo a env 'num)
                   (typeo b env 'num)
                   (== type 'num))
                  
                  ((== expr (list '- a b))
                   (typeo a env 'num)
                   (typeo b env 'num)
                   (== type 'num))

                  ((== expr (list '* a b))
                   (typeo a env 'num)
                   (typeo b env 'num)
                   (== type 'num))

                  ((== expr (list '/ a b))
                   (typeo a env 'num)
                   (typeo b env 'num)
                   (== type 'num))

                  ((== expr (list '> a b))
                   (typeo a env 'num)
                   (typeo b env 'num)
                   (== type 'bool))

                  ((== expr (list '>= a b))
                   (typeo a env 'num)
                   (typeo b env 'num)
                   (== type 'bool))

                  ((== expr (list '= a b))
                   (typeo a env 'num)
                   (typeo b env 'num)
                   (== type 'bool))

                  ((== expr (list '++ a b))
                   (typeo a env 'str)
                   (typeo b env 'str)
                   (== type 'str))

                  ((== expr (list 'num->str a))
                   (typeo a env 'num)
                   (== type 'str))

                  ((== expr (list '! a))
                   (typeo a env 'bool)
                   (== type 'bool))

                  ((== expr (list 'len a))
                   (typeo a env 'str)
                   (== type 'num)))))

  
   ; function calls
   ((fresh (func-id func-args func-inputs func-output)
           (== expr (cons func-id func-args))
           (typeo func-id env (list func-inputs func-output)) ; a function type looks like '((<input type> ...) <output>)
           (== type func-output) ;the function output should match with the given type
           (type-listo func-args env func-inputs))) ; each function argument should match with its expected type


   ; function definitions
   ((fresh (param body param_type r_type env^ pairs)
           (conde ((== expr (list 'lambda param body))
                   (== type (list param_type r_type))
                   (pairo param param_type pairs) ; create an association between the params and their types
                   (appendo pairs env env^) ; add the new pair to the env
                   (typeo body env^ r_type))))) ; recrusively call typeo on the updated env
                  
                   
   
   
                 
   ))


; Helper functions for Task 2

#| (pairo param param_type pairs)
    Pairo is a relation which takes
    the paramaters for a lambda function and a list of types.
    The relation holds true if <pairs> is a list of pairs which map a parameter to its type.

    Sample usage:

  >>> (run 1 (v) (pairo (list 'x 'y) (list 'str 'num) v))
   '(((x . str) (y . num)))
|#
(define (pairo param param_type pairs)
  (conde ((== param '())
          (== param_type '())
          (== pairs '()))
         ((fresh (fstparam restparam fstptype restptype fpair pairs^)
                 (== param (cons fstparam restparam))
                 (== param_type (cons fstptype restptype))
                 (== fpair (cons fstparam fstptype))
                 (appendo (list fpair) pairs^ pairs)
                 (pairo restparam restptype pairs^)))))

  

#|(type-listo exprs env types)
  
   exprs: a list of expressions
   env: a type environment
   types: a list of types
   The relation holds true if each type in <types> corresponds to an expression in <exprs>.

  Sample usage:
  >>> (run* (types) (type-listo '(1 a #t) '((a . str)) types))
  '((num str bool))
|#
(define (type-listo exprs env types)
  (conde
   ((== exprs '() ) (== types '() ))
   ; check if each expression in <exprs> has the correct type by:
   ; 1. ensuring the the first expression has the correct type
   ; 2. recursively checking the rest of the expressions have the correct types 

   ((fresh (expr exprs^ type types^)
          (== exprs (cons expr exprs^))
          (== types (cons type types^))
          (typeo expr env type)
          (type-listo exprs^ env types^)))))
   
#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#

(define (lookupo key alst value)
  (fresh (fkey fval rest)
         (== (cons (cons fkey fval) rest) alst)
         (conde
          ((== key fkey) (== value fval))
          ((=/= key fkey) (lookupo key rest value))))) ; the order of args to lookupo was incorrect


; Add your helper functions here
; A relation that holds true if `obj` is a boolean. 
(define (boolo obj)
  (conde ((== obj #f))
         ((== obj #t))))

; The relational form of the `append` function from lecture.
(define (appendo xs ys xsys)
  (conde ((== xs'())
          (== ys xsys))
         ((fresh (x xs^ xsys^)
                 (== xs (cons x xs^))
                 (== xsys (cons x xsys^))
                 (appendo xs^ ys xsys^)))))


                 
         




