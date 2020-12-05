#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
(require "p2-base.rkt")
(require "mk.rkt")
;(require "p2-soln.rkt")

; (require racket/pretty) ; you might find the prettyprint function useful

(provide type-check-spreadsheet fill-in)

;-------------------------------------------------------------------------------
; * Task 3: Type Checking a Spreadsheet *
;-------------------------------------------------------------------------------

#|
(type-check-spreadsheet spreadsheet)
  spreadsheet: An spreadsheet AST described in the project handout.

  Returns a list of booleans, representing whether the types of each column
  is correctly annotated.
|#
(define (type-check-spreadsheet spreadsheet)
  (let* ([defs (rest (second spreadsheet)) ] ; exclude the symbol 'def
         [columns (rest (third spreadsheet))] ; exclude the symbol 'columns
         [env (create-env defs '())]
         )
    (check-col-types columns env))) ; return a list of bools that rep. whether the column type is correctly annotated

; Helper functions for task 3
#|
   (create-env definitions typeenv)
    definitions: A list of definitions. Ex: '((voting-age 18) (concat (lambda (x y) (++ x y))) )
    typeenv: an environment with identifiers and their types

    Returns a new environment with the definitions and their types added.

    Sample usage: 
    > (create-env '((voting-age 18) (concat (lambda (x y) (++ x y))) ) '())
     '((concat (str str) str) (voting-age . num))
|#
(define (create-env definitions typeenv)
  (if (empty? definitions)
      typeenv
      (let* ([def (first definitions)]
             [id (first def)]
             [expr (second def)]
             [type-lst (run* (out) (typeo expr typeenv out))] ; a list of possible types
             [type (if (empty? type-lst)
                       'error
                       (first type-lst))]
             [id-type-pair (cons id type)]
             [typeenv^ (cons id-type-pair typeenv)]) ; add the first definition to the environment
             
        (create-env (rest definitions) typeenv^)
  )))

  (define (check-col-types cols env)
    (void)) ;TODO implement this!
 
;-------------------------------------------------------------------------------
; * Task 4: Synthesizing Programs *
;-------------------------------------------------------------------------------

#|
(fill-in lvar expr type n)
  lvar: The logic variable to be filled in
  expr: An expression following the spreadsheet grammar, with a logic variable
        somewhere in the expression to be filled in.
  type: The desired type of the expression after BLANK is filled in.
  n:    The maximum number of results to return.

  Macro that runs a miniKanren query that will replace the symbol `BLANK`
  in the spreadsheet expression `expr` so that the type of the expression
  is consistent with `type`. The query returns at most `n` results.
|#

(define-syntax fill-in
  (syntax-rules ()
    [(fill-in lvar expr type n)
     (void) ; TODO
    ]))


