#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
(require "p2-base.rkt") ; (require "p2-soln.rkt")
(require "mk.rkt")

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
   
    (check-col-types columns env '())
    )) ; return a list of bools that rep. whether the column type is correctly annotated

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


#| (check-col-types cols env rlst)
    cols: a list of computed or value columns
    env: a type environment
    rlst: the list of bools to be returned

    This function checks if each column has the correct type annotations. If so,
    add #t to the return lst <rlst>. Otherwise, add #f to <rlst>.

|#

(define/match (check-col-types cols env rlst)
  [('() env rlst) rlst]
  
  [((cons (list id type (cons 'values vs)) rest) env rlst)
   ; value columns 
   (let* ([is-correct-type (check-type vs type env)]
          [env^ (if is-correct-type
                    (cons (cons id type) env)
                    (cons (cons id 'error) env))] ; add value column to the environment since computed columns depend on value columns

          [rlst^ (append rlst (list is-correct-type))])

     (check-col-types rest env^ rlst^))]
     
  [((cons (list id type (list 'computed expr)) rest) env^ rlst)
   
   ; computed columns
   (void) ; TODO implement this 
      
   ]

  )

(define (col-checker cols typeenv types)
  (if (empty? cols)
      #t
      (and (equal? (first types) (lookup typeenv (first cols)))
           (col-checker (rest cols) typeenv (rest types)))))

(define (check-type items expected-type env)
  (if (empty? items)
      #t
      (let* ([fst (first items)]
             [rst (rest items)]
             [check (equal? (typeof fst env) expected-type)])
        (if (equal? check #f)
            #f     
            (check-type rst expected-type env)))))
        
#|(define (check-compute items expected-type env)
  (void)) ; TODO


(define (check-col-types cols env rlst)
  (if (empty? cols)
      rlst
      (let* ([fst (first cols)]
             [rst (rest cols)]
             [expected-type (second fst)]
             [items (third fst)]
             [columntype (first items)])
        (if (equal? columntype 'values)
            (check-col-types rst env (append (list (check-value (rest items) expected-type)) rlst))
            (if (equal? columntype 'computed)
                (check-col-types rst env (append (list (check-compute (rest items) expected-type env)) rlst))
                'error))
            
        )))
|#
             
#|
(lookup lst key)
  Takes in a list of associations and returns a value given a key. 
|#
(define (lookup lst key)
  (cond [(empty? lst) 'error]
        [(equal? key (car (first lst)))
         (cdr (first lst))]
        [(lookup (rest lst) key)]))
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


