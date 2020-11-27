### quasiquote

(de quasiquote Lst
   (macro
      (macro
         (^(macro (_quasiquote ^ Lst))) ) ) )

(de _quasiquote X
   (use BQ
      (deflate
         (recur (X Acc)
            (ifn X
               (flip Acc)
               (if (pair (car X))
                  (if (atom (cdr @))
                     (recurse (cdr X) (cons (car X) Acc))
                     (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
                  (case (car X)
                     ("`"  (if BQ  # if backquote encountered already
                              (recurse (cdr  X) (cons (car X) Acc))  # then do nothing
                              # else mark encountered and 'quote' the list
                              (on BQ)
                              (recurse (cdr  X) (cons (lit 'quote) Acc)) ) ) 
                     (","  (recurse (cddr X) (cons (list 'list (cadr X)) '^ Acc)) )
                     (",@" (recurse (cddr X) (cons (cadr X) '^ Acc)) )
                     (T    (recurse (cdr  X) (cons (car X) Acc))) ) ) ) ) ) ) )


### read-macros (for m.l files)

# leq - create bindings for eval-quasiquote forms
#
# ~(leq [X 2 Y 3]
#    `(* ,X ,Y) )

(de let-eval-quasiquote-readmacro Lst
   (let [(Args . Body) (leqargs Lst)]
      (macro
         '((let ^ Args (eval (quasiquote ^ Body )))) ) ) )

# leq* - bindings in nested eval-quasiquote forms. used in macro-defining
# macros.
#
#  (leq ,[...]          # ','  - computed at fn definition time
#     ~(leq* ,,[...]    # ",,' - computed at defined fn's run time
#     ... )

(de let-eval-quasiquote-star-readmacro Lst
   (let [(Args . Body) (leqargs Lst)]
      (macro
         '(",""`"(let ^ Args (eval (quasiquote ^ Body ))) ) ) ) )

(de leqargs (Lst)
   (let [L (_leqargs Lst)
         I (index (find pair L) L)
         Args (head I L)
         Body (tail (- I) L)]
      (macro '((^ Args) ^ Body)) ) )

(de _leqargs (Lst)
   (if (atom (car Lst))
      (cons (car Lst) (_leqargs (cdr Lst)))
      Lst ) )

# evq* - alternate notation for nested quasiquotes. all the following are the
# same (pretty sure)
#
#     ~(leq [...]
#         `(...
#            ~(leq
#               ~(leq* ,,[...]
#                  ... ) ) ) )
#
#         vs
#
#     ~(leq [...]
#        `( ...
#           ~(leq
#              ~(evq*
#                  `(let ,,[...]
#                     ... ) ) ) ) )
#
#         vs
#
#      (let
#         (evq
#            `( ...
#               (let ,[...]
#                  (evq
#                     '(let ,,[...]
#                        ... ) ) ) ) ) )


(de eval-quasiquote-readmacro Lst
   (macro '((evq "," ^ Lst))) )


# shorthand for quasiquote functions
#
# if (car Lst) is a list, supply your own args
#
#     ~(qfn (X Y) `(,(car X) ,(car Y)))
#
#  else use Qsyms ((car Lst) is assumed to be "`")
#
#     ~(qfn `(,(car Q1) ,(car Q2)))
#
(de qfn Lst
   (if (= (car Lst) "`")
      (macro (q ^ Lst))
      (let [(Args . Body) Lst]
         (macro
            (list (lit '((^ Args) (quasi ^ Body)))) ) ) ) )

# same as above, with anaphoric Qsyms
#
# ~(q `(,(car Q1) ,(cadr Q1)))
#
(de q Lst
   (macro
      (list
         (lit '((^(Qsyms Lst)) (quasi ^ Lst))) ) ) )

# Q1 ... QN
(de Qsyms (Lst)
   (by
      '((X) (stem (chop X) 'Q))
      sort
      (filter '((X) (and (pre? 'Q (pack X)) (> (length @) 1)))
         (uniq
            (flat Lst) ) ) ) )


### abbrevs
(def 'quasi quasiquote)
(def 'qq    quasi)
(def 'leq   let-eval-quasiquote-readmacro)
(def 'leq*  let-eval-quasiquote-star-readmacro)
(def 'evq   eval-quasiquote)
(def 'evq*  eval-quasiquote-readmacro)
