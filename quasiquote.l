### quasiquote

(de quasiquote Lst
   (macro
      (macro
         (^(deflate (macro (_quasiquote ^ Lst)))) ) ) )

(de _quasiquote X
   (recur (X Acc)
      (ifn X
         (flip Acc)
         (if (pair (car X))
            (if (atom (cdr @))
               (recurse (cdr X) (cons (car X) Acc))
               (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
            (case (car X)
               ("`"  (recurse (cdr  X) (cons (lit 'quote) Acc)) )
               (","  (recurse (cddr X) (cons (list 'list (cadr X)) '^ Acc)) )
               (",@" (recurse (cddr X) (cons (cadr X) '^ Acc)) )
               (T    (recurse (cdr  X) (cons (car X) Acc))) ) ) ) ) )

(de eval-quasiquote Lst
   (macro
      (eval
         (quasiquote ^ Lst) ) ) )


### read-macros

# evqr - alternate notation for nested quasiquotes
#
# ~(evqr
#     `(let ...) )
#
(de eval-quasiquote-readmacro Lst
   (macro '((evq "," ^ Lst))) )


# shorthand for quasiquote functions
#
# ~(qfn (X) `(,(car X) ,(cadr X)))
#
(de qfn Lst
   (let [(Args . Body) Lst]
      (macro
         (list (lit '((^ Args) (quasi ^ Body)))) ) ) )

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
(def 'evq   eval-quasiquote)
(def 'evqr  eval-quasiquote-readmacro)
