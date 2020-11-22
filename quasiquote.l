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


### read-macros for m.l files

# ~(leq [X 2 Y 3]
#    `(* ,X ,Y) )
#
(de let-eval-quasiquote-readmacro Lst
   (let [(Args . Body) (leqargs Lst)]
      (macro
         '((let ^ Args (eval-quasiquote ^ Body ))) ) ) )

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


# leq* is a more concise version of the following 'evq*' example
#
# ~(leq* ,,[letargs (list ,@Xs) (list ,@Ys)]
#     ... )

(de let-eval-quasiquote-star-readmacro Lst
   (let [(Args . Body) (leqargs Lst)]
      (macro
         '(",""`"(let ^ Args (eval-quasiquote ^ Body ))) ) ) )

# evq* - alternate notation for nested quasiquotes
#
# ~(evq*
#     `(let ,,[...]) )
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
