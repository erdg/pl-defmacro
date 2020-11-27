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

# evq - 'eval quasiquote' abbreviation
#
# ~(evq ...)
#
#  expands to
#
#  (eval
#     (quasiquote
#        ... ) )

(de eval-quasiquote Lst
   (macro
      '((eval (quasiquote ^ Lst))) ) )

# leq - 'let eval quasiquote' abbreviation
#
# ~(leq [X 2 Y 3]
#    `(* ,X ,Y) )
#
#  expands to
#
#  (let [X 2 Y 3]
#     (eval
#        (quasiquote
#           `(* ,X ,Y) ) ) )

(de let-eval-quasiquote Lst
   (let [(Args . Body) (leqargs Lst)]
      (macro
         '((let ^ Args (eval (quasiquote ^ Body )))) ) ) )

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

# qfn - quasiquote functions
#
#  use anaphoric Qsyms if (car Lst) is a backquote "`"
#
#     ~(qfn `(,(car Q1) ,(car Q2)))
#
#  else supply your own args
#
#     ~(qfn (X Y) `(,(car X) ,(car Y)))

(de qfn Lst
   (if (= (car Lst) "`")
      (macro (_qfn ^ Lst))
      (let [(Args . Body) Lst]
         (macro
            (list (lit '((^ Args) (quasiquote ^ Body)))) ) ) ) )

# same as above, with anaphoric Qsyms
#
# ~(_qfn `(,(car Q1) ,(cadr Q1)))

(de _qfn Lst
   (macro
      (list
         (lit '((^(Qsyms Lst)) (quasiquote ^ Lst))) ) ) )

# Q1 ... QN
(de Qsyms (Lst)
   (by
      '((X) (stem (chop X) 'Q))
      sort
      (filter
         '((X) (and (pre? 'Q (pack X)) (= (length @) 2)))
         (uniq (flat Lst) ) ) ) )


### abbrevs
(def 'quasi quasiquote)
(def 'qq    quasi)
(def 'leq   let-eval-quasiquote-readmacro)
(def 'leq*  let-eval-quasiquote-star-readmacro)
(def 'evq   eval-quasiquote)
(def 'evq*  eval-quasiquote-readmacro)
