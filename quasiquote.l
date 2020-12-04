### quasiquote.l - common lisp style macros

#  : (let X 2 (quasiquote "`"(* ","X ",@"(3 4 5))))
#
#  -> (* 2 3 4 5)

#  regarding (nested) backquotes - each 'quasiquote' call "removes" only one
#  (level of) backquote (usually car of Lst), while processing all "one level"
#  of 'unquote' and 'unqoute-splice's in Lst.

#  : (let X 2
#        (quasiquote
#           "`"(* ","X                                # `(* ,X `(+ ,,X ,@(3 4 5)))
#                 "`"(+ ","","X ",@"(3 4 5)) ) ) )
#
#  -> (* 2 "`"(+ ","X 3 4 5))

#  it is what it is, i guess. seems easy enough to reason about and still allows
#  for nested backquote macros that work as expected (though the exact syntax
#  differs from common lisp). See 'defunits.m.l' and 'mac.m.l' for examples of
#  nested backquote macros.

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
                     # backquote
                     ("`"  (if BQ                                              # if encountered already
                              (recurse (cdr  X) (cons (car X) Acc))            # do nothing
                              (on BQ)                                          # else mark encountered
                              (recurse (cdr  X) (cons (lit 'quote) Acc)) ) )   # and 'quote' the list
                     # unquote
                     (","  (recurse (cddr X) (cons (list 'list (cadr X)) '^ Acc)) )
                     # unquote-splice
                     (",@" (recurse (cddr X) (cons (cadr X) '^ Acc)) )
                     (T    (recurse (cdr  X) (cons (car X) Acc))) ) ) ) ) ) ) )

# i'm continually amazed that such a simple system allows to almost directly
# translate the most complex common lisp macros (i've found) to picolisp. i'm
# grateful for having discovered it


# remove one level of nesting
#
#  : (deflate '('quote (blah (blah)))
#  -> '(blah (blah))

(de deflate (Lst)
   (make
      (for L Lst
         (if (pair L) (chain @) (link L)) ) ) )


### read-macros (for m.l files)
#
# a spoonful of sugar helps the medicine go down

# qq - 'quasiquote' abbreviation
#
# ~(qq ...)
#
#  expands to
#
#  (quasiquote ...)

(de qq Lst
   (macro
      '((quasiquote ^ Lst))) )


# evq - 'eval quasiquote' abbrev
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
(def 'leq   let-eval-quasiquote)
(def 'evq   eval-quasiquote)
