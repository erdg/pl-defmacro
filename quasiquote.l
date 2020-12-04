### quasiquote.l - common lisp style macros

#  : (let X 2 (quasiquote "`"(* ","X ",@"(3 4 5))))
#
#  -> (* 2 3 4 5)

#  regarding (nested) backquotes - each 'quasiquote' call "removes" only one
#  (level of) backquote (usually car of Lst), while processing all 'unquote'
#  and 'unqoute-splice's in Lst.

#  : (let X 2
#        (quasiquote
#           "`"(* ","X
#                 "`"(+ ","X ",@"(3 4 5)) ) ) )
#
#  -> (* 2 "`"(+ 2 3 4 5))

#  it is what it is, guess. seems easy enough to reason about and still allows
#  for nested backquote macros that work as expected (though the exact syntax
#  differs from common lisp). See 'defunits.m.l' and 'mac.m.l' for examples of
#  nested backquote macros.

#  a mini-lecture-rant on nested backquotes:

#     nested backquotes seem unnecessarily shrouded in mystery in common lisp
#     lore. as far as i can tell, they simply allow for this kind
#     of pattern:

#        you're writing a macro (the parent) that writes a macro (the child)
#        and you need to refer to the (future) run-time arguments of the child
#        macro right now (in the definition of the parent).

#     look at 'defunits.m.l'. 'defunits' is a macro used to define
#     relationships between similar types of quantities (e.g. quantities of
#     time - seconds, minutes, days, milliseconds, etc.) it does this by
#     writing another macro (named 'unit-of-[quantity]') that captures the
#     user-defined relationships as basic multiplication (using a 'case'
#     statement). that's a fancy way of saying there are 60 seconds in a minute
#     and in order to find out how many seconds are in 17 minutes we need to
#     multiply (* 17 60).

#     but right now we're righting a macro that defines another macro which
#     defines (simple multiplication) relationships between as-of-now unknown
#     quantities (e.g.  time, distance, mass, blemflarks, whatever). but we
#     know that whatever they are, we want to multiply them to get to some
#     number of some base unit. so we use nested backquotes (and "stacked"
#     unquotes) to name these abstract somethings and write code with them now.

#     the other included example of nested backquotes is 'mac!'. 'mac!' is a
#     macro that writes macros that evaluate certain run-time arguments (those
#     prefixed with a bang '!') one time only (see 'evolution-of-mac-bang.l'
#     for a more detailed explanation).

#     "when your macro-writing (parent) macro has run-time arguments (from the
#     child) to use, nested backquotes will do!"

#  and finally, the macro you've all been waiting for

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
                     ("`"  (if BQ                                              # if backquote encountered already
                              (recurse (cdr  X) (cons (car X) Acc))            # do nothing
                              (on BQ)                                          # else mark encountered
                              (recurse (cdr  X) (cons (lit 'quote) Acc)) ) )   # and 'quote' the list
                     (","  (recurse (cddr X) (cons (list 'list (cadr X)) '^ Acc)) )  # unquote
                     (",@" (recurse (cddr X) (cons (cadr X) '^ Acc)) )               # unquote-splice
                     (T    (recurse (cdr  X) (cons (car X) Acc))) ) ) ) ) ) ) )


### read-macros (for m.l files)

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
