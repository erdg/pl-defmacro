(load "quasiquote.l" "utils.l")

# mload - only loads one top level form per file, so the convention is to wrap
# everything with the 'm' "macro".
#
#  (m
#     ...
#  )

(de mload (File)
   (mapc eval
      (cdr
         (transform
            (mread File) ) ) ) )

(def 'MREADSTRING "_-=!?<>$*")
# read a macro file ("File.m.l" by convention)
(de mread (File)
   (in File
      (make
         (while (read MREADSTRING)
            (link @) ) ) ) )

(de transform (Lst)
   (any
      (glue " "
         (_transform Lst) ) ) )

# 'sym' all backticks '`', unquotes ',' and unquote-splices ',@' so they appear
# as transient symbols when the code is 'eval'ed by 'mload'
(de _transform (X)
   (recur (X Acc)
      (ifn X
         (flip Acc)
         (case (car X)
            ("`"  (recurse (cdr  X) (cons (sym "`") Acc)) )
            (","  (if (= (cadr X) "@")
                     (recurse (cddr X) (cons (sym ",@") Acc))
                     (recurse (cdr X) (cons (sym ",") Acc)) ) )
            (T    (recurse (cdr  X) (cons (car X) Acc))) ) ) ) )

# working with mfiles
(de mvi (Name)
   (let File (pack Name ".m.l")
      (call 'vim File)
      (mload File) ) )

(de mload-all ()
   (for M (mfiles (dir))
      (mload M) ) )

(de mfiles (Lst)
   (filter
      '((X)
         (= "ml"
            (pack (tail 2 (split (chop X) ".")))) )
      Lst ) )


### mc - the macro compiler (proof of concept)
#
# the idea is...
#
#  # to pipe the macro definition to low-level 'read'
#
#  (eval
#     (quasiquote
#        "`"(* ","X ","X) ) )
#
#  # transform "eval quasiquote" forms into "macro _quasiquote" forms with a
#  # backquote readmacro
#
#  (macro
#     `(_quasiquote
#        "`"(* ","X ","X) ) )
#
#  # bring it back to lisp so the backquote readmacro fires
#
#  (macro
#     '(* ^ (list X) ^ (list X)) )
#
#  # pipe *that* back to low-level 'read' to remove the quotes from the 'macro'
#  # lists and finally bring it back to lisp where it can run like normal
#
#  (macro
#     (* ^ (list X) ^ (list X)) )
#
#
# compiled macros tend to be an order of magnitude faster, e.g.
#
#  : (defunits time s  m 60 h (60 m) d (24 h))
#  -> unit-of-time
#
#  : (def 'unit-of-time-compiled (mc unit-of-time))
#  -> unit-of-time-compiled
#
#  : (bench (do 1000000 (unit-of-time 28 'd)))
#  -> approx. 10 seconds
#
#  : (bench (do 1000000 (unit-of-time-compiled 28 'd)))
#  -> approx. 1 second

(de mc (M)
   (and M
      # "raw" mode
      (pipeMacro @)
      (replace @ 'eval 'macro)
      # 'quasiquote -> _quasiquote with '`' read-macro
      (_mc @)
      (remove-parens-before-backquotes @)
      # prep transient symbols
      (mapcar '((X) (if (pair X) (sym (pack @)) X)) @)
      # back to lisp
      (any (glue " " @))
      # back to "raw" mode
      (pipeMacro @)
      # remove "'"s following 'macro'
      (remove-quotes-after-macros @)
      # prep transient symbols again
      (mapcar '((X) (if (pair X) (sym (pack @)) X)) @)
      # back to lisp
      (any (glue " " @)) ) )

(de pipeMacro (M)
   (pipe
      (println M)
      (make
         (while (read MREADSTRING)
            (link @) ) ) ) )

(de _mc (Ls)
   (make
      (for L Ls
         (if (= L 'quasiquote)
            (link "`" "(" '_quasiquote)
            (link L) ) ) ) )

(de remove-parens-before-backquotes (Lst)
   (use L
      (setq L Lst)
      (let Bs (indexes "`" Lst)
         (for (I . B) Bs
            (setq L (remove (- B 1 (- I 1)) L)) ) ) ) )

(de remove-quotes-after-macros (Lst)
   (use L
      (setq L Lst)
      (let Qs (mapcar inc (indexes 'macro Lst))
         (for (I . Q) Qs
            (setq L (remove (- Q (- I 1)) L)) ) ) ) )
