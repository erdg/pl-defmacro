(load "quasiquote.l" "utils.l")

# mload - only loads one top level form per file, so the convention is to wrap
# everything with the 'm' "macro".
#
#  (m
#     ...
#  )
#
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

# macro compiler

(de mc (M)
   (_mc (leaves M (= @ 'quasiquote) '_quasiquote) ) )

(de _mc (X)
   (recur (X Acc)
      (ifn X
         (flip Acc)
         (if (pair (car X))
            (if (= (caar X) '_quasiquote)
               # eval '_quasiquote' forms
               (recurse (cddr X) (cons (eval (car X) Acc)))
               (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
            (recurse (cdr  X) (cons (car X) Acc))) ) ) )


# (pipe (print (mc1 nif$)) (make (while (read "_-=!$<>") (link @))))
# (replace @ "'" 'macro)
# (glue " " @)
# (any @)

(de mcompile (M)
   (use Tmp
      (setq Tmp (mc M))
      (while (fish '((L) (= (car L) '_quasiquote)) Tmp)
         (setq Tmp (mc Tmp)) )
      (let Raw (pipe
                  (print Tmp)
                  (make
                     (while (read "_-=!?<>$*")
                        (link @) ) ) )
         (any (glue " " (replace Raw "'" 'macro))) ) ) )



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
