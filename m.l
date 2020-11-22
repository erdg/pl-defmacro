(load "quasiquote.l" "utils.l")

# mload - only loads one top level form per file, so the convention is to wrap
# everything with the 'm' "macro". (NOTE 'm' is not actually defined, don't use
# it in regular '.l' files)
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

# read macro file ("File.m.l" by convention)
(de mread (File)
   (in File
      (make
         (while (read "_-!?<>$")
            (link @) ) ) ) )

(de transform (Lst)
   (any
      (glue " "
         (_transform Lst) ) ) )

(de _transform (X)
   (recur (X Acc)
      (ifn X
         (flip Acc)
         (if (pair (car X))
            (if (atom (cdr @))
               (recurse (cdr X) (cons (car X) Acc))
               (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
            (case (car X)
               ("`"  (recurse (cdr  X) (cons (sym "`") Acc)) )
               (","  (if (= (cadr X) "@")
                        (recurse (cddr X) (cons (sym ",@") Acc))
                        (recurse (cdr X) (cons (sym ",") Acc)) ) )
               (T    (recurse (cdr  X) (cons (car X) Acc))) ) ) ) ) )


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
