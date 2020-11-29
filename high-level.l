# this is a high-level overview of the minimum code needed to write
# "quasiquote-style macros" in native picolisp, i.e. with '`', ',' and ',@'
# behaving as expected (and not having to be transient symbols).

# m.l - the macro (".m.l") file loader

(de mload (File)
   (mapc eval
      (cdr
         (transform
            (mread File) ) ) ) )

(def 'MREADSTRING "_-=!?<>$*")

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


# quasiquote.l - the quasiquote implementation

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


# mac.m.l - define new macros with 'mac'

(m

   (de mac Lst
      (let [(Name Args . Body) Lst]
         (eval
            (quasiquote
               `(de ,Name Lst
                  (let [,Args Lst]
                     (eval
                        (quasiquote
                           ,@Body ) ) ) ) ) ) ) )

)

# the rest of the code in this repo expands upon this idea to add some syntax
# sugar, a toy macro "compiler", a couple neat variations of 'mac' and a few
# examples of more complicated macros.


# the adventure continues with 'explanation.l'
