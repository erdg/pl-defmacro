(load "utils.l")

(de defmacro Lst
   (let [(@Nm @Args . Body) Lst]
      (macro
         (de @Nm Lst
            (let [@Args Lst]
               (eval
                  (cl-backquote-form ^ Body) ) ) ) ) ) )

(de cl-backquote-form Lst
   (macro
      (macro
         (^(macro (_walk ^(deflate Lst)))) ) ) )

(de _walk X
   (recur (X Acc BQ) # backquote encountered?
      (ifn X
         (flip Acc)
         (if (pair (car X))
            (if (atom (cdr @))
               (recurse (cdr X) (cons (car X) Acc))
               (recurse (cdr X) (cons (recurse (car X) NIL) Acc)) )
            (case (car X)
               ("`"  (recurse (cdr  X) (cons (lit 'quote) Acc)) )
               # experimental stuff to try to get nested backquotes to "work"
               # ("`"  (ifn BQ # backwards because recursion
               #          (recurse (cdr X) (cons (car X) Acc)) # ignore it
               #          (on BQ)
               #          (recurse (cdr  X) (cons (lit 'quote) Acc)) ) )
               (","  (recurse (cddr X) (cons (list 'list (cadr X)) '^ Acc)) )
               (",@" (recurse (cddr X) (cons (cadr X) '^ Acc)) )
               (T    (recurse (cdr  X) (cons (car X) Acc))) ) ) ) ) )
