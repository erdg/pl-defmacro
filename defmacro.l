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
         (^(macro (_walk ^(_deflate Lst)))) ) ) )

# remove one level of nesting, eg. (1 2 (3 4 (5 6))) -> (1 2 3 4 (5 6))
(de _deflate (Lst)
   (make
      (for L Lst
         (if (pair L) (chain @) (link L)) ) ) )

(de _walk X
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
