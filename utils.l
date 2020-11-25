### utils

(def 'lD ld) # all. the. time.

(de flat  (L) (fish atom L))

(de symb @
   (any (apply pack (rest))) )

(de which (Fn L)
   (index (find Fn L) L) )

# remove one level of nesting, eg. (1 2 (3 4 (5 6))) -> (1 2 3 4 (5 6))
(de deflate (Lst)
   (make
      (for L Lst
         (if (pair L) (chain @) (link L)) ) ) )

(de groups-of (N Src)
   (if (=0 N) (err "zero length"))
   # hack to account for off by 1
   (inc 'N)
   (recur (Src Acc)
      (let Rest (nth Src N)
         (if (pair Rest)
            (recurse Rest (cons (head (- N 1) Src) Acc))
            (flip (cons Src Acc)) ) ) ) )


(de leaves L
   (let ((@Tree @Test @Result) L)
      (macro
         (_leaves
            @Tree
            '((@) @Test)
            '((@) @Result) ) ) ) )

(de _leaves (Tree Test Result)
   (if Tree
      (if (pair Tree)
         (cons
            (_leaves (car Tree) Test Result)
            (_leaves (cdr Tree) Test Result) )
         (if (Test Tree)
            (Result Tree)
            Tree ) ) ) )
