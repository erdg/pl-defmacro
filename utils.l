### utils

(de flat  (L) (fish atom L))

(de symb @
   (any (apply pack (rest))) )

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


