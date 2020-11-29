(m

   (def 'lambda quote)

   (de let->job (Lst)
      (make
         (for L (groups 2 Lst)
            (link (cons (car L) (cadr L))) ) ) )

   (mac alet (Args . Body)
      `(lambda @
         (job '[,(cons 'This (cdr (last Body))) ,@(let->job Args)]
            ,@(diff Body (last Body))
            (apply This (rest)) ) ) )

   (mac dlambda "Ds"
      `(lambda @
         (case (next)
            ,@(make
               (for "D" "Ds"
                  (link
                     (list (car "D")
                        (quasiquote
                           `(apply (lambda ,@(cdr "D")) (rest)) ) ) ) ) ) ) ) )

)

# BUG - ',@(diff Body (last Body))' was originally defined as ',@(head -1 Body)'
# but 'mread' seems to parses that as '",@"(head - 1 Body). which is weird because
# "-" is already included in 'MREADSTRING'.
