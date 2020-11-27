(load "quasiquote.l" "utils.l")

(de defmacro Lst
   (let [(@Nm @Args . Body) Lst]
      (macro
         (de @Nm Lst
            (let [@Args Lst]
               (eval
                  (quasiquote ^ Body) ) ) ) ) ) )
