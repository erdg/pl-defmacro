(mac defunits (Quantity Base . Units)
   (mac ,(symb 'unit-of- Quantity) ("Val" "Unit")
      (* ,,"Val"
         (case ,,"Unit"
            (,Base 1)
            ,@(mapcar
                  '((X) (quasi `(,(car X) ,(cadr X))))
                  (groups-of 2 Units) ) ) ) ) )
