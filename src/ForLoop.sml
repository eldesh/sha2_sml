
structure ForLoop =
struct
  fun for i cond step body =
    let
      fun for' i =
        if cond i then 
          (body i;
           for' (step i))
        else
          ()
    in
      for' i
    end

  fun for' i cond =
    for i cond (fn i=>i+1)
end

