
structure Sha2Type =
struct
  datatype 'w t = Hash of 'w * 'w * 'w * 'w
                        * 'w * 'w * 'w * 'w

  fun toString wos (Hash(h0,h1,h2,h3,h4,h5,h6,h7)) =
    let fun tos w = StringCvt.padLeft #"0" 16 (wos w) in
      foldl (fn(w,s)=> s ^ tos w ^ " ") "" [h0,h1,h2,h3,h4,h5,h6,h7]
    end
end

