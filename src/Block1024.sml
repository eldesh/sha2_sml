
structure Block1024 =
struct
  (* 16 64-bit words => 1024bit block *)
  datatype t = Block of Word64.word vector

  exception WrongLength of Word64.word vector

  fun toVector (Block vec) = vec

  fun toString (Block vec) =
    let fun tos w = StringCvt.padLeft #"0" 16 (Word64.toString w) in
      Vector.foldl (fn(w,s)=> s ^ tos w ^ "\n") "" vec
    end

  fun fromVector vec =
    if Vector.length vec = 16 then
      Block vec
    else
      raise WrongLength vec

  fun sub (Block vec, n) = Vector.sub(vec, n)

  fun scan (get: (Word64.word,'a) Reader.t) : (t,'a) Reader.t =
    fn ss =>
      Option.map
          (fn(xs,ss)=> (Block (vector xs),ss))
          (Reader.seqN get 16 ss)
end

