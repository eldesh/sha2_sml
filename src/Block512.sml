
structure Block512 =
struct
  (* 16 32-bit words => 512bit block *)
  datatype t = Block of Word32.word vector

  exception WrongLength of Word32.word vector

  fun toVector (Block vec) = vec

  fun fromVector vec =
    if Vector.length vec = 16 then
      Block vec
    else
      raise WrongLength vec

  fun sub (Block vec, n) = Vector.sub(vec, n)

  fun scan (get: (Word32.word,'a) Reader.t) : (t,'a) Reader.t =
    fn ss =>
      Option.map
          (fn(xs,ss)=> (Block (vector xs),ss))
          (Reader.seqN get 16 ss)
end

