
functor MkBlock (W : WORD) =
struct
  (**
   * 16 32-bit words => 512bit block or
   * 16 64-bit words => 1024bit block
   *)
  datatype t = Block of W.word vector

  val lengthBit = W.wordSize * 16

  exception WrongLength of W.word vector

  fun toVector (Block vec) = vec

  fun toString (Block vec) =
    let fun tos w = StringCvt.padLeft #"0" (W.wordSize div 4) (W.toString w) in
      Vector.foldl (fn(w,s)=> s ^ tos w ^ "\n") "" vec
    end

  fun fromVector vec =
    if Vector.length vec = 16 then
      Block vec
    else
      raise WrongLength vec

  fun sub (Block vec, n) = Vector.sub(vec, n)

  fun scan (get: (W.word,'a) Reader.t) : (t,'a) Reader.t =
    fn ss =>
      Option.map
          (fn(xs,ss)=> (Block (vector xs),ss))
          (Reader.seqN get 16 ss)
end

