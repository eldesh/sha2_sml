
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

  fun bind  NONE    _ = NONE
    | bind (SOME x) f = f x

  type ('a,'b) reader = ('a, 'b) StringCvt.reader

  fun seqN (get:('a,'b) reader) n : ('a list, 'b) reader =
    if n = 0 then (fn ss => SOME([], ss))
    else
      fn ss =>
        bind (get ss)            (fn (x ,ss) =>
        bind (seqN get (n-1) ss) (fn (xs,ss) =>
        SOME(x::xs,ss)))

  fun scan (get: (Word32.word,'a) StringCvt.reader) : (t,'a) StringCvt.reader =
    fn ss =>
      Option.map
          (fn(xs,ss)=> (Block (vector xs),ss))
          (seqN get 16 ss)
end

