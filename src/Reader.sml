
structure Reader =
struct
  type ('a,'b) t = ('a,'b) StringCvt.reader

  type ('a,'b) reader = ('a, 'b) t

  fun bind  NONE    _ = NONE
    | bind (SOME x) f = f x

  fun fmap f get strm =
    case get strm
      of SOME(x,strm) => SOME(f x,strm)
       | NONE         => NONE

  fun seqN get n =
    if n = 0 then (fn ss => SOME([], ss))
    else
      fn ss =>
        bind (get ss)            (fn (x ,ss) =>
        bind (seqN get (n-1) ss) (fn (xs,ss) =>
        SOME(x::xs,ss)))

  fun read_all get =
    let
      fun go ss =
        case get ss
          of SOME(x,ss) =>
              (case go ss
                 of (xs,ss) => (x::xs,ss))
           | NONE => ([],ss)
    in go
    end

end

