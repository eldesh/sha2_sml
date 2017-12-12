
structure Parser =
struct
local
  infixr 6 <&>
  infixr 6 <&
  infixr 6 &>
  infix  5 <&=>
  infix  5 <?@
  infix  5 <@
  infixr 4 <|>
  infix    <:&>
in
  type ('r, 'a) t = ('r, 'a list) StringCvt.reader

  fun bind  NONE    _ = NONE
    | bind (SOME x) f = f x

  fun symbol sym ss =
    case ss
      of [] => NONE
       | x::xs => if x = sym
                  then SOME(x, xs)
                  else NONE

  fun satisfy2 pred f ss =
    case ss
      of [] => NONE
       | x::xs => if pred x
                  then SOME(f x, xs)
                  else NONE

  fun satisfy pred =
    satisfy2 pred (fn x=>x)

  fun alt p q = fn ss =>
    case p ss
      of NONE   => q ss
       | SOME r => SOME r

  fun p <|> q = alt p q

  fun seq p q = fn ss =>
    bind (p ss ) (fn (pr,ss') =>
    bind (q ss') (fn (qr,ss'') =>
    SOME((pr,qr),ss'')))

  fun p <@ f = fn ss =>
    bind (p ss) (fn (r,ss') =>
    SOME(f r,ss'))

  fun p <&> q = seq p q

  fun p <& q =
    seq p q <@ #1

  fun p &> q =
    seq p q <@ #2

  fun p <:&> q =
    p <&> q <@ op::

  fun p <&=> q = fn ss =>
    bind (p ss) (fn (r,ss') =>
    q r ss')

  fun p <?@ (no,yes) = fn ss =>
    bind (p ss) (fn (r,ss') =>
    Option.getOpt (yes r, no))

  fun <?> p ss =
    case p ss
      of NONE        => SOME(NONE, ss)
       | SOME(r,ss') => SOME(SOME(r), ss')

  fun <*> p ss =
    case p ss
      of NONE        => SOME([], ss)
       | SOME(r,ss') =>
           case <*> p ss'
             of NONE          => SOME([r]  , ss')
              | SOME(rs,ss'') => SOME(r::rs, ss'')

  fun <+> p =
    p <&> <*> p <@ op::

  fun succeed r ss = SOME(r, ss)

  fun choice (ps: ('r, ''a) t list) : ('r option, ''a) t =
    foldl
      op<|>
      (succeed NONE)
      (map (fn p=> p <@ SOME) ps)

  fun choice' ps ss =
    case choice ps ss
      of NONE => raise Fail "choice' NONE"
       | SOME(NONE  , ss') => NONE
       | SOME(SOME r, ss') => SOME(r, ss')

  fun pack open_ p close_ = open_ &> p <& close_
  fun parenthesis p = pack (symbol #"(") p (symbol #")")
  fun brakets     p = pack (symbol #"[") p (symbol #"]")
  fun braces      p = pack (symbol #"{") p (symbol #"}")

  exception ParseError of string

  fun just p ss =
    case p ss
      of NONE => raise ParseError "just: parse error"
       | SOME(r,ss) =>
           if null ss
           then r
           else raise ParseError "just: input remains"
end (* local *)
end

