
structure CAVPParser =
struct
local
  structure IO = TextIO
  open Parser
  fun K x _ = x
  (* --> workaround for sml/nj *)
  infixr 6 <&>
  infixr 6 <&
  infixr 6 &>
  infix  5 <&=>
  infix  5 <?@
  infix  5 <@
  infixr 4 <|>
  infix    <:&>
  (* <-- workaround for sml/nj *)

in
  fun token tok ss =
    let
      val xs = List.take (ss, size tok)
    in
      if implode xs = tok
      then SOME((), List.drop (ss, size tok))
      else NONE
    end
    handle Subscript => NONE

  val digit       = satisfy Char.isDigit
  val digit1_9    = satisfy (fn c=> c <> #"0" andalso Char.isDigit c)
  val space       = satisfy Char.isSpace
  val spaces      = <+> space

  val number : (int, char) t =
        (symbol #"0" <@ K (valOf (Int.fromString "0")))
    <|> (digit1_9 <&> <*> digit <@ (valOf o Int.fromString o implode o op::))

  val linebreak = token "\r\n"

  val hex = satisfy (Char.contains "0123456789abcdefABCDEF")
  val hexstr = <+> hex <@ implode

  val comment = token "#"
                <& (<*> (satisfy (fn c => not (Char.contains "\r\n" c))))
                <& linebreak

  val empty_line = linebreak

  fun alt p q = fn ss =>
    case p ss
      of NONE   => q ss
       | SOME r => SOME r

  val br = linebreak
  val sps = spaces

  val Seed = token "Seed" <@ K "Seed"
  val COUNT = token "COUNT" <@ K "COUNT"
  val MD = token "MD" <@ K "MD"
  val Len = token "Len" <@ K "Len"
  val Msg = token "Msg" <@ K "Msg"

  val header =
    pack (<*> (alt comment empty_line))
         (token "[L = " &> number <& token "]")
         (<*> (alt comment empty_line))

  fun assert msg false = raise Fail msg
    | assert _   _     = ()

  infix ==>
  fun p ==> q = (not q) orelse (p andalso q)

  fun test_case L =
    ((Len <& sps <& (token "=") <& sps) &> number <& br) <&=> (fn len =>
    ((Msg <& sps <& (token "=") <& sps) &> hexstr <& br) <&=> (fn msg =>
    ((MD  <& sps <& (token "=") <& sps) &> hexstr <& br) <&=> (fn exp =>
    (* check invariant *)
    (assert "len -> msg = 0" (len = 0 ==> msg = "00");
     assert "length msg = len" (len <> 0 ==> size msg = len div 4);
     assert "length MD = L" (L = size exp div 2);
     succeed (if len = 0 then "" else msg, exp)
    ))))

  fun parser () =
    header <&=> (fn L =>
    <*> (test_case L <& empty_line))

  fun parse path =
    let
      val file = IO.openIn path
      val contents = IO.inputAll file
    in
      parser () (explode contents)
    end

end (* local *)
end

