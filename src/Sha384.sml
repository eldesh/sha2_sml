
structure Sha384 :> SHA2 =
struct
local
  structure C = MkSha2Core(
                    structure I = Sha384Init
                    structure F = Sha384And512Func)

  datatype 'a h = Hash of 'a * 'a * 'a * 'a
                        * 'a * 'a

  fun fromEntity (Sha2Type.Hash(h0,h1,h2,h3,h4,h5,_,_)) =
    Hash(h0,h1,h2,h3,h4,h5)

  structure H = MkSha2(
  struct
    structure Word = C.Word
    type 'a t = 'a h

    fun toString (Hash(h0,h1,h2,h3,h4,h5) : Word.word t) =
      let fun tos w = StringCvt.padLeft #"0" (2 * 8) (Word.toString w)
      in concat (map tos [h0,h1,h2,h3,h4,h5]) end

    fun toVector (Hash(w0,w1,w2,w3,w4,w5)) =
      vector [w0,w1,w2,w3,w4,w5]

    structure SS = Substring
    fun fromHexString str =
      let
        val ss = SS.full str
        fun get ss =
          let
            val w = SOME(SS.splitAt (ss, 16)) handle Subscript => NONE
          in
            Option.mapPartial
              (fn(w,ws)=> Option.map
                            (fn w=> (w,ws))
                            (Word.fromString (SS.string w)))
              w
          end
      in
        case Reader.seqN get 6 ss
          of SOME([h0,h1,h2,h3,h4,h5],ss) =>
               if SS.isEmpty ss
               then SOME(Hash(h0,h1,h2,h3,h4,h5))
               else NONE
           | NONE =>
               NONE
      end

    fun hashStream getw = Reader.fmap fromEntity (C.hashStream getw)
    fun hashStream' getw = (#1 o valOf) o (hashStream getw)
  end)
in
  open H
end
end

