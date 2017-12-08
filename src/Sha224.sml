
structure Sha224 :> SHA2 =
struct
local
  structure C = MkSha2Core(
                    structure I = Sha224Init
                    structure F = Sha224And256Func)

  datatype 'a h = Hash of 'a * 'a * 'a * 'a
                        * 'a * 'a * 'a

  fun fromEntity (Sha2Type.Hash(h0,h1,h2,h3,h4,h5,h6,_)) =
    Hash(h0,h1,h2,h3,h4,h5,h6)

  structure H = MkSha2(
  struct
    structure Word = C.Word
    type 'a t = 'a h

    fun toString (Hash(h0,h1,h2,h3,h4,h5,h6) : Word.word t) =
      let fun tos w = StringCvt.padLeft #"0" (2 * 4) (Word.toString w)
      in concat (map tos [h0,h1,h2,h3,h4,h5,h6]) end

    structure SS = Substring
    fun fromString str =
      let
        val ss = SS.full str
        fun get ss =
          let
            val w = SOME(SS.splitAt (ss, 8)) handle Subscript => NONE
          in
            Option.mapPartial
              (fn(w,ws)=> Option.map
                            (fn w=> (w,ws))
                            (Word.fromString (SS.string w)))
              w
          end
      in
        case Reader.seqN get 7 ss
          of SOME([h0,h1,h2,h3,h4,h5,h6],ss) =>
               if SS.isEmpty ss
               then SOME(Hash(h0,h1,h2,h3,h4,h5,h6))
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

