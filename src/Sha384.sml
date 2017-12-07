
structure Sha384 =
struct
local
  structure T = Sha2Type64
  structure C = Sha384And512Core(Sha384Init)

  datatype h = Hash of T.Word.word * T.Word.word * T.Word.word * T.Word.word
                     * T.Word.word * T.Word.word * T.Word.word

  fun fromEntity (T.Hash(h0,h1,h2,h3,h4,h5,h6,h7)) =
    Hash(h0,h1,h2,h3,h4,h5,h6)

  structure H = MkSha2(
  struct
    open C
    type t = h
    fun scan getw = Reader.fmap fromEntity (C.scan getw)
  end)
in
  open H
end
end

