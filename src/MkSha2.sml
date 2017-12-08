
functor MkSha2 (S : SHA2CORE) :> SHA2 =
struct
local
  structure R = Reader
in
  open S

  fun hashSubstring str =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) Substring.getc in
      hashStream' getWord8 str
    end

  fun hashString str =
    hashSubstring (Substring.full str)

  fun hashVectorSlice slice =
    hashStream' VectorSlice.getItem slice

  fun hashVector vector =
    hashVectorSlice (VectorSlice.full vector)

  fun hashBinStream strm =
    hashStream' BinIO.StreamIO.input1 strm

  fun hashTextStream strm =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) TextIO.StreamIO.input1 in
      hashStream' getWord8 strm
    end
end (* local *)
end

