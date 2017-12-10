
functor MkSha2 (S : SHA2CORE) :> SHA2 =
struct
local
  structure R = Reader
in
  open S

  fun ofSubstring str =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) Substring.getc in
      (#1 o valOf) (scan getWord8 str)
    end

  fun ofString str =
    ofSubstring (Substring.full str)

  fun ofVectorSlice slice =
    (#1 o valOf) (scan VectorSlice.getItem slice)

  fun ofVector vector =
    ofVectorSlice (VectorSlice.full vector)

  fun ofBinStream strm =
    (#1 o valOf) (scan BinIO.StreamIO.input1 strm)

  fun ofTextStream strm =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) TextIO.StreamIO.input1 in
      (#1 o valOf) (scan getWord8 strm)
    end
end (* local *)
end

