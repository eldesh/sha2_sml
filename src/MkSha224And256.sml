
functor MkSha224And256 (S : SHA224AND256CORE) =
struct
local
  structure R = Reader
in
  open S

  fun hash_substring str =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) Substring.getc in
      (#1 o valOf) (scan getWord8 str)
    end

  fun hash_string str =
    hash_substring (Substring.full str)

  fun hash_vector_slice slice =
    (#1 o valOf) (scan VectorSlice.getItem slice)

  fun hash_vector vector =
    hash_vector_slice (VectorSlice.full vector)

  fun hash_bin_stream strm =
    (#1 o valOf) (scan BinIO.StreamIO.input1 strm)

  fun hash_text_stream strm =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) TextIO.StreamIO.input1 in
      (#1 o valOf) (scan getWord8 strm)
    end
end (* local *)
end

