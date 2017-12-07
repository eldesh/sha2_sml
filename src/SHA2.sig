
signature SHA2 =
sig
  include SHA2CORE

  val ofSubstring   : Substring.substring -> Word.word t
  val ofString      : String.string -> Word.word t
  val ofVectorSlice : Word8.word VectorSlice.slice -> Word.word t
  val ofVector      : Word8.word Vector.vector -> Word.word t
  val ofBinStream   : BinIO.StreamIO.instream -> Word.word t
  val ofTextStream  : TextIO.StreamIO.instream -> Word.word t
end

