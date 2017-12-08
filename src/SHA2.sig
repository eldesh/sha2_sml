
signature SHA2 =
sig
  include SHA2CORE

  val hashSubstring   : Substring.substring -> Word.word t
  val hashString      : String.string -> Word.word t
  val hashVectorSlice : Word8.word VectorSlice.slice -> Word.word t
  val hashVector      : Word8.word Vector.vector -> Word.word t
  val hashBinStream   : BinIO.StreamIO.instream -> Word.word t
  val hashTextStream  : TextIO.StreamIO.instream -> Word.word t
end

