
signature SHA2 =
sig
  include SHA2CORE
  val ofSubstring   : Substring.substring -> t
  val ofString      : String.string -> t
  val ofVectorSlice : Word8.word VectorSlice.slice -> t
  val ofVector      : Word8.word Vector.vector -> t
  val ofBinStream   : BinIO.StreamIO.instream -> t
  val ofTextStream  : TextIO.StreamIO.instream -> t
end

