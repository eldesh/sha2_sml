
signature SHA2FUNC =
sig
  structure Word : WORD

  val CH    : Word.word * Word.word * Word.word -> Word.word
  val MAJ   : Word.word * Word.word * Word.word -> Word.word
  val BSIG0 : Word.word -> Word.word
  val BSIG1 : Word.word -> Word.word
  val SSIG0 : Word.word -> Word.word
  val SSIG1 : Word.word -> Word.word
  val K     : Word.word vector
end

