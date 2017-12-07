
signature SHA2TYPE =
sig
  structure Word : WORD

  datatype t = Hash of Word.word * Word.word * Word.word * Word.word
                     * Word.word * Word.word * Word.word * Word.word

  val toString : t -> string
end

