
signature SHA2TYPE =
sig
  structure Word : WORD where type word = Word32.word

  datatype t = Hash of Word.word * Word.word * Word.word * Word.word
                     * Word.word * Word.word * Word.word * Word.word
end

