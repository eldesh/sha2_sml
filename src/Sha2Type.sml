
structure Sha2Type :> SHA2TYPE =
struct
  structure Word = Word32

  datatype t = Hash of Word.word * Word.word * Word.word * Word.word
                     * Word.word * Word.word * Word.word * Word.word
end

