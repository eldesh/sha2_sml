
structure Sha2Type =
struct
  structure Word = Word32
  type word = Word.word

  datatype t = Hash of word * word * word * word
                     * word * word * word * word
end

