
signature SHA2CORE =
sig
  structure Word : WORD
  type 'a t

  val toString : Word.word t -> string
  val scan : (Word8.word, 'a) Reader.t -> (Word.word t, 'a) Reader.t
end

