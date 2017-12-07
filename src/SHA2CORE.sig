
signature SHA2CORE =
sig
  structure Word : WORD

  type t
  val scan : (Word8.word, 'a) Reader.t -> (t, 'a) Reader.t
end

