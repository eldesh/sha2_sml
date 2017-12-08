
signature SHA2CORE =
sig
  structure Word : WORD
  eqtype 'a t

  (**
   * stringify value of 'a t
   *)
  val toString : Word.word t -> string

  (**
   * Read a value of type 'a t from a word stream
   *)
  val fromString : string -> Word.word t option

  (**
   * Hash a word stream then return it with the rest of the stream
   *)
  val hashStream : (Word8.word, 'a) Reader.t -> (Word.word t, 'a) Reader.t

  (**
   * Hash a word stream then return it and discard remaining stream.
   * The input stream is expected to constructed from word.
   *)
  val hashStream' : (Word8.word, 'a) Reader.t -> 'a -> Word.word t
end

