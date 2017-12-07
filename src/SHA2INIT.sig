
signature SHA2INIT =
sig
  structure Word : WORD
  (**
   * H[0]0..H[0]7
   *)
  val H0 : Word.word Sha2Type.t
end

