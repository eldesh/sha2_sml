(**
 * 6.3.  SHA-384 and SHA-512 Initialization
 *
 * For SHA-512, the initial hash value, H(0), consists of the following
 * eight 64-bit words, in hex.  These words were obtained by taking the
 * first 64 bits of the fractional parts of the square roots of the
 * first eight prime numbers.
 *)
structure Sha512Init :> SHA2INIT
                        where type Word.word = Word64.word =
struct
  structure Word = Word64

  val H0 : Word.word Sha2Type.t =
    Sha2Type.Hash
      ( 0wx6a09e667f3bcc908
      , 0wxbb67ae8584caa73b
      , 0wx3c6ef372fe94f82b
      , 0wxa54ff53a5f1d36f1
      , 0wx510e527fade682d1
      , 0wx9b05688c2b3e6c1f
      , 0wx1f83d9abfb41bd6b
      , 0wx5be0cd19137e2179
      )
end

