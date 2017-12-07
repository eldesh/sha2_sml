(**
 * 6.3.  SHA-384 and SHA-512 Initialization
 *
 * For SHA-384, the initial hash value, H(0), consists of the following
 * eight 64-bit words, in hex.  These words were obtained by taking the
 * first 64 bits of the fractional parts of the square roots of the
 * ninth through sixteenth prime numbers.
 *)
structure Sha384Init =
struct
  structure S = Sha2Type64
  open S
  val H0 =
    Hash ( 0wxcbbb9d5dc1059ed8
         , 0wx629a292a367cd507
         , 0wx9159015a3070dd17
         , 0wx152fecd8f70e5939
         , 0wx67332667ffc00b31
         , 0wx8eb44a8768581511
         , 0wxdb0c2e0d64f98fa7
         , 0wx47b5481dbefa4fa4
         )
end

