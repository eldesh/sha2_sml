(**
 * 5.  Functions and Constants Used
 * 5.1.  SHA-224 and SHA-256
 *
 * SHA-224 and SHA-256 use six logical functions, where each function
 * operates on 32-bit words, which are represented as x, y, and z.  The
 * result of each function is a new 32-bit word.
 *)
structure Sha224And256Func =
struct
local
  structure Ops = MkSha2Ops(Word32)
  open Ops

  infix 2 AND
  infix 1 OR XOR
in
  fun CH (x, y, z) =
    (x AND y) XOR ((NOT x) AND z)

  fun MAJ (x, y, z) =
    (x AND y) XOR (x AND z) XOR (y AND z)

  fun BSIG0 x = (ROTR 2  x) XOR (ROTR 13 x) XOR (ROTR 22 x)
  fun BSIG1 x = (ROTR 6  x) XOR (ROTR 11 x) XOR (ROTR 25 x)
  fun SSIG0 x = (ROTR 7  x) XOR (ROTR 18 x) XOR (SHR  3  x)
  fun SSIG1 x = (ROTR 17 x) XOR (ROTR 19 x) XOR (SHR  10 x)

  (**
   * SHA-224 and SHA-256 use the same sequence of sixty-four constant
   * 32-bit words, K0, K1, ..., K63.  These words represent the first 32
   * bits of the fractional parts of the cube roots of the first sixty-
   * four prime numbers.  In hex, these constant words are as follows
   * (from left to right):
   *)
  val K : Word32.word vector = vector
    [ 0wx428a2f98, 0wx71374491, 0wxb5c0fbcf, 0wxe9b5dba5
    , 0wx3956c25b, 0wx59f111f1, 0wx923f82a4, 0wxab1c5ed5
    , 0wxd807aa98, 0wx12835b01, 0wx243185be, 0wx550c7dc3
    , 0wx72be5d74, 0wx80deb1fe, 0wx9bdc06a7, 0wxc19bf174
    , 0wxe49b69c1, 0wxefbe4786, 0wx0fc19dc6, 0wx240ca1cc
    , 0wx2de92c6f, 0wx4a7484aa, 0wx5cb0a9dc, 0wx76f988da
    , 0wx983e5152, 0wxa831c66d, 0wxb00327c8, 0wxbf597fc7
    , 0wxc6e00bf3, 0wxd5a79147, 0wx06ca6351, 0wx14292967
    , 0wx27b70a85, 0wx2e1b2138, 0wx4d2c6dfc, 0wx53380d13
    , 0wx650a7354, 0wx766a0abb, 0wx81c2c92e, 0wx92722c85
    , 0wxa2bfe8a1, 0wxa81a664b, 0wxc24b8b70, 0wxc76c51a3
    , 0wxd192e819, 0wxd6990624, 0wxf40e3585, 0wx106aa070
    , 0wx19a4c116, 0wx1e376c08, 0wx2748774c, 0wx34b0bcb5
    , 0wx391c0cb3, 0wx4ed8aa4a, 0wx5b9cca4f, 0wx682e6ff3
    , 0wx748f82ee, 0wx78a5636f, 0wx84c87814, 0wx8cc70208
    , 0wx90befffa, 0wxa4506ceb, 0wxbef9a3f7, 0wxc67178f2
    ]
end
end

