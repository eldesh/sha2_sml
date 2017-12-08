(**
 * 5.  Functions and Constants Used
 * 5.2.  SHA-384 and SHA-512
 *
 * SHA-384 and SHA-512 each use six logical functions, where each
 * function operates on 64-bit words, which are represented as x, y, and
 * z.  The result of each function is a new 64-bit word.
 *)
structure Sha384And512Func =
struct
local
  open Sha384And512Ops

  infix 2 AND
  infix 1 OR XOR
in
  fun CH (x, y, z) =
    (x AND y) XOR ((NOT x) AND z)

  fun MAJ (x, y, z) =
    (x AND y) XOR (x AND z) XOR (y AND z)

  fun BSIG0 x = (ROTR 28 x) XOR (ROTR 34 x) XOR (ROTR 39 x)
  fun BSIG1 x = (ROTR 14 x) XOR (ROTR 18 x) XOR (ROTR 41 x)
  fun SSIG0 x = (ROTR 1  x) XOR (ROTR 8  x) XOR (SHR  7  x)
  fun SSIG1 x = (ROTR 19 x) XOR (ROTR 61 x) XOR (SHR  6  x)

  (**
   * SHA-384 and SHA-512 use the same sequence of eighty constant 64-bit
   * words, K0, K1, ... K79.  These words represent the first 64 bits of
   * the fractional parts of the cube roots of the first eighty prime
   * numbers.  In hex, these constant words are as follows (from left to
   * right):
   *)
  val K : Word64.word vector = vector
    [ 0wx428a2f98d728ae22, 0wx7137449123ef65cd, 0wxb5c0fbcfec4d3b2f, 0wxe9b5dba58189dbbc
    , 0wx3956c25bf348b538, 0wx59f111f1b605d019, 0wx923f82a4af194f9b, 0wxab1c5ed5da6d8118
    , 0wxd807aa98a3030242, 0wx12835b0145706fbe, 0wx243185be4ee4b28c, 0wx550c7dc3d5ffb4e2
    , 0wx72be5d74f27b896f, 0wx80deb1fe3b1696b1, 0wx9bdc06a725c71235, 0wxc19bf174cf692694
    , 0wxe49b69c19ef14ad2, 0wxefbe4786384f25e3, 0wx0fc19dc68b8cd5b5, 0wx240ca1cc77ac9c65
    , 0wx2de92c6f592b0275, 0wx4a7484aa6ea6e483, 0wx5cb0a9dcbd41fbd4, 0wx76f988da831153b5
    , 0wx983e5152ee66dfab, 0wxa831c66d2db43210, 0wxb00327c898fb213f, 0wxbf597fc7beef0ee4
    , 0wxc6e00bf33da88fc2, 0wxd5a79147930aa725, 0wx06ca6351e003826f, 0wx142929670a0e6e70
    , 0wx27b70a8546d22ffc, 0wx2e1b21385c26c926, 0wx4d2c6dfc5ac42aed, 0wx53380d139d95b3df
    , 0wx650a73548baf63de, 0wx766a0abb3c77b2a8, 0wx81c2c92e47edaee6, 0wx92722c851482353b
    , 0wxa2bfe8a14cf10364, 0wxa81a664bbc423001, 0wxc24b8b70d0f89791, 0wxc76c51a30654be30
    , 0wxd192e819d6ef5218, 0wxd69906245565a910, 0wxf40e35855771202a, 0wx106aa07032bbd1b8
    , 0wx19a4c116b8d2d0c8, 0wx1e376c085141ab53, 0wx2748774cdf8eeb99, 0wx34b0bcb5e19b48a8
    , 0wx391c0cb3c5c95a63, 0wx4ed8aa4ae3418acb, 0wx5b9cca4f7763e373, 0wx682e6ff3d6b2b8a3
    , 0wx748f82ee5defb2fc, 0wx78a5636f43172f60, 0wx84c87814a1f0ab72, 0wx8cc702081a6439ec
    , 0wx90befffa23631e28, 0wxa4506cebde82bde9, 0wxbef9a3f7b2c67915, 0wxc67178f2e372532b
    , 0wxca273eceea26619c, 0wxd186b8c721c0c207, 0wxeada7dd6cde0eb1e, 0wxf57d4f7fee6ed178
    , 0wx06f067aa72176fba, 0wx0a637dc5a2c898a6, 0wx113f9804bef90dae, 0wx1b710b35131c471b
    , 0wx28db77f523047d84, 0wx32caab7b40c72493, 0wx3c9ebe0a15c9bebc, 0wx431d67c49c100d4c
    , 0wx4cc5d4becb3e42b6, 0wx597f299cfc657e2a, 0wx5fcb6fab3ad6faec, 0wx6c44198c4a475817
    ]
end
end

