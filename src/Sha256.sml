
(**
 * 2.d. block = 512-bit or 1024-bit string
 *)
functor MkBlock(type word) =
struct
  datatype t = Block of word vector
end


(**
 * hash quantity H(N)
 *)
structure Sha256Quantity =
struct
  type word = Word32.word
  datatype t = H of word * word * word * word
                  * word * word * word * word

  fun toString (H(h0,h1,h2,h3,h4,h5,h6,h7)) =
      concat
        (map
           (StringCvt.padLeft #"0" 8 o Word32.toString)
           [h0,h1,h2,h3,h4,h5,h6,h7])

  fun toTuple (H(h0,h1,h2,h3,h4,h5,h6,h7)) =
    (h0,h1,h2,h3,h4,h5,h6,h7)

  fun fromTuple (h0,h1,h2,h3,h4,h5,h6,h7) =
    H (h0,h1,h2,h3,h4,h5,h6,h7)

  (**
   * 6.1. SHA-224 and SHA-256 Initialization
   *)
  fun init () : t =
    H ( 0wx6a09e667 (* H(0)0 *)
      , 0wxbb67ae85 (* H(0)1 *)
      , 0wx3c6ef372 (* H(0)2 *)
      , 0wxa54ff53a (* H(0)3 *)
      , 0wx510e527f (* H(0)4 *)
      , 0wx9b05688c (* H(0)5 *)
      , 0wx1f83d9ab (* H(0)6 *)
      , 0wx5be0cd19 (* H(0)7 *)
      )
end

structure Sha256 =
struct
  structure Quantity = Sha256Quantity
  type word = Quantity.word
  datatype t = Ctx of { (* a message schedule: W0, W1, ..., W64 *)
                        sche : word vector
                      , (* eight working variables *)
                        work : word array
                      , (* hash value of 8 words *)
                        hash : word vector
                      }

   (**
    * 3.  Operations on Words
    *
    * The following logical operators will be applied to words in all four
    * hash operations specified herein.  SHA-224 and SHA-256 operate on
    * 32-bit words while SHA-384 and SHA-512 operate on 64-bit words.
    *
    * In the operations below, x<<n is obtained as follows: discard the
    * leftmost n bits of x and then pad the result with n zeroed bits on
    * the right (the result will still be the same number of bits).
    * Similarly, x>>n is obtained as follows: discard the rightmost n bits
    * of x and then prepend the result with n zeroed bits on the left (the
    * result will still be the same number of bits).
    *)
  structure Ops =
  struct
    infix >>
    infix <<
    val op>> = Word32.>>
    val op<< = Word32.<<
    val w = Word.fromInt Word32.wordSize

    (**
     * a. Bitwise logical word operations
     *       X AND Y  =  bitwise logical "and" of  X and Y.
     *)
    fun AND (x,y) = Word32.andb (x, y)
    (*       X OR Y   =  bitwise logical "inclusive-or" of X and Y. *)
    fun OR  (x,y) = Word32.orb (x, y)
    (*       X XOR Y  =  bitwise logical "exclusive-or" of X and Y. *)
    fun XOR (x,y) = Word32.xorb (x, y)
    (*       NOT X    =  bitwise logical "complement" of X.  *)
    fun NOT x = Word32.notb x

    infix 2 AND
    infix 1 OR XOR

    (**
     * c. The right shift operation SHR^n(x), where x is a w-bit word and n
     * is an integer with 0 <= n < w, is defined by
     *    SHR^n(x) = x>>n
     *)
    fun SHR n x =
      let val n = Word.fromInt n in
        x >> n
      end

    (**
     * d. The rotate right (circular right shift) operation ROTR^n(x), where
     *    x is a w-bit word and n is an integer with 0 <= n < w, is defined
     *    by
     *       ROTR^n(x) = (x>>n) OR (x<<(w-n))
     *)
    fun ROTR n x =
      let val n = Word.fromInt n in
        (x >> n) OR (x << (w-n))
      end
  end

  open Ops
  infix 2 AND
  infix 1 OR XOR

  (**
   * 5.  Functions and Constants Used
   *
   * SHA-224 and SHA-256 use six logical functions, where each function
   * operates on 32-bit words, which are represented as x, y, and z.  The
   * result of each function is a new 32-bit word.
   *)
  structure Functions =
  struct
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

  open Functions

  (**
   * 4.1.  SHA-224 and SHA-256
   *
   * Suppose a message has length L < 2^64.  Before it is input to the
   * hash function, the message is padded on the right as follows:
   *)
  fun padd ws =
    let
      val L = Vector.length ws * 8
      val K = ref 0
    in
      while (L + 1 + !K) mod 512 <> 448 do
      (
       K := !K + 1
      );
      Vector.concat
      [ ws
      , Vector.fromList [0wx80] (* append "1" *)
      , Vector.tabulate(!K div 8, fn _=> 0w0)
      , Vector.fromList(ToWord8List.fromWord64 (Word64.fromInt L))
      ]
    end

  fun assert x y =
    if x <> y then raise Fail "assert"
    else ()

  val () =
    let
      val exp = Vector.fromList
                [ 0wx61, 0wx62, 0wx63, 0wx64, 0wx65, 0wx80, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00
                , 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00
                , 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00
                , 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx28
                ]

      val act = padd (Vector.fromList[0wx61, 0wx62, 0wx63, 0wx64, 0wx65])
    in
      assert exp act
    end

  fun bind  NONE    _ = NONE
    | bind (SOME x) f = f x

  fun split ss =
    let
      val conv = Word32.fromInt o Word8.toInt
      val get = VectorSlice.getItem
      fun split' ss =
        case (bind (get ss) (fn (w3,ss) =>
              bind (get ss) (fn (w2,ss) =>
              bind (get ss) (fn (w1,ss) =>
              bind (get ss) (fn (w0,ss) =>
              SOME((w3,w2,w1,w0),ss))))))
          of NONE => []
           | SOME((w3,w2,w1,w0),ss) =>
              foldl Word32.orb 0w0 [
                Word32.<< (conv w3, 0w24),
                Word32.<< (conv w2, 0w16),
                Word32.<< (conv w1, 0w08),
                Word32.<< (conv w0, 0w00)
              ]
              :: split' ss
    in
      split' (VectorSlice.full ss)
    end

  fun list vec =
    let
      fun unfoldr f e =
        case f e
          of NONE => []
           | SOME(x,e) => x::unfoldr f e
    in
      unfoldr VectorSlice.getItem (VectorSlice.full vec)
    end

  fun for i cond step body =
    let
      fun for' i =
        if cond i then 
          (body i;
           for' (step i))
        else
          ()
    in
      for' i
    end

  fun for' i cond =
    for i cond (fn i=>i+1)

  local
    structure B = Block512
  in

  fun print_t (a,b,c,d,e,f,g,h) =
    let
      val ss = map (StringCvt.padLeft #"0" 8 o Word32.toString) [a,b,c,d,e,f,g,h]
    in
      print (String.concatWith " " ss ^ "\n")
    end

  fun print_h H =
    let
      val (h0,h1,h2,h3,h4,h5,h6,h7) = Sha256Quantity.toTuple H
    in
      print (String.concatWith "\n" (map Word32.toString [h0,h1,h2,h3,h4,h5,h6,h7]) ^ "\n")
    end

  fun print_w W =
    let
      val int = Int.toString
      val word32 = Word32.toString
      fun go n ws =
        case VectorSlice.getItem ws
          of SOME(w,ws) => (print(concat["W[", int n, "] = ", word32 w, "\n"]);
                            go (n+1) ws)
           | NONE => ()
    in
      go 0 (VectorSlice.full W)
    end

  local
    infix :==
    fun (arr,n) :== v =
      Array.update(arr,n,v)

    fun foldNat f e n =
      let
        fun go i e =
          if i = n
          then e
          else go (i+1) (f(i,e))
      in
        go 0 e
      end
  in

  (** Prepare the message schedule W *)
  fun scheduleW M =
    let
      val sub = Array.sub
      val W : Word32.word array = Array.array(64, 0w0)
    in
      for' 0 (fn t=> t<=15) (fn t=>
        (W,t) :== B.sub(M,t));
      for' 16 (fn t=> t<=63) (fn t=>
        (W,t) :== SSIG1(sub(W,t-2)) + sub(W,t-7) + SSIG0(sub(W,t-15)) + sub(W,t-16));
      Array.vector W
    end

  fun foldli f =
    let
      fun go i e [] = e
        | go i e (x::xs) = go (i+1) (f(i,x,e)) xs
    in
      go 0
    end

  (* 6.2.  SHA-224 and SHA-256 Processing *)
  fun process (Ms:Block512.t list) =
    let
      val sub = Vector.sub
    in
      foldli
        (fn (i,M,H)=>
         let
           (** Prepare the message schedule W *)
           val W = scheduleW M
           val _ = (print_h H; print "\n")
           val (a,b,c,d,e,f,g,h) =
             foldNat
               (** 2. Initialize the working variables: *)
               (fn (t,H as (a,b,c,d,e,f,g,h))=>
                let
                  (** 3. Perform the main hash computation: *)
                  val T1 = h + BSIG1 e + CH(e,f,g) + sub(K,t) + sub(W,t)
                  val T2 = BSIG0 a + MAJ(a,b,c)
                in
                  (T1+T2,a,b,c,d+T1,e,f,g)
                end)
               (Sha256Quantity.toTuple H)
               64
            val H1 = Sha256Quantity.toTuple H
         in
           (** 4. Compute the intermediate hash value H(i) *)
           Sha256Quantity.fromTuple
             ( a + #1 H1
             , b + #2 H1
             , c + #3 H1
             , d + #4 H1
             , e + #5 H1
             , f + #6 H1
             , g + #7 H1
             , h + #8 H1 )
         end)
        (Sha256Quantity.init())
        Ms
    end
  end
  end (* local *)

  fun hash w8s =
    let
      val pw8s = padd w8s
      val w32s = split pw8s
      fun go ss =
        case Block512.scan List.getItem ss
          of NONE => []
           | SOME(b,ss) => b::go ss
    in
      process (go w32s)
    end
end

