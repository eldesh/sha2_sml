
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
structure Sha224Quantity =
struct
  type word = Word32.word
  datatype t = H of word * word * word * word
                  * word * word * word * word

  (**
   * 6.1. SHA-224 and SHA-256 Initialization
   *)
  fun init () : t =
    H ( 0wxc1059ed8
      , 0wx367cd507
      , 0wx3070dd17
      , 0wxf70e5939
      , 0wxffc00b31
      , 0wx68581511
      , 0wx64f98fa7
      , 0wxbefa4fa4
      )
end



(**
 * hash quantity H(N)
 *)
structure Sha256Quantity =
struct
  type word = Word32.word
  datatype t = H of word * word * word * word
                  * word * word * word * word

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

structure Block512 =
struct
  (* 16 32-bit words => 512bit block *)
  datatype t = Block of Word32.word vector

  exception WrongLength of Word32.word vector

  fun fromVector vec =
    if Vector.length vec = 16 then
      Block vec
    else
      raise WrongLength vec

  fun bind  NONE    _ = NONE
    | bind (SOME x) f = f x

  type ('a,'b) reader = ('a, 'b) StringCvt.reader

  fun seqN (get:('a,'b) reader) n : ('a list, 'b) reader =
    if n = 0 then (fn ss => SOME([], ss))
    else
      fn ss =>
        bind (get ss)            (fn (x ,ss) =>
        bind (seqN get (n-1) ss) (fn (xs,ss) =>
        SOME(x::xs,ss)))

  fun scan (get: (Word32.word,'a) StringCvt.reader) : (t,'a) StringCvt.reader =
    fn ss =>
      Option.map
          (fn(xs,ss)=> (Block (vector xs),ss))
          (seqN get 16 ss)
end


structure ToWord8Tuple =
struct
  (*
  fun fromWord16 w16 =
    let val conv = Word8.fromInt o Word16.toInt in
      ( conv (Word16.>> (Word16.andb (w16, 0wxff00), 0w8))
      , conv (Word16.>> (Word16.andb (w16, 0wx00ff), 0w0)))
    end
    *)
  
  fun fromWord32 w32 =
    let val conv = Word8.fromInt o Word32.toInt in
      ( conv (Word32.>> (Word32.andb (w32, 0wxff000000), 0w24))
      , conv (Word32.>> (Word32.andb (w32, 0wx00ff0000), 0w16))
      , conv (Word32.>> (Word32.andb (w32, 0wx0000ff00), 0w08))
      , conv (Word32.>> (Word32.andb (w32, 0wx000000ff), 0w00)))
    end
  
  fun fromWord64 w64 =
    let val conv = Word8.fromInt o Word64.toInt in
      ( conv (Word64.>> (Word64.andb (w64, 0wxff00000000000000), 0w56))
      , conv (Word64.>> (Word64.andb (w64, 0wx00ff000000000000), 0w48))
      , conv (Word64.>> (Word64.andb (w64, 0wx0000ff0000000000), 0w40))
      , conv (Word64.>> (Word64.andb (w64, 0wx000000ff00000000), 0w32))
      , conv (Word64.>> (Word64.andb (w64, 0wx00000000ff000000), 0w24))
      , conv (Word64.>> (Word64.andb (w64, 0wx0000000000ff0000), 0w16))
      , conv (Word64.>> (Word64.andb (w64, 0wx000000000000ff00), 0w08))
      , conv (Word64.>> (Word64.andb (w64, 0wx00000000000000ff), 0w00)))
    end
end

structure ToWord8List =
struct
  structure T = ToWord8Tuple
  (*
  fun fromWord16 w16 =
    let val (wh, wl) = T.fromWord16 w16 in
      [wh, wl]
    end
    *)

  fun fromWord32 w32 =
    let val (w3, w2, w1, w0) = T.fromWord32 w32 in
      [w3, w2, w1, w0]
    end

  fun fromWord64 w64 =
    let val (w7, w6, w5, w4, w3, w2, w1, w0) = T.fromWord64 w64 in
      [w7, w6, w5, w4, w3, w2, w1, w0]
    end
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
  end

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

  fun split [] = []
    | split (w3::w2::w1::w0::ws) =
        let
          val conv = Word32.fromInt o Word8.toInt
        in
          foldl Word32.orb 0w0 [
            Word32.<< (conv w3, 0w24),
            Word32.<< (conv w2, 0w16),
            Word32.<< (conv w1, 0w08),
            Word32.<< (conv w0, 0w00)
          ]
          ::split ws
        end

  fun list vec : Word32.word list =
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

  fun process (ws:Block512.t list) =
    let
      infix :==
      fun (arr,n) :== v =
        Array.update(arr,n,v)

        (*
      val Wt: Word32.word array = Array.array(64, 0w0)
      *)
    in
      (*
      for' 1 (fn i=> i<=N) (fn i=>
        for' 0 (fn t=> t<=15) (fn t=>
          (Wt,t) :== sub(M,i)
        );
      )
      *)
      (*
      val Wt = Vector.tabulate(64, fn t=>
        if t <= 15 then
          List.nth(ws, t)
        else
          SSIG1(W(t-2)) + W(t-7) + SSIG0(w(t-15)) + W(t-16)
      )
      val Wt1 = () (* Vector.tabulate(, fn i=> ()) *)
      *)
      ()
    end

end


structure Sha2 =
struct
end

