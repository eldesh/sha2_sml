
functor MkSha224And256(val bit : int) =
struct
local
  structure Functions = Sha224And256Func
  open Functions
  structure For = ForLoop
  open For

  structure R = Reader
  structure Blk = Block512

  fun bind  NONE    _ = NONE
    | bind (SOME x) f = f x
in
  structure Word = Word32
  type word = Word.word

  val bit = bit

  datatype t = Hash of word * word * word * word
                     * word * word * word * word


  fun toString (Hash(h0,h1,h2,h3,h4,h5,h6,h7)) =
    String.concatWith "\n" (map Word.toString [h0,h1,h2,h3,h4,h5,h6,h7])


  (**
   * 6.1. SHA-224 and SHA-256 Initialization
   *)
  val H0 =
    if bit = 224 then
      Hash ( 0wxc1059ed8, 0wx367cd507, 0wx3070dd17, 0wxf70e5939
           , 0wxffc00b31, 0wx68581511, 0wx64f98fa7, 0wxbefa4fa4 )
    else if bit = 256 then
      Hash ( 0wx6a09e667, 0wxbb67ae85, 0wx3c6ef372, 0wxa54ff53a
           , 0wx510e527f, 0wx9b05688c, 0wx1f83d9ab, 0wx5be0cd19 )
    else
      raise Fail "unknown algorithm expect SHA224 or SHA256"

  (**
   * 4.1.  SHA-224 and SHA-256
   *
   * Suppose a message has length L < 2^64.  Before it is input to the
   * hash function, the message is padded on the right as follows:
   *)

  (**
   * a. "1" is appended.  Example: if the original message is "01010000",
   *    this is padded to "010100001".
   *
   * b. K "0"s are appended where K is the smallest, non-negative solution
   *    to the equation
   *
   *       ( L + 1 + K ) mod 512 = 448
   *)
  fun getK L =
    if (L + 1) mod 512 <= 448
    then 448 - ((L + 1) mod 512)
    else 512 - ((L + 1) mod 512) + 448

  fun padding ws =
    let
      val L = Vector.length ws * 8
      val K = getK L
    in
      Vector.concat
      [ ws
      , Vector.fromList [0wx80] (* append "1" *)
      , Vector.tabulate(K div 8, fn _=> 0w0)
      , Vector.fromList(ToWord8List.fromWord64 (Word64.fromInt L))
      ]
    end

  fun split (ss: Word8.word vector) : word list =
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
        (W,t) :== Blk.sub(M,t));
      for' 16 (fn t=> t<=63) (fn t=>
        (W,t) :== SSIG1(sub(W,t-2)) + sub(W,t-7) + SSIG0(sub(W,t-15)) + sub(W,t-16));
      Array.vector W
    end

  (** 3. Perform the main hash computation: *)
  fun compute W (t, Hash(a,b,c,d,e,f,g,h)) =
    let
      val sub = Vector.sub
      val T1 = h + BSIG1 e + CH(e,f,g) + sub(K,t) + sub(W,t)
      val T2 = BSIG0 a + MAJ(a,b,c)
    in
      Hash (T1+T2,a,b,c,d+T1,e,f,g)
    end

  fun process_block (M,H) =
    let
      val _ = print(toString H ^ "\n")
      val W = scheduleW M
      val Hash (a,b,c,d,e,f,g,h) = foldNat (compute W) H 64
      val Hash H1 = H
    in
      (** 4. Compute the intermediate hash value H(i) *)
      Hash ( a + #1 H1, b + #2 H1, c + #3 H1, d + #4 H1
           , e + #5 H1, f + #6 H1, g + #7 H1, h + #8 H1 )
    end

  fun repeatN 0 _ = []
    | repeatN n x = x::repeatN (n-1) x

  fun mkgetw32 getw8 ss =
    let
      infix <<
      val op<< = Word32.<<
      val toWord32 = Word32.fromInt o Word8.toInt
    in
      case R.fmap (map toWord32) (R.seqN getw8 4) ss
        of SOME([w3,w2,w1,w0],ss) =>
             SOME(foldl Word32.orb 0w0
                  [ w3<<0w24, w2<<0w16, w1<<0w08, w0<<0w00], ss)
         | NONE => NONE
    end

  (* process tail of the entity stream and padding words *)
  fun process_tail n ws H =
    let
      val getw32 = mkgetw32 List.getItem
      val L = (length ws + n*4) * 8
      val K = getK L
      val ws = List.concat [
                 ws
               , [0wx80] (* append "1" *)
               , repeatN (K div 8) 0w0
               , ToWord8List.fromWord64 (Word64.fromInt L)
               ]
      fun go H ss =
        case Blk.scan getw32 ss
          of SOME(M,ss) => go (process_block (M,H)) ss
           | NONE       => H
    in
      go H ws
    end

  (* 6.2.  SHA-224 and SHA-256 Processing *)
  fun process (Ms:Block512.t list) =
    foldl process_block H0 Ms

  fun scan getw8 =
    let
      val getw32 = mkgetw32 getw8
      fun go n H ss =
        case Blk.scan getw32 ss
          of SOME(M,ss) => go (n+1) (process_block (M,H)) ss
           | NONE       =>
               let val SOME(ws,ss) = R.read_all getw8 ss in
                 SOME(process_tail n ws H, ss)
               end
    in
      go 0 H0
    end

  end (* local *)

  fun hash w8s =
    let
      val pw8s = padding w8s
      val w32s = split pw8s
      fun go ss =
        case Block512.scan List.getItem ss
          of NONE => []
           | SOME(b,ss) => b::go ss
    in
      case process (go w32s)
        of Hash (H0,H1,H2,H3,H4,H5,H6,H7) =>
           if bit = 224 then
             [H0,H1,H2,H3,H4,H5,H6]
           else if bit = 256 then
             [H0,H1,H2,H3,H4,H5,H6,H7]
           else
             raise Fail "unknown algorithm expect SHA224 or SHA256"
    end

end (* local *)
end

