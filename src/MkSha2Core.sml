
functor MkSha2Core(structure I : SHA2INIT
                   structure F : SHA2FUNC
                   sharing type F.Word.word = I.Word.word)
 : SHA2CORE
   where type Word.word = I.Word.word
     and type      'a t = 'a Sha2Type.t =
struct
local
  structure BasisWord = Word

  structure R   = Reader
  structure Blk = MkBlock(I.Word)

  open F
  open ForLoop

  (* fold on Nat sequence from 0 to n *)
  fun foldNat f e n =
    let
      fun go i e =
        if i = n
        then e
        else go (i+1) (f(i,e))
    in go 0 e
    end
in
  open Sha2Type
  structure Word = F.Word

  (** NOTE: SML cannot resolve overloading on op+ for F.Word.word type *)
  val op+ = F.Word.+

  (**
   * 6.1. SHA-224 and SHA-256 Initialization
   *)
  val H0 = I.H0

  fun onBitWidth v32 v64 =
    if Word.wordSize = 32
    then v32
    else v64

  fun toString (Hash(h0,h1,h2,h3,h4,h5,h6,h7)) =
    let fun tos w = StringCvt.padLeft #"0" ((onBitWidth 4 8) * 2) (Word.toString w)
    in concat (map F.Word.toString [h0,h1,h2,h3,h4,h5,h6,h7]) end

  (**
   * b. K "0"s are appended where K is the smallest, non-negative solution
   *    to the equation
   *
   *       ( L + 1 + K ) mod 512 = 448
   *)
  fun getK L =
    let
      val (w,m) = onBitWidth (512, 448) (1024, 896)
      val op+ = Int.+
    in
      if (L + 1) mod w <= m
      then m - ((L + 1) mod w)
      else w - ((L + 1) mod w) + m
    end

  infix :==
  fun (arr,n) :== v =
    Array.update(arr,n,v)

  (** Prepare the message schedule W *)
  fun scheduleW (M:Blk.t) : F.Word.word vector =
    let
      val sub = Array.sub
      val len = onBitWidth 64 80
      val W : F.Word.word array = Array.array(len, F.Word.fromInt 0)
    in
      for' 0 (fn t=> t<=15) (fn t=>
        (W,t) :== Blk.sub(M,t));
      for' 16 (fn t=> t<=(len-1)) (fn t=>
        (W,t) :== SSIG1(sub(W,t-2)) + sub(W,t-7) + SSIG0(sub(W,t-15)) + sub(W,t-16));
      Array.vector W
    end

  (** 6.2.3. Perform the main hash computation: *)
  fun compute W (t, Hash(a,b,c,d,e,f,g,h)) =
    let
      val sub = Vector.sub
      val T1 = h + BSIG1 e + CH(e,f,g) + sub(K,t) + sub(W,t)
      val T2 = BSIG0 a + MAJ(a,b,c)
    in
      Hash (T1+T2,a,b,c,d+T1,e,f,g)
    end

  (* compute hash for a 512bit block *)
  fun process_block (M,H) =
    let
      val _ = () (* print(toString H ^ "\n") *)
      val W = scheduleW M
      val Hash (a,b,c,d,e,f,g,h) = foldNat (compute W) H (onBitWidth 64 80)
      val Hash H1 = H
    in
      (** 4. Compute the intermediate hash value H(i) *)
      Hash ( a + #1 H1, b + #2 H1, c + #3 H1, d + #4 H1
           , e + #5 H1, f + #6 H1, g + #7 H1, h + #8 H1 )
    end

  fun repeatN 0 _ = []
    | repeatN n x = x::repeatN (n-1) x

  fun scanWord32 getWord8 ss =
    let
      infix <<
      val op<< = Word32.<<
      val toWord32 = Word32.fromInt o Word8.toInt
    in
      case R.fmap (map toWord32) (R.seqN getWord8 4) ss
        of SOME([w3,w2,w1,w0],ss) =>
             SOME(foldl Word32.orb (Word32.fromInt 0)
                  [ w3<<0w24, w2<<0w16, w1<<0w08, w0<<0w00], ss)
         | NONE => NONE
    end

  fun foldli f e =
    let
      val op+ = Int.+
      fun go _ e [] = e
        | go i e (x::xs) = go (i+1) (f(i,x,e)) xs
    in
      go 0 e
    end

  fun scanWord (getWord8: (Word8.word, 'a) R.t) (ss:'a) : (I.Word.word * 'a) option =
    let
      infix <<
      val (op<<,op* ) = (I.Word.<<, Int.* )
      val toWord = I.Word.fromInt o Word8.toInt
      val n = onBitWidth 4 8
    in
      R.bind (R.seqN getWord8 n ss) (fn (ws,ss) =>
      SOME (foldli
              (fn(i,x,w) =>
                 I.Word.orb(
                   x << (BasisWord.fromInt((n-i-1)*8)), w))
              (I.Word.fromInt 0) 
              (map toWord ws), ss))
    end

  (**
   * process tail of the entity stream and padding words
   *
   * 4.1.  SHA-224 and SHA-256
   * Suppose a message has length L < 2^64.  Before it is input to the
   * hash function, the message is padded on the right as follows:
   *
   * 4.2.  SHA-384 and SHA-512
   * Suppose a message has length L < 2^128.  Before it is input to the
   * hash function, the message is padded on the right as follows:
   *)
  fun process_tail n ws H =
    let
      val op+ = Int.+
      val L = (length ws + n * 4) * 8
      val K = getK L
      fun go H ss =
        case Blk.scan (scanWord List.getItem) ss
          of SOME(M,ss) => go (process_block (M,H)) ss
           | NONE       => H
    in
      go
        H
        (List.concat [ (* padding sequence *)
           ws
         , [0wx80] (* append "1" *)
         , repeatN (K div 8) 0w0
         , ToWord8List.fromWord64 (Word64.fromInt L)
         ])
    end

  (** 6.2.  SHA-224 and SHA-256 Processing *)
  fun scan getw8 =
    let
      val op+ = Int.+
      fun entity n H ss =
        case Blk.scan (scanWord getw8) ss
          of SOME(M,ss) => entity (n+1) (process_block (M,H)) ss
           | NONE       => (n, H, ss)

      fun tail (n, H, ss) =
        let val (ws,ss) = R.read_all getw8 ss in
          SOME(process_tail n ws H, ss)
        end
    in
      tail o (entity 0 H0)
    end

end (* local *)
end

