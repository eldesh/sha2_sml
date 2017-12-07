
functor MkSha224And256 (S : sig val H0 : Sha2Type.t end) =
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
  open Sha2Type

  fun toString (Hash(h0,h1,h2,h3,h4,h5,h6,h7)) =
    String.concatWith "\n" (map Word.toString [h0,h1,h2,h3,h4,h5,h6,h7])

  (**
   * 6.1. SHA-224 and SHA-256 Initialization
   *)
  val H0 = S.H0

  (**
   * 4.1.  SHA-224 and SHA-256
   *
   * Suppose a message has length L < 2^64.  Before it is input to the
   * hash function, the message is padded on the right as follows:
   *)

  (**
   * b. K "0"s are appended where K is the smallest, non-negative solution
   *    to the equation
   *
   *       ( L + 1 + K ) mod 512 = 448
   *)
  fun getK L =
    if (L + 1) mod 512 <= 448
    then 448 - ((L + 1) mod 512)
    else 512 - ((L + 1) mod 512) + 448

  infix :==
  fun (arr,n) :== v =
    Array.update(arr,n,v)

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

  (* fold on Nat sequence from 0 to n *)
  fun foldNat f e n =
    let
      fun go i e =
        if i = n
        then e
        else go (i+1) (f(i,e))
    in
      go 0 e
    end

  (* compute hash for a 512bit block *)
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

  fun scanWord32 getWord8 ss =
    let
      infix <<
      val op<< = Word32.<<
      val toWord32 = Word32.fromInt o Word8.toInt
    in
      case R.fmap (map toWord32) (R.seqN getWord8 4) ss
        of SOME([w3,w2,w1,w0],ss) =>
             SOME(foldl Word32.orb 0w0
                  [ w3<<0w24, w2<<0w16, w1<<0w08, w0<<0w00], ss)
         | NONE => NONE
    end

  (* process tail of the entity stream and padding words *)
  fun process_tail n ws H =
    let
      val scanWord32 = scanWord32 List.getItem
      val L = (length ws + n*4) * 8
      val K = getK L
      val ws = List.concat [
                 ws
               , [0wx80] (* append "1" *)
               , repeatN (K div 8) 0w0
               , ToWord8List.fromWord64 (Word64.fromInt L)
               ]
      fun go H ss =
        case Blk.scan scanWord32 ss
          of SOME(M,ss) => go (process_block (M,H)) ss
           | NONE       => H
    in
      go H ws
    end

  (* 6.2.  SHA-224 and SHA-256 Processing *)
  fun scan getw8 =
    let
      val scanWord32 = scanWord32 getw8
      fun go n H ss =
        case Blk.scan scanWord32 ss
          of SOME(M,ss) => go (n+1) (process_block (M,H)) ss
           | NONE       =>
               let val SOME(ws,ss) = R.read_all getw8 ss in
                 SOME(process_tail n ws H, ss)
               end
    in
      go 0 H0
    end

  fun hash str =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) Substring.getc in
      (#1 o valOf) (scan getWord8 (Substring.full str))
    end

  fun hash_vector vector =
    (#1 o valOf) (scan VectorSlice.getItem (VectorSlice.full vector))

  fun hash_bin_stream strm =
    (#1 o valOf) (scan BinIO.StreamIO.input1 strm)

  fun hash_text_stream strm =
    let val getWord8 = R.fmap (Word8.fromInt o Char.ord) TextIO.StreamIO.input1 in
      (#1 o valOf) (scan getWord8 strm)
    end

end (* local *)
end

