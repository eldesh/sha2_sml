
functor Sha384And512Core (S : sig val H0 : Sha2Type64.t end) (* : SHA2CORE *) =
struct
local
  structure Functions = Sha384And512Func
  open Functions
  structure For = ForLoop
  open For

  structure R = Reader
  structure Blk = Block1024

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
  open Sha2Type64

  fun toString (Hash(h0,h1,h2,h3,h4,h5,h6,h7)) =
    String.concatWith "\n" (map Word.toString [h0,h1,h2,h3,h4,h5,h6,h7])

  (**
   * 6.3. SHA-384 and SHA-512 Initialization
   *)
  val H0 = S.H0

  (**
   * b. K "0"s are appended where K is the smallest, non-negative solution
   *    to the equation
   *
   *       ( L + 1 + K ) mod 1024 = 896
   *)
  fun getK L =
    if (L + 1) mod 1024 <= 896
    then 896  - ((L + 1) mod 1024)
    else 1024 - ((L + 1) mod 1024) + 896

  infix :==
  fun (arr,n) :== v =
    Array.update(arr,n,v)

  (** Prepare the message schedule W *)
  fun scheduleW M =
    let
      val sub = Array.sub
      val W : Word64.word array = Array.array(80, 0w0)
    in
      for' 0 (fn t=> t<=15) (fn t=>
        (W,t) :== Blk.sub(M,t));
      for' 16 (fn t=> t<=79) (fn t=>
        (W,t) :== SSIG1(sub(W,t-2)) + sub(W,t-7) + SSIG0(sub(W,t-15)) + sub(W,t-16));
      Array.vector W
    end

  (** 6.4.3. Perform the main hash computation: *)
  fun compute W (t, Hash(a,b,c,d,e,f,g,h)) =
    let
      val sub = Vector.sub
      val T1 = h + BSIG1 e + CH(e,f,g) + sub(K,t) + sub(W,t)
      val T2 = BSIG0 a + MAJ(a,b,c)
      val H = Hash (T1+T2,a,b,c,d+T1,e,f,g)
    in
      print(Int.toString t ^ " " ^ Sha2Type64.toString H ^ "\n");
      H
    end

  (* compute hash for a 1024bit block *)
  fun process_block (M,H) =
    let
      val _ = print(toString H ^ "\n")
      val W = scheduleW M
      val Hash (a,b,c,d,e,f,g,h) = foldNat (compute W) H 80
      val Hash H1 = H
    in
      (** 4. Compute the intermediate hash value H(i) *)
      Hash ( a + #1 H1, b + #2 H1, c + #3 H1, d + #4 H1
           , e + #5 H1, f + #6 H1, g + #7 H1, h + #8 H1 )
    end

  fun repeatN 0 _ = []
    | repeatN n x = x::repeatN (n-1) x

  fun scanWord64 getWord8 ss =
    let
      infix <<
      val op<< = Word64.<<
      val toWord64 = Word64.fromInt o Word8.toInt
    in
      case R.fmap (map toWord64) (R.seqN getWord8 8) ss
        of SOME([w7,w6,w5,w4,w3,w2,w1,w0],ss) =>
             SOME(foldl Word64.orb 0w0
                  [ w7<<0w56, w6<<0w48, w5<<0w40, w4<<0w32
                  , w3<<0w24, w2<<0w16, w1<<0w08, w0<<0w00], ss)
         | NONE => NONE
    end

  (**
   * process tail of the entity stream and padding words
   *
   * 4.2.  SHA-384 and SHA-512
   * Suppose a message has length L < 2^128.  Before it is input to the
   * hash function, the message is padded on the right as follows:
   *)
  fun process_tail n ws H =
    let
      val scanWord64 = scanWord64 List.getItem
      val L = (length ws + n*8) * 8
      val K = getK L
      (* padding sequence *)
      val ws = List.concat [
                 ws
               , [0wx80] (* append "1" *)
               , repeatN (K div 8) 0w0
               (* 128-bit representation of L *)
               , ToWord8List.fromWord64 (Word64.fromInt 0)
               , ToWord8List.fromWord64 (Word64.fromInt L)
               ]
      fun go H ss =
        case Blk.scan scanWord64 ss
          of SOME(M,ss) => (print((Blk.toString M) ^ "\n");
                            go (process_block (M,H)) ss)
           | NONE       => H
    in
      go H ws
    end

  (** 6.4.  SHA-384 and SHA-512 Processing *)
  fun scan getw8 =
    let
      fun entity n H ss =
        case Blk.scan (scanWord64 getw8) ss
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

