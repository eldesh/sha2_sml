
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
    H ( 0wx6a09e667
      , 0wxbb67ae85
      , 0wx3c6ef372
      , 0wxa54ff53a
      , 0wx510e527f
      , 0wx9b05688c
      , 0wx1f83d9ab
      , 0wx5be0cd19
      )
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

  fun process ws =
    let
      val Wt = Vector.tabulate(64, fn t=>
        if t <= 15 then
          List.nth(ws, t)
        else
          SSIG1(W(t-2)) + W(t-7) + SSIG0(w(t-15)) + W(t-16)
      )
      val Wt1 = Vector.tabulate(, fn i=> )
    in
    end

end


structure Sha2 =
struct
end

