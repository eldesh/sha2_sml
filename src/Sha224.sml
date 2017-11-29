
(**
 * hash quantity H(N)
 *)
structure Sha224Quantity =
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


structure Sha224 =
struct
  structure Quantity = Sha224Quantity
  type word = Quantity.word
  datatype t = Ctx of { (* a message schedule: W0, W1, ..., W64 *)
                        sche : word vector
                      , (* eight working variables *)
                        work : word array
                      , (* hash value of 8 words *)
                        hash : word vector
                      }

  structure Ops = Sha224And256Ops
  open Ops

  structure Functions = Sha224And256Func
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
      val (h0,h1,h2,h3,h4,h5,h6,h7) = Quantity.toTuple H
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
               (Quantity.toTuple H)
               64
            val H1 = Quantity.toTuple H
         in
           (** 4. Compute the intermediate hash value H(i) *)
           Quantity.fromTuple
             ( a + #1 H1
             , b + #2 H1
             , c + #3 H1
             , d + #4 H1
             , e + #5 H1
             , f + #6 H1
             , g + #7 H1
             , h + #8 H1 )
         end)
        (Quantity.init())
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
      case process (go w32s)
        of Quantity.H (H0,H1,H2,H3,H4,H5,H6,H7)
            => [H0,H1,H2,H3,H4,H5,H6]
    end
end


