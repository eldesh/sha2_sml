
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

