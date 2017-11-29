
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


