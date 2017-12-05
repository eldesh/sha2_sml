
structure Sha224 =
struct
local
  structure H = MkSha224And256(val bit = 224)
  structure R = Reader
in

  datatype t = Hash of H.word * H.word * H.word * H.word
                     * H.word * H.word * H.word

  fun fromEntity (H.Hash(a,b,c,d,e,f,g,_)) =
    Hash(a,b,c,d,e,f,g)

  fun scan getw strm =
    R.fmap fromEntity (H.scan getw) strm

  val hash = fromEntity o H.hash
  val hash_vector = fromEntity o H.hash_vector

end (* local *)
end

