
structure Sha256 =
struct
local
  structure H = MkSha224And256(Sha256Init)
  structure R = Reader
in
  datatype t = datatype H.t

  fun fromEntity h = h

  fun scan getw strm =
    R.fmap fromEntity (H.scan getw) strm

  val hash = H.hash
  val hash_vector = H.hash_vector

end (* local *)
end

