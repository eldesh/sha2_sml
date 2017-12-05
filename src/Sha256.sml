
structure Sha256 =
struct
local
  structure H = MkSha224And256(val bit = 256)
  structure R = Reader
in
  datatype t = datatype H.t

  fun fromEntity h = h

  fun scan getw strm =
    R.fmap fromEntity (H.scan getw) strm

end (* local *)
end

