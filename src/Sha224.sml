
structure Sha224 =
struct
local
  structure C = Sha224And256Core(
                    structure I = Sha224Init
                    structure F = Sha224And256Func)

  datatype 'a h = Hash of 'a * 'a * 'a * 'a
                        * 'a * 'a * 'a

  fun fromEntity (Sha2Type.Hash(h0,h1,h2,h3,h4,h5,h6,_)) =
    Hash(h0,h1,h2,h3,h4,h5,h6)

  structure H = MkSha2(
  struct
    open C
    type 'a t = 'a h
    fun scan getw = Reader.fmap fromEntity (C.scan getw)
  end)
in
  open H
end
end

