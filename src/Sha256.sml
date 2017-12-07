
structure Sha256 =
struct
local
  structure C = Sha224And256Core(Sha256Init)
  structure H = MkSha224And256(C)
in
  open H
end
end

