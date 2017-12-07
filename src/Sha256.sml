
structure Sha256 =
struct
local
  structure C = Sha224And256Core(
                    structure I = Sha256Init
                    structure F = Sha224And256Func)
  structure H = MkSha2(C)
in
  open H
end
end

