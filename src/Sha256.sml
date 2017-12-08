
structure Sha256 =
struct
local
  structure H = MkSha2(MkSha2Core(
                    structure I = Sha256Init
                    structure F = Sha224And256Func))
in
  open H
end
end

