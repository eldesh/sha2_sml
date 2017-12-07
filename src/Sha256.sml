
structure Sha256 =
struct
local
  structure H = MkSha2(Sha224And256Core(Sha256Init))
in
  open H
end
end

