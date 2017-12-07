
structure Sha512 =
struct
local
  structure H = MkSha2(Sha384And512Core(Sha512Init))
in
  open H
end
end

