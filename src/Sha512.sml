
structure Sha512 =
struct
  structure H = MkSha2(Sha384And512Core(Sha512Init))
  open H
end

