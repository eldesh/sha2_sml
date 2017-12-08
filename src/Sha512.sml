
structure Sha512 =
struct
  structure H = MkSha2(MkSha2Core(
                    structure I = Sha512Init
                    structure F = Sha384And512Func))
  open H
end

