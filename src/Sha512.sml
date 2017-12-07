
structure Sha512 =
struct
  structure C = Sha384And512Core(
                    structure I = Sha512Init
                    structure F = Sha384And512Func)
  structure H = MkSha2(C)
  open H
end

