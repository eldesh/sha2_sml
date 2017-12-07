
structure Sha256Init =
struct
  open Sha2Type
  val H0 =
      Hash ( 0wx6a09e667, 0wxbb67ae85, 0wx3c6ef372, 0wxa54ff53a
           , 0wx510e527f, 0wx9b05688c, 0wx1f83d9ab, 0wx5be0cd19 )
end

