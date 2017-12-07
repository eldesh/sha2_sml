
structure Sha256Init :> SHA2INIT
                        where type Word.word = Word32.word =
struct
  structure Word = Word32

  val H0 : Word.word Sha2Type.t =
      Sha2Type.Hash
        ( 0wx6a09e667, 0wxbb67ae85, 0wx3c6ef372, 0wxa54ff53a
        , 0wx510e527f, 0wx9b05688c, 0wx1f83d9ab, 0wx5be0cd19 )
end

