
structure Sha224Init :> SHA2INIT
                        where type Word.word = Word32.word =
struct
  structure Word = Word32

  val H0 : Word.word Sha2Type.t =
      Sha2Type.Hash
        ( 0wxc1059ed8, 0wx367cd507, 0wx3070dd17, 0wxf70e5939
        , 0wxffc00b31, 0wx68581511, 0wx64f98fa7, 0wxbefa4fa4 )
end

