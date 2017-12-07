
signature SHA2TYPE =
sig
  datatype 'w t = Hash of 'w * 'w * 'w * 'w
                        * 'w * 'w * 'w * 'w

  val toString : ('w -> string) -> 'w t -> string
end

