
structure Sha2Type =
struct
local
  fun bind  NONE    _ = NONE
    | bind (SOME x) f = f x
in
  datatype 'w t = Hash of 'w * 'w * 'w * 'w
                        * 'w * 'w * 'w * 'w
end
end

