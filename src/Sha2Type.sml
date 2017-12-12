
structure Sha2Type =
struct
  datatype 'w t = Hash of 'w * 'w * 'w * 'w
                        * 'w * 'w * 'w * 'w
end

