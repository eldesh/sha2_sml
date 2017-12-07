
functor Sha2Type(W : WORD) :> SHA2TYPE where type Word.word = W.word =
struct
  structure Word = W

  datatype t = Hash of Word.word * Word.word * Word.word * Word.word
                     * Word.word * Word.word * Word.word * Word.word

  fun toString (Hash(h0,h1,h2,h3,h4,h5,h6,h7)) =
    let fun tos w = StringCvt.padLeft #"0" 16 (Word.toString w) in
      foldl (fn(w,s)=> s ^ tos w ^ " ") "" [h0,h1,h2,h3,h4,h5,h6,h7]
    end
end

