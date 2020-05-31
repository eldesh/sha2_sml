
val _ = PolyML.make "src";
val _ = PolyML.make "test/Sha2Test";

fun main' () = ignore
  (Sha2Test.main (CommandLine.name(), CommandLine.arguments()))

val _ = PolyML.export ("sha2test-poly", main')


