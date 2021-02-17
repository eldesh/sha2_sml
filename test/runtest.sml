
(* dummy structure for executing unit tests *)
structure z = struct
  val _ = Sha2Test.main(CommandLine.name(), CommandLine.arguments())
end
