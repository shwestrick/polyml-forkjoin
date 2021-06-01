(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

PolyML.make "lib";

fun main () =
  let
    val n = CommandLineArgs.parseInt "N" 1024
    val _ = print ("N " ^ Int.toString n ^ "\n")

    val _ = Benchmark.initialize ()

    val input = TreeMatrix.tabulate n (fn (i, j) => 1.0)
    val result =
      Benchmark.run "dmm" (fn _ => TreeMatrix.multiply (input, input))
  in
    ()
  end
