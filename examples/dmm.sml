(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

use "../src/Deque.sml";
use "../src/SimpleRandom.sml";
use "../src/Scheduler.sml";
use "../src/ForkJoin.sml";

use "CommandLineArgs.sml";
use "SeqBasis.sml";
use "Seq.sml";
use "TreeMatrix.sml";
use "Util.sml";
use "Benchmark.sml";

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
