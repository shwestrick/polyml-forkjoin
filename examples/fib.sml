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

fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun fib n =
  if n <= 20 then sfib n else
  let
    val (a, b) = ForkJoin.par (fn _ => fib (n-1), fn _ => fib (n-2))
  in
    a + b
  end

fun main () =
  let
    val n = CommandLineArgs.parseInt "N" 39
    val _ = print ("N " ^ Int.toString n ^ "\n")
    val result = Benchmark.run "fib" (fn _ => fib n)
    val _ = print ("result " ^ Int.toString result ^ "\n")
  in
    ()
  end
