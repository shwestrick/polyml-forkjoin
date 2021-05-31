use "../src/Deque.sml";
use "../src/SimpleRandom.sml";
use "../src/Scheduler.sml";
use "../src/ForkJoin.sml";

use "CommandLineArgs.sml";
use "SeqBasis.sml";
use "Seq.sml";
use "TreeMatrix.sml";

fun reportTime f =
  let
    val t0 = Time.now ()
    val result = f ()
    val t1 = Time.now ()
  in
    print ("time " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n");
    result
  end

fun main () =
  let
    val procs = CommandLineArgs.parseInt "procs" 1
    val n = CommandLineArgs.parseInt "N" 1024
    val r = CommandLineArgs.parseInt "repeat" 10
    val _ = print ("procs " ^ Int.toString procs ^ "\n")
    val _ = print ("repeat " ^ Int.toString r ^ "\n")
    val _ = print ("N " ^ Int.toString n ^ "\n")

    val _ = ForkJoin.initialize procs

    val input = TreeMatrix.tabulate n (fn (i, j) => 1.0)

    fun loop k =
      let
        val result = reportTime (fn _ => TreeMatrix.multiply (input, input))
      in
        if k > 1 then loop (k-1) else result
      end

    val result = loop r
    (* val _ = print ("result " ^ Int.toString result ^ "\n") *)
  in
    ()
  end
