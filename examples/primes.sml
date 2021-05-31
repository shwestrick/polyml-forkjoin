use "../src/Deque.sml";
use "../src/SimpleRandom.sml";
use "../src/Scheduler.sml";
use "../src/ForkJoin.sml";

use "CommandLineArgs.sml";
use "SeqBasis.sml";
use "Seq.sml";
use "TreeMatrix.sml";

structure CLA = CommandLineArgs

(* primes: int -> int array
 * generate all primes up to (and including) n *)
fun primes n =
  if n < 2 then ForkJoin.alloc 0 else
  let
    (* all primes up to sqrt(n) *)
    val sqrtPrimes = primes (Real.floor (Math.sqrt (Real.fromInt n)))

    (* allocate array of flags to mark primes. *)
    val flags = ForkJoin.alloc (n+1) : Word8.word array
    fun mark i = Array.update (flags, i, 0w0)
    fun unmark i = Array.update (flags, i, 0w1)
    fun isMarked i = Array.sub (flags, i) = 0w0

    (* initially, mark every number *)
    val _ = ForkJoin.parfor 10000 (0, n+1) mark

    (* unmark every multiple of every prime in sqrtPrimes *)
    val _ =
      ForkJoin.parfor 1 (0, Array.length sqrtPrimes) (fn i =>
        let
          val p = Array.sub (sqrtPrimes, i)
          val numMultiples = n div p - 1
        in
          ForkJoin.parfor 4096 (0, numMultiples) (fn j => unmark ((j+2) * p))
        end)
  in
    (* for every i in 2 <= i <= n, filter those that are still marked *)
    SeqBasis.filter 4096 (2, n+1) (fn i => i) isMarked
  end

(* ==========================================================================
 * parse command-line arguments and run
 *)

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
    val procs = CLA.parseInt "procs" 1
    val n = CLA.parseInt "N" (100 * 1000 * 1000)
    val r = CLA.parseInt "repeat" 10
    val _ = print ("procs " ^ Int.toString procs ^ "\n")
    val _ = print ("repeat " ^ Int.toString r ^ "\n")
    val _ = print ("N " ^ Int.toString n ^ "\n")

    val _ = ForkJoin.initialize procs

    fun loop k =
      let
        val result = reportTime (fn _ => primes n)
      in
        if k > 1 then loop (k-1) else result
      end

    val result = loop r
    val numPrimes = Array.length result
    val _ = print ("number of primes " ^ Int.toString numPrimes ^ "\n")
  in
    ()
  end
