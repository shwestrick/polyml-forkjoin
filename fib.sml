use "CommandLineArgs.sml";
use "Deque.sml";
use "SimpleRandom.sml";
use "Scheduler.sml";

fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun fib n =
  if n <= 20 then sfib n else
  let
    val (a, b) = Scheduler.par (fn _ => fib (n-1), fn _ => fib (n-2))
  in
    a + b
  end

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
    val n = CommandLineArgs.parseInt "N" 39
    val r = CommandLineArgs.parseInt "repeat" 10
    val _ = print ("procs " ^ Int.toString procs ^ "\n")
    val _ = print ("repeat " ^ Int.toString r ^ "\n")
    val _ = print ("N " ^ Int.toString n ^ "\n")

    val _ = Scheduler.initialize procs

    fun loop k =
      let
        val result = reportTime (fn _ => fib n)
      in
        if k > 1 then loop (k-1) else result
      end

    val result = loop r
    val _ = print ("result " ^ Int.toString result ^ "\n")
  in
    ()
  end
