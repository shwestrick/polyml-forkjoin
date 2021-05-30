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

fun parseN () =
  let
    val default = 39
    fun loop args =
      case args of
        [] => default
      | "-N" :: nstr :: args' => Option.valOf (Int.fromString nstr)
      | _ :: args' => loop args'
  in
    loop (CommandLine.arguments ())
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
    val _ = Scheduler.initialize ()

    val n = parseN ()
    val _ = print ("N " ^ Int.toString n ^ "\n")

    fun loop k =
      let
        val result = reportTime (fn _ => fib n)
      in
        if k > 1 then loop (k-1) else result
      end

    val result = loop 40
    val _ = print ("result " ^ Int.toString result ^ "\n")
  in
    ()
  end
