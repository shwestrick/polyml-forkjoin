# polyml-forkjoin
Nested fork-join parallelism in [Poly/ML](https://github.com/polyml/polyml).

```sml
structure ForkJoin:
sig
  (** Initialize the scheduler with the given number of threads. This should
    * be called exactly once at program startup.
    *)
  val initialize: int -> unit

  (** Run two functions in parallel and get their results. *)
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b

  (** A parallel for-loop. `parfor g (i, j) f` executes f(k) in parallel
    * for every k between i and j (inclusive i, exclusive j). The parameter
    * `g` is for granularity control: the loop is split up into approximately
    * (j-i)/g subranges each of size at most g, and each subrange is processed
    * sequentially.
    *)
  val parfor: int -> (int * int) -> (int -> unit) -> unit

  (** UNSAFE!! Allocate a new array of the given length. Intended for use
    * in high-performance libraries.
    *)
  val alloc: int -> 'a array
end
```

To use it in your project:
```
use "src/Deque.sml";
use "src/SimpleRandom.sml";
use "src/Scheduler.sml";
use "src/ForkJoin.sml";
```

## Example

For example, parallel Fibonacci with granularity control (switches to
sequential implementation at input size 20 and smaller).
```sml
fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun fib n =
  if n <= 20 then sfib n else
  let
    val (a, b) = ForkJoin.par (fn _ => fib (n-1), fn _ => fib (n-2))
  in
    a + b
  end
```

See `examples/` for more.
