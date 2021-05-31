structure ForkJoin:
sig
  val alloc: int -> 'a array
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val parfor: int -> (int * int) -> (int -> unit) -> unit
  val initialize: int -> unit
end =
struct

  open Scheduler

  fun for (lo, hi) f =
    if lo >= hi then () else (f lo; for (lo+1, hi) f)

  fun parfor g (lo, hi) f =
    if hi-lo <= g then
      for (lo, hi) f
    else
      let
        val mid = lo + (hi-lo) div 2
        val _ = par (fn _ => parfor g (lo, mid) f, fn _ => parfor g (mid, hi) f)
      in
        ()
      end

  fun alloc len =
    let
      val vec = RunCall.allocateWordMemory (Word.fromInt len, 0wx40, 0w0)
    in
      RunCall.unsafeCast vec
    end

end
