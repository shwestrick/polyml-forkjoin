(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Scheduler:
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val initialize: int -> unit
end =
struct

  structure Mutex = Thread.Mutex
  structure ConditionVar = Thread.ConditionVar
  structure Thread = Thread.Thread  (** lol nice *)

  (** =======================================================================
    * Parse the number of threads from the command line
    *)

  val defaultNumThreads = 1
  val maxNumThreads = 256
  val numThreads = ref defaultNumThreads
  fun napTime() =
    Time.fromNanoseconds (LargeInt.fromInt (!numThreads * 100))
  val originalThread = ref (Thread.self ())

  (** =======================================================================
    * Tasks and results
    *)

  datatype task = Task of unit -> unit

  datatype 'a result =
    Finished of 'a
  | Raised of exn

  fun extractResult rr =
    case rr of
      Finished x => x
    | Raised e => raise e

  fun result (f: unit -> 'a): 'a result =
    Finished (f ()) handle e => Raised e

  fun newTask (f: unit -> 'a, r : 'a result option ref) =
    Task (fn _ => r := SOME (result f))

  (** =======================================================================
    * Deques and thread-local stuff
    *)

  val deques: task Deque.t vector =
    Vector.tabulate (maxNumThreads, fn _ => Deque.new ())
  val rands: SimpleRandom.t vector =
    Vector.tabulate (maxNumThreads, SimpleRandom.rand)

  fun popDiscard deq =
    case Deque.popBot deq of
      SOME _ => true
    | NONE => false

  val threadIdTag = Universal.tag ()

  fun randomOtherId myId =
    let
      val x =
        SimpleRandom.boundedInt (0, !numThreads-1) (Vector.sub (rands, myId))
    in
      if x < myId then x else x+1
    end

  (** Try to do steal cycles elsewhere until resultRef is ready *)
  fun doWorkUntilDone myId resultRef =
    let
      fun stealLoop attempts =
        case !resultRef of
          SOME result => result
        | NONE =>
            if not (Thread.isActive (!originalThread)) then
              ( Thread.exit ()
              ; raise Fail "Impossible"
              )
            else if attempts >= !numThreads * 100 then
              (** Take a little break and let other processors steal for a
                * moment before trying again.
                *)
              (OS.Process.sleep (napTime()); stealLoop 0)
            else
              let
                val friend = randomOtherId myId
              in
                case Deque.popTop (Vector.sub (deques, friend)) of
                  NONE => stealLoop (attempts+1)
                | SOME (Task t) => (t (); stealLoop 0)
              end
    in
      stealLoop 0
    end


  fun dopar myId deq (f, g) =
    let
      val grr = ref NONE
      val _ = Deque.pushBot (deq, newTask (g, grr))

      val fr = result f

      val gr =
        if popDiscard deq then
          result g
        else
          doWorkUntilDone myId grr
    in
      (extractResult fr, extractResult gr)
    end


  fun par (f: unit -> 'a, g: unit -> 'b) =
    let
      val myId: int = Option.valOf (Thread.getLocal threadIdTag)
      val deq = Vector.sub (deques, myId)
    in
      if Deque.isFull deq then
        (f (), g ())
      else
        dopar myId deq (f, g)
    end


  fun threadFunc myId =
    let
      (* val _ = print ("Hello from thread " ^ Int.toString myId ^ "\n") *)
      val _ = Thread.setLocal (threadIdTag, myId)

      fun stealLoop attempts =
        if not (Thread.isActive (!originalThread)) then
          ( Thread.exit ()
          ; raise Fail "Impossible"
          )
        else if attempts >= !numThreads * 100 then
          (** Take a little break and let other processors steal for a
            * moment before trying again.
            *)
          (OS.Process.sleep (napTime ()); stealLoop 0)
        else
          let
            val friend = randomOtherId myId
          in
            case Deque.popTop (Vector.sub (deques, friend)) of
              NONE => stealLoop (attempts+1)
            | SOME (Task t) => (t (); stealLoop 0)
          end
    in
      stealLoop 0
    end

  fun initialize n =
    if n <= 0 then
      raise Fail "negative number of threads"
    else if n > maxNumThreads then
      raise Fail "too many threads (reconfigure scheduler)"
    else
      let
        val _ = print ("initializing with " ^ Int.toString n ^ " threads\n")

        fun spawnThreads i =
          if i >= n then ()
          else
            ( Thread.fork (fn _ => threadFunc i, [])
            ; spawnThreads (i+1)
            )
      in
        originalThread := Thread.self ();
        Thread.setLocal (threadIdTag, 0);
        numThreads := n;
        spawnThreads 1
      end

end
