(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Scheduler:
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val initialize: int -> unit
  val isInitialized: unit -> bool
end =
struct

  fun die msg =
    ( TextIO.output (TextIO.stdErr, msg ^ "\n")
    ; TextIO.flushOut TextIO.stdErr
    ; OS.Process.exit OS.Process.failure
    )

  structure Mutex = Thread.Mutex
  structure ConditionVar = Thread.ConditionVar
  structure Thread = Thread.Thread  (** lol nice *)

  (** =======================================================================
    * Some global data. numThreads is set by `initialize`
    *)

  val isInitializedFlag = ref false
  val initLock = Mutex.mutex ()

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

  (** =======================================================================
    * Fancy CSS
    *)

  structure CSS:
  sig
    val incSurplus: int -> unit
    val decSurplus: int -> unit
    val sampleMightHaveSurplus: int -> int option
    val sleep: int -> unit
  end =
  struct

    datatype proc_state = S of
      { surplus: int ref
      , asleep: bool ref
      , cvar: ConditionVar.conditionVar
      , lock: Mutex.mutex
      }

    datatype global_state = G of
      { numSurplus: int ref
      , numSleep: int ref
      , lock: Mutex.mutex
      }

    val globalState: global_state =
      G {numSurplus = ref 0, numSleep = ref 0, lock = Mutex.mutex ()}

    val states: proc_state vector =
      Vector.tabulate (maxNumThreads, fn _ =>
        S { surplus = ref 0
          , asleep = ref false
          , cvar = ConditionVar.conditionVar ()
          , lock = Mutex.mutex ()
          })

    fun sleep myId =
      let
        val S {surplus, asleep, cvar, lock} = Vector.sub (states, myId)
        val G {numSurplus, numSleep, lock=glock} = globalState
      in
        Mutex.lock lock;
        Mutex.lock glock;
        if !numSurplus <> 0 then
          (* don't sleep!! *)
          ( Mutex.unlock glock
          ; Mutex.unlock lock
          )
        else
          ( numSleep := !numSleep+1
          ; Mutex.unlock glock
          ; asleep := true
          (* releases lock and waits for signal *)
          ; print ("[" ^ Int.toString myId ^ "] going to sleep\n")
          ; ConditionVar.wait (cvar, lock)
          ; print ("[" ^ Int.toString myId ^ "] woke up\n")
          )
      end


    fun tryClaimSleeper () =
      let
        val n = !numThreads
        val myId: int =
          Option.valOf (Thread.getLocal threadIdTag)
          handle Option => die "failed to get ID"

        fun loop id =
          if id >= n then NONE
          else if id = myId then
            loop (id+1)
          else
          let
            val S {surplus, asleep, cvar, lock} = Vector.sub (states, id)
          in
            if not (!asleep) orelse not (Mutex.trylock lock) then
              loop (id+1)
            else if not (!asleep) then
              (Mutex.unlock lock; loop (id+1))
            else
              SOME id
          end
      in
        loop 0
      end


    (* should be holding the lock for id *)
    fun finishWakeSleeper id =
      let
        val S {asleep, cvar, lock, ...} = Vector.sub (states, id)

        val myId: int =
          Option.valOf (Thread.getLocal threadIdTag)
          handle Option => die "failed to get ID"

        (** could be CAS *)
        val isAsleep = !asleep;
        val _ = if isAsleep then asleep := false else ()
        val _ = Mutex.unlock lock;
      in
        if not isAsleep then () else
          ( print ("[" ^ Int.toString myId ^ "] signalling " ^ Int.toString id ^ "\n")
          ; ConditionVar.signal cvar
          )
      end


    fun decSurplus id =
      let
        val S {surplus, asleep, cvar, lock} = Vector.sub (states, id)

        (** could be a fetch-and-add *)
        val _ = Mutex.lock lock
        val olds = !surplus
        val _ = surplus := olds-1
        val _ = Mutex.unlock lock

        val zeroSurplusTransition = (olds = 1)
      in
        if not zeroSurplusTransition then () else
        let
          val G {numSurplus, lock=glock, ...} = globalState
        in
          Mutex.lock glock;
          numSurplus := !numSurplus - 1;
          Mutex.unlock glock
        end
      end


    fun incSurplus id =
      let
        val S {surplus, asleep, cvar, lock} = Vector.sub (states, id)

        (** could be a fetch-and-add *)
        val _ = Mutex.lock lock
        val olds = !surplus
        val _ = surplus := olds+1
        val _ = Mutex.unlock lock

        val nonzeroSurplusTransition = (olds = 0)
      in
        if not nonzeroSurplusTransition then () else

        (** TODO: this is wrong. need to announce surplus first,
          * and THEN try to wake someone?
          *)
        let
          (* takes the lock of a sleeper, if it can find one *)
          val sleeperToWake = tryClaimSleeper ()
          val G {numSurplus, numSleep, lock=glock} = globalState
        in
          Mutex.lock glock;

          numSurplus := !numSurplus + 1;

          if not (Option.isSome sleeperToWake) then ()
          else numSleep := !numSleep - 1;

          Mutex.unlock glock;

          case sleeperToWake of
            NONE => ()
          | SOME sleeperId => finishWakeSleeper sleeperId
        end
      end

    fun sampleMightHaveSurplus myId =
      let
        val n = !numThreads

        fun loop attempts =
          if attempts > 100 then
            NONE
          else
          let
            val id = randomOtherId myId
            val S {surplus, asleep, cvar, lock} = Vector.sub (states, id)
          in
            if not (Mutex.trylock lock) then
              loop (attempts+1)
            else if not (!asleep) andalso !surplus > 0 then
              ( print ("[" ^ Int.toString myId ^ "] proc " ^ Int.toString id ^ " might have surplus (" ^ Int.toString (!surplus) ^ ")\n")
              ; Mutex.unlock lock
              ; SOME id
              )
            else
              ( Mutex.unlock lock
              ; loop (attempts+1)
              )
          end
      in
        loop 0
      end

  end

  (** =======================================================================
    * Scheduler loops
    *)

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
              case CSS.sampleMightHaveSurplus myId of
                NONE => stealLoop (attempts+1)
              | SOME friend =>
                  case Deque.popTop (Vector.sub (deques, friend)) of
                    NONE =>
                      ( print ("[" ^ Int.toString myId ^ "] failed to steal from " ^ Int.toString friend ^ "\n")
                      ; stealLoop (attempts+1)
                      )
                  | SOME (Task t) =>
                      ( CSS.decSurplus friend
                      ; print ("[" ^ Int.toString myId ^ "] sucessfully stole from " ^ Int.toString friend ^ "\n")
                      ; t ()
                      ; stealLoop 0
                      )
    in
      stealLoop 0
    end


  fun dopar myId deq (f, g) =
    let
      val grr = ref NONE
      val _ = CSS.incSurplus myId
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
      val myId: int =
        Option.valOf (Thread.getLocal threadIdTag)
        handle Option => die "failed to get ID"

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
          (CSS.sleep myId; stealLoop 0)
        else
          case CSS.sampleMightHaveSurplus myId of
            NONE => stealLoop (attempts+1)
          | SOME friend =>
              case Deque.popTop (Vector.sub (deques, friend)) of
                NONE =>
                  ( print ("[" ^ Int.toString myId ^ "] failed to steal from " ^ Int.toString friend ^ "\n")
                  ; stealLoop (attempts+1)
                  )
              | SOME (Task t) =>
                  ( CSS.decSurplus friend
                  ; print ("[" ^ Int.toString myId ^ "] sucessfully stole from " ^ Int.toString friend ^ "\n")
                  ; t ()
                  ; stealLoop 0
                  )
    in
      stealLoop 0
    end

  fun isInitialized () =
    !isInitializedFlag

  fun initialize n =
    if n <= 0 then
      raise Fail "negative number of threads"
    else if n > maxNumThreads then
      raise Fail "too many threads (reconfigure scheduler)"
    else
      let
        val _ = Mutex.lock initLock
        val _ =
          if not (!isInitializedFlag) then () else
          ( TextIO.output (TextIO.stdErr, "[ERR] scheduler already initialized\n")
          ; TextIO.flushOut TextIO.stdErr
          ; OS.Process.exit OS.Process.failure
          )
        val _ =
          print ("initializing scheduler with " ^ Int.toString n ^ " threads\n")

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
        spawnThreads 1;
        isInitializedFlag := true;
        Mutex.unlock initLock
      end

end
