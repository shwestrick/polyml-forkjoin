(** A simple doubly-ended queue. Reasonable interface, but nothing fancy
  * in the implementation yet.
  *
  * These deques are intended for work stealing, where each deque has one
  * pusher but many poppers. It is NOT CORRECT for multiple threads to call
  * pushBot concurrently (more advanced implementations in the future can
  * exploit this for improved efficiency, e.g. Arora-Blumofe-Plaxton design).
  *)
structure Deque:
sig
  type 'a t

  val new: unit -> 'a t

  (** Depending on the implementation, maybe it resizes, maybe it doesn't.
    * Pushing onto a full deque fails (indicated by returning false).
    *
    * Note that pushBot may NOT be called concurrently with itself.
    *)
  val isFull: 'a t -> bool
  val pushBot: 'a t * 'a -> bool

  (** Popping returns NONE if the deque is empty. *)
  val popBot: 'a t -> 'a option
  val popTop: 'a t -> 'a option
end =
struct

  val DEFAULT_CAPACITY = 100

  type 'a t =
    { top: int ref
    , bot: int ref
    , data: 'a option array
    , lock: Thread.Mutex.mutex
    }


  fun new () =
    { top = ref 0
    , bot = ref 0
    , data = Array.array (DEFAULT_CAPACITY, NONE)
    , lock = Thread.Mutex.mutex ()
    }


  fun isFull ({bot, data, ...}: 'a t) =
    !bot >= Array.length data


  fun pushBot (deq as {top, bot, data, lock}: 'a t, elem) =
    if isFull deq then
      false
    else
      let
        val _ = Thread.Mutex.lock lock
        val idx = !bot
      in
        Array.update (data, idx, SOME elem);
        bot := idx + 1;
        Thread.Mutex.unlock lock;
        true
      end


  fun isEmptyLocked ({top, bot, data, lock}: 'a t) =
    let
      val tidx = !top
      val bidx = !bot
    in
      if bidx > tidx then
        false
      else if bidx = 0 then
        (** it's empty, and also indices are reset *)
        true
      else
        (** it's empty, but we need to reset indices *)
        ( bot := 0
        ; top := 0
        ; true
        )
    end


  fun popBot (deq as {top, bot, data, lock}: 'a t) =
    let
      val _ = Thread.Mutex.lock lock
    in
      if isEmptyLocked deq then
        ( Thread.Mutex.unlock lock
        ; NONE
        )
      else
        let
          val idx = !bot
          val elem = Array.sub (data, idx-1) (** This has to be SOME! *)
        in
          bot := idx - 1;
          Thread.Mutex.unlock lock;
          elem
        end
    end


  fun popTop (deq as {top, bot, data, lock}: 'a t) =
    let
      val _ = Thread.Mutex.lock lock
    in
      if isEmptyLocked deq then
        ( Thread.Mutex.unlock lock
        ; NONE
        )
      else
        let
          val idx = !top
          val elem = Array.sub (data, idx) (** This has to be SOME! *)
        in
          top := idx + 1;
          Thread.Mutex.unlock lock;
          elem
        end
    end

end
