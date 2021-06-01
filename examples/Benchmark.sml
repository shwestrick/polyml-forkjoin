(** Copyright (c) 2021 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure Benchmark =
struct

  fun getTimes msg n f =
    let
      fun loop tms n =
        let
          val (result, tm) = Util.getTime f
        in
          print (msg ^ " " ^ Time.fmt 4 tm ^ "s\n");

          if n <= 1 then
            (result, List.rev (tm :: tms))
          else
            loop (tm :: tms) (n-1)
        end
    in
      loop [] n
    end

  fun initialize () =
    if ForkJoin.isInitialized () then () else
    let
      val nprocs = CommandLineArgs.parseInt "procs" 1
      val _ = print ("procs " ^ Int.toString nprocs ^ "\n")
    in
      ForkJoin.initialize nprocs
    end

  fun stats () =
    let
      val x = PolyML.Statistics.getLocalStats ()
      fun pi msg i = print (msg ^ " " ^ Int.toString i ^ "\n")
      fun pt msg t = print (msg ^ " " ^ Time.fmt 4 t ^ "\n")
    in
      pt "gc-real    " (#timeGCReal x);
      pt "gc-user    " (#timeGCUser x);
      pt "gc-sys     " (#timeGCSystem x);
      pt "nongc-real " (#timeNonGCReal x);
      pt "nongc-user " (#timeNonGCUser x);
      pt "nongc-sys  " (#timeNonGCSystem x);
      pi "full-gcs   " (#gcFullGCs x);
      pi "part-gcs   " (#gcPartialGCs x)
    end

  fun run msg f =
    let
      val _ = initialize ()
      val warmup = Time.fromReal (CommandLineArgs.parseReal "warmup" 0.0)
      val rep = CommandLineArgs.parseInt "repeat" 1
      val _ =
        if rep >= 1 then ()
        else Util.die "-repeat N must be at least 1"
      val _ = print ("warmup " ^ Time.fmt 4 warmup ^ "\n")
      val _ = print ("repeat " ^ Int.toString rep ^ "\n")

      fun warmupLoop startTime =
        if Time.>= (Time.- (Time.now (), startTime), warmup) then
          () (* warmup done! *)
        else
          let
            val (_, tm) = Util.getTime f
          in
            print ("warmup_run " ^ Time.fmt 4 tm ^ "s\n");
            warmupLoop startTime
          end

      val _ =
        if Time.<= (warmup, Time.zeroTime) then ()
        else ( print ("====== WARMUP ======\n" ^ msg ^ "\n")
             ; warmupLoop (Time.now ())
             ; print ("==== END WARMUP ====\n")
             )

      val _ = print (msg ^ "\n")
      val t0 = Time.now ()
      val (result, tms) = getTimes "time" rep f
      val t1 = Time.now ()
      val endToEnd = Time.- (t1, t0)

      val total = List.foldl Time.+ Time.zeroTime tms
      val avg = Time.toReal total / (Real.fromInt rep)
    in
      print ("average " ^ Real.fmt (StringCvt.FIX (SOME 4)) avg ^ "s\n");
      print ("total   " ^ Time.fmt 4 total ^ "s\n");
      print ("end-to-end " ^ Time.fmt 4 endToEnd ^ "s\n");
      stats ();
      result
    end

end
