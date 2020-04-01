(* cvar.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Condition variables, which are write-once unit-values synchronous memory
 * cells.
 *)

#include "debug.def"
#include "spin-lock.def"

structure CVar (*: sig

    type cvar

    val new : unit -> cvar
    val signal : cvar -> unit
    val waitEvt : cvar -> unit pevent
    val wait : cvar -> unit

  end*) = struct

    structure PT = PrimTypes
    structure PEvt = PrimEvent

    _primcode (

      #ifdef DIRECT_STYLE

            extern void* NewStack(void *, void *) __attribute__((alloc));

      #endif

        typedef waiter = [
            PrimEvent.event_state,        (* event-instance status flag *)
            vproc,                        (* vproc affinity *)
            FLS.fls,                        (* FLS of thread *)
            PT.fiber                        (* thread's continuation *)
          ];
        typedef cvar = ![
            int,                        (* spinlock *)
            bool,                        (* state *)
            List.list                        (* list of waiters *)
          ];

(* the fields of a cvar *)
#define CV_LOCK      0
#define CV_STATE     1
#define CV_WAITING   2

      (* create a new signal variable *)
        define inline @cvar-new (_ : unit / _ : exh) : cvar =
            let cv : cvar = alloc(0, false, nil)
            let cv : cvar = promote (cv)
            return (cv)
          ;

      (* signal the variable, which wakes up any threads that are waiting on it. *)
        define @cvar-signal (cv : cvar / _ : exh) : unit =
            let self : vproc = SchedulerAction.@atomic-begin ()
            SPIN_LOCK(cv, CV_LOCK)
              do UPDATE(CV_STATE, cv, true)
              let waiting : list = SELECT(CV_WAITING, cv)
              do UPDATE(CV_WAITING, cv, nil)
            SPIN_UNLOCK(cv, CV_LOCK)
            do SchedulerAction.@atomic-end (self)
          (* loop over the list of waiting threads waking them *)
            fun signalWaiting (l : list) : unit =
                  case l
                   of nil => return (UNIT)
                    | CONS(hd : waiter, tl : List.list) =>
                        let flg : PEvt.event_state = #0(hd)
                        let sts : PEvt.event_status = CAS(&0(flg), PEvt.WAITING, PEvt.SYNCHED)
                        do if Equal(sts, PEvt.WAITING)
                            then  (* enqueue the waiting thread *)
                              Threads.@enqueue-ready-in-atomic (self, #1(hd), #2(hd), #3(hd))
                            else return()
                        apply signalWaiting (tl)
                  end
            (* in *)
            apply signalWaiting (waiting)
          ;

      (* wait for a variable to be signaled *)
        define @cvar-wait (cv : cvar / _ : exh) : unit =
            if (SELECT(CV_STATE, cv))
            then return (UNIT)
            else
              (* slow-path requires waiting *)
                let self : vproc = SchedulerAction.@atomic-begin ()
                SPIN_LOCK(cv, CV_LOCK)
                case SELECT(CV_STATE, cv)
                 of true =>
                    SPIN_UNLOCK(cv, CV_LOCK)
                    do SchedulerAction.@atomic-end (self)
                    return (UNIT)
                  | false =>
                    fun enterScheduler (syncK : cont(unit)) : unit =
                      let flg : PEvt.event_state = alloc (PEvt.WAITING)
                      let fls : FLS.fls = FLS.@get()
                      let item : waiter = alloc (flg, self, fls, syncK)
                      let l : list = CONS(item, SELECT(CV_WAITING, cv))
                      let l : list = promote (l)
                      do UPDATE(CV_WAITING, cv, l)
                      SPIN_UNLOCK(cv, CV_LOCK)
                      do SchedulerAction.@stop-from-atomic (self)
                      return (UNIT) (* impossible *)

                    cont syncK (_ : unit) = return (UNIT)
                    (* in *)
                  #ifdef DIRECT_STYLE
                    (* need to perform `enterScheduler` on a diff stack *)
                    let diffStack : cont(cont(unit)) = ccall NewStack (self, enterScheduler)
                    throw diffStack (syncK)
                  #else
                    (* immutable stack version *)
                    apply enterScheduler (syncK)
                  #endif
                end
          ;
      )

    type cvar = _prim (cvar)

    val new        : unit -> cvar = _prim(@cvar-new)
    val signal     : cvar -> unit = _prim(@cvar-signal)
    val wait       : cvar -> unit = _prim(@cvar-wait)

  end
