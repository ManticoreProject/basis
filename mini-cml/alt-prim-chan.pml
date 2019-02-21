(* prim-chan.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Primitive-CML channels.
 *
 * This is an alternative implementation that uses an imperative representation of
 * the channel queues.
 *)

#include "spin-lock.def"

structure PrimChan (*: sig

    type 'a chan

    val new : unit -> 'a chan

    val send : ('a chan * 'a) -> unit
    val recv : 'a chan -> 'a

(*
    val sendEvt : ('a chan * 'a) -> unit PrimEvent.pevent
    val recvEvt : 'a chan -> 'a PrimEvent.pevent
*)

  end*) = struct

    structure PT = PrimTypes
    structure PEvt = PrimEvent

    _primcode (

      #ifdef DIRECT_STYLE

            extern void* NewStack(void *, void *) __attribute__((alloc));

      #endif

      (* the representation of a CML thread suspended on a channel *)
        typedef sendq_item = ![
            PEvt.event_state,                (* 0: event-instance status flag *)
            any,                        (* 1: message *)
            vproc,                        (* 2: vproc affinity *)
            FLS.fls,                        (* 3: FLS of thread *)
            cont(unit),                        (* 4: thread's continuation *)
            any                                (* 5: link field *)
          ];

        typedef recvq_item = ![
            PEvt.event_state,                (* 0: event-instance status flag *)
            vproc,                        (* 1: vproc affinity *)
            FLS.fls,                        (* 2: FLS of thread *)
            cont(any),                        (* 3: thread's continuation *)
            any                                (* 4: link field *)
          ];

        typedef chan_rep = ![            (* all fields are mutable *)
            int,                        (* spin lock *)
            sendq_item,                        (* sendq head item *)
            sendq_item,                        (* sendq tail item *)
            recvq_item,                        (* recvq head item *)
            recvq_item                        (* recvq tail item *)
          ];

        (* offsets into the chan_rep object *)
#        define CH_LOCK            0
#        define CH_SENDQ_HD        1
#        define CH_SENDQ_TL        2
#        define CH_RECVQ_HD        3
#        define CH_RECVQ_TL        4

        (* offsets in sendq items *)
#        define SENDQ_MSG        1
#        define SENDQ_VPROC      2
#        define SENDQ_FLS        3
#        define SENDQ_CONT       4
#        define SENDQ_LINK       5

        (* offsets in recvq items *)
#        define RECVQ_VPROC      1
#        define RECVQ_FLS        2
#        define RECVQ_CONT       3
#        define RECVQ_LINK       4

#        define Q_NIL        enum(0)

      (* does a channel have waiting receivers? *)
        define inline @chan-waiting-recv (ch : chan_rep) : bool =
            if NotEqual(SELECT(CH_RECVQ_HD, ch), Q_NIL) then return (true) else return (false)
          ;

      (* does a channel have waiting senders? *)
        define inline @chan-waiting-send (ch : chan_rep) : bool =
            if NotEqual(SELECT(CH_SENDQ_HD, ch), Q_NIL) then return (true) else return (false)
          ;

      (* enqueue an item on a channel's recv queue *)
        define inline @chan-enqueue-recv (ch : chan_rep, flg : PEvt.event_state, vp : vproc, fls : FLS.fls, k : cont(any)) : () =
            let item : recvq_item = alloc (flg, vp, fls, k, Q_NIL)
            let item : recvq_item = promote (item)
            let tl : recvq_item = SELECT(CH_RECVQ_TL, ch)
            if Equal(tl, Q_NIL)
              then
                do UPDATE(CH_RECVQ_HD, ch, item)
                do UPDATE(CH_RECVQ_TL, ch, item)
                return ()
              else
                do UPDATE(RECVQ_LINK, tl, (any)item)
                do UPDATE(CH_RECVQ_TL, ch, item)
                return ()
          ;

      (* enqueue an item on a channel's send queue *)
        define inline @chan-enqueue-send (ch : chan_rep, flg : PEvt.event_state, msg : any, vp : vproc, fls : FLS.fls, k : cont(any)) : () =
            let item : sendq_item = alloc (flg, msg, vp, fls, k, Q_NIL)
            let item : sendq_item = promote (item)
            let tl : sendq_item = SELECT(CH_SENDQ_TL, ch)
            if Equal(tl, Q_NIL)
              then
                do UPDATE(CH_SENDQ_HD, ch, item)
                do UPDATE(CH_SENDQ_TL, ch, item)
                return ()
              else
                do UPDATE(SENDQ_LINK, tl, (any)item)
                do UPDATE(CH_SENDQ_TL, ch, item)
                return ()
          ;

      (* dequeue an item from a channel's recv queue; returns Q_NIL if queue is empty *)
        define inline @chan-dequeue-recv (ch : chan_rep) : recvq_item =
            let hd : recvq_item = SELECT(CH_RECVQ_HD, ch)
            if Equal(hd, Q_NIL)
              then
                return ((recvq_item)Q_NIL)
              else
                let next : recvq_item = SELECT(RECVQ_LINK, hd)
                do UPDATE(CH_RECVQ_HD, ch, next)
                if Equal(next, Q_NIL)
                  then
                    do UPDATE(CH_RECVQ_TL, ch, next)
                    return (hd)
                  else return (hd)
          ;

      (* dequeue an item from a channel's send queue; returns Q_NIL if queue is empty *)
        define inline @chan-dequeue-send (ch : chan_rep) : sendq_item =
            let hd : sendq_item = SELECT(CH_SENDQ_HD, ch)
            if Equal(hd, Q_NIL)
              then
                return ((sendq_item)Q_NIL)
              else
                let next : sendq_item = SELECT(SENDQ_LINK, hd)
                do UPDATE(CH_SENDQ_HD, ch, next)
                if Equal(next, Q_NIL)
                  then
                    do UPDATE(CH_SENDQ_TL, ch, next)
                    return (hd)
                  else return (hd)
          ;

      (***** Channel operations *****)

        define inline constr @chan-new (arg : unit / exh : exh) : chan_rep =
            let ch : chan_rep = alloc(0, (sendq_item)Q_NIL, (sendq_item)Q_NIL, (recvq_item)Q_NIL, (recvq_item)Q_NIL)
            let ch : chan_rep = promote (ch)
            return (ch)
          ;

        define @chan-recv (ch : chan_rep / exh : exh) : any =
            let self : vproc = SchedulerAction.@atomic-begin ()
            SPIN_LOCK(ch, CH_LOCK)
          (* a loop to try to get an item from the sendq *)
            fun tryLp () : any =
                  let item : sendq_item = @chan-dequeue-send (ch)
                  (* in *)
                    if NotEqual(item, Q_NIL)
                      then
                        let state : PEvt.event_state = #0(item)
                        if Equal(DEREF(state), PEvt.SYNCHED)
                          then apply tryLp()
                          else
                          (* there is a matching send, but we must
                           * check to make sure that some other
                           * thread has not already claimed the event.
                           *)
                            fun matchLp () : any =
                                  let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
                                  case sts
                                   of PEvt.WAITING => (* we matched the send! *)
                                        SPIN_UNLOCK(ch, CH_LOCK)
                                        do Threads.@enqueue-ready-in-atomic (
                                            self, SELECT(SENDQ_VPROC, item),
                                            SELECT(SENDQ_FLS, item),
                                            SELECT(SENDQ_CONT, item))
                                        let msg : any = SELECT(SENDQ_MSG, item)
                                        do SchedulerAction.@atomic-end (self)
                                        (* in *)
                                          return (msg)
                                    | PEvt.CLAIMED => (* may be claimed, so spin *)
                                        do Pause()
                                        apply matchLp()
                                    | PEvt.SYNCHED => (* some other thread got it *)
                                        apply tryLp()
                                  end
                            (* in *)
                              apply matchLp ()
                      else
                        fun doRecv (recvK : cont(any)) : unit =
                          let fls : FLS.fls = FLS.@get-in-atomic(self)
                          let flg : PEvt.event_state = alloc(PEvt.WAITING)
                          do @chan-enqueue-recv (ch, flg, self, fls, recvK)
                          SPIN_UNLOCK(ch, CH_LOCK)
                          (* in *)
                            do SchedulerAction.@stop-from-atomic(self)
                            return (UNIT) (* unreachable *)
                        (* in *)
                          cont recvK (x : any) = return (x)
                          (* in *)
                          #ifdef DIRECT_STYLE
                            (* need to perform `doRecv` on a diff stack *)
                            let diffStack : cont(cont(any)) = ccall NewStack (self, doRecv)
                            throw diffStack (recvK)
                          #else
                            (* immutable stack version *)
                            apply doRecv (recvK)
                          #endif

            (* in *)
              apply tryLp ()
          ;

        define @chan-send (arg : [chan_rep, any] / exh : exh) : unit =
            let ch : chan_rep = #0(arg)
            let msg : any = #1(arg)
            let self : vproc = SchedulerAction.@atomic-begin ()
            SPIN_LOCK(ch, CH_LOCK)
            fun tryLp () : unit =
                  let item : recvq_item = @chan-dequeue-recv(ch)
                  (* in *)
                    if NotEqual(item, Q_NIL)
                      then
                        let state : PEvt.event_state = #0(item)
                        if Equal(DEREF(state), PEvt.SYNCHED)
                          then apply tryLp()
                          else
                          (* there is a matching recv, but we must
                           * check to make sure that some other
                           * thread has not already claimed the event.
                           *)
                            fun matchLp () : unit =
                                  let sts : PEvt.event_status = CAS(&0(state), PEvt.WAITING, PEvt.SYNCHED)
                                  case sts
                                   of PEvt.WAITING => (* we matched the recv! *)
                                        SPIN_UNLOCK(ch, CH_LOCK)
                                        if Equal(self, SELECT(RECVQ_VPROC, item))
                                          then (* sending to a local thread *)
                                            cont sendK (_ : unit) = return (UNIT)
                                            (* in *)
                                              let fls : FLS.fls = FLS.@get-in-atomic(self)
                                              do VProcQueue.@enqueue-in-atomic (self, fls, sendK)
                                              do FLS.@set-in-atomic(self, SELECT(RECVQ_FLS, item))
                                              do SchedulerAction.@atomic-end (self)
                                              let k : cont(any) = SELECT(RECVQ_CONT, item)
                                              (* in *)
                                                throw k (msg)
                                          else (* sending to a remote thread *)
                                            do SchedulerAction.@atomic-end (self)
                                            let k : cont(any) = SELECT(RECVQ_CONT, item)

                                          #ifdef DIRECT_STYLE
                                            (* the throw to k must happen on a different stack *)
                                            fun invokeRecvCont (_ : unit) : unit =
                                              throw k (msg)

                                            let recvk : cont(unit) = ccall NewStack(self, invokeRecvCont)
                                          #else
                                            (* immutable stack version *)
                                            cont recvk (_ : unit) = throw k (msg)
                                          #endif
                                            (* in *)
                                              do VProcQueue.@enqueue-on-vproc (
                                                    SELECT(RECVQ_VPROC, item), SELECT(RECVQ_FLS, item),
                                                    recvk)
                                              return (UNIT)
                                    | PEvt.CLAIMED => (* may be claimed, so spin *)
                                        do Pause()
                                        apply matchLp()
                                    | PEvt.SYNCHED => (* some other thread got it *)
                                        apply tryLp()
                                  end
                            (* in *)
                              apply matchLp ()
                        else
                          fun doSend (sendK : cont(unit)) : unit =
                            let fls : FLS.fls = FLS.@get-in-atomic(self)
                            let flg : PEvt.event_state = alloc(PEvt.WAITING)
                            do @chan-enqueue-send (ch, flg, msg, self, fls, sendK)
                            SPIN_UNLOCK(ch, CH_LOCK)
                            (* in *)
                              do SchedulerAction.@stop-from-atomic(self)
                              return (UNIT) (* unreachable *)
                          (* in *)
                            cont sendK (_ : unit) = return (UNIT)
                            (* in *)
                          #ifdef DIRECT_STYLE
                              (* we need to perform `doSend` on a new stack *)
                              let diffStack : cont(cont(unit)) = ccall NewStack (self, doSend)
                              throw diffStack (sendK) (* abandons current stack *)
                          #else
                              (* immutable stack version *)
                              apply doSend (sendK)
                          #endif

                          (* in *)

              (* in *)
                apply tryLp ()
        ;

      )

    type 'a chan = _prim (chan_rep)

    val new : unit -> 'a chan                = _prim (@chan-new)
    val send : ('a chan * 'a) -> unit        = _prim (@chan-send)
    val recv : 'a chan -> 'a                = _prim (@chan-recv)
  end
