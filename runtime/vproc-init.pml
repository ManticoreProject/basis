(* vproc-init.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Virtual processor initialization.
 *)


structure VProcInit (* :
  sig

    _prim(

    (* bootstrap the vproc for purely-sequential execution *)
      define @bootstrap-sequential ( / exh : exh) : unit;

    (* bootstrap the vprocs
     *   - mkAct is a function that takes a vproc and returns the top-level scheduler
     *     for that vproc.
     *)
      define @bootstrap (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : ();

    )

  end *) = struct

    structure PT = PrimTypes

#include "vproc-queue.def"

    _primcode (


#ifdef DIRECT_STYLE
      
      extern void* NewStack(void *, void *) __attribute__((alloc));

#endif


#ifndef DIRECT_STYLE

    (* bootstrap the vproc for purely-sequential execution *)
      define @bootstrap-sequential ( / exh : exh) : () =
        let vp : vproc = host_vproc
        
        (**** vp->schedCont ****)
        cont schedCont (k : PT.fiber) = 
          throw k(UNIT)
        do vpstore(VP_SCHED_CONT, vp, schedCont)
        
        (**** vp->shutdownCont ****)
        cont shutdownCont (_ : unit) =
          do ccall VProcExit(vp)
          return ()
        let shutdownCont : PT.fiber = promote(shutdownCont)
        do vpstore(VP_SHUTDOWN_CONT, vp, shutdownCont)

        (**** vp->currentFLS ****)
        let fls : FLS.fls = FLS.@new(UNIT / exh)
        let fls : FLS.fls = promote(fls)
        do vpstore(CURRENT_FLS, vp, fls)

        (**** vp->dummyK ****)
        cont dummyK (x : unit) = 
            let _ : unit = SchedulerAction.@stop()
            return()
        let dummyK : PT.fiber = promote(dummyK)
        do vpstore(VP_DUMMYK, vp, dummyK)
        return()
  ;

#else

  (* bootstrap the vproc for purely-sequential execution *)
      define @bootstrap-sequential ( / exh : exh) : () =
        let vp : vproc = host_vproc
        
        (**** vp->schedCont ****)
        fun schedCont (k : PT.fiber) : unit = 
          throw k(UNIT)
        do vpstore(VP_SCHED_CONT, vp, schedCont)
      
        (**** vp->shutdownCont ****)
        fun shutdownCont (_ : unit) : unit =
        do ccall VProcExit(vp)
          return (UNIT)
        let shutdownCont : fun(unit / -> unit) = promote(shutdownCont)
        do vpstore(VP_SHUTDOWN_CONT, vp, shutdownCont)

        (**** vp->currentFLS ****)
        let fls : FLS.fls = FLS.@new(UNIT / exh)
        let fls : FLS.fls = promote(fls)
        do vpstore(CURRENT_FLS, vp, fls)

        (**** vp->dummyK ****)
        fun dummyK (x : unit) : unit = 
            let _ : unit = SchedulerAction.@stop()
            return(UNIT)
        let dummyK : fun(unit / -> unit) = promote(dummyK)
        do vpstore(VP_DUMMYK, vp, dummyK)
        return()
  ;

#endif

    (* bootstrap the vprocs
     *   - mkAct is a function that takes a vproc and returns the top-level scheduler
     *     for that vproc.
     *)
      define @bootstrap (mkAct : fun (vproc / exh -> PT.sched_act) / exh : exh) : () =
          let self : vproc = SchedulerAction.@atomic-begin()
          let nVProcs : int = VProc.@num-vprocs()
          let shutdownCnt : ![int] = alloc(nVProcs)
          let shutdownCnt : ![int] = promote(shutdownCnt)
        (* initialize fields in the vproc structure *)
          fun initVPFields (vp : vproc / exh : exh) : () =

  #ifndef DIRECT_STYLE
              (**** vp->schedCont ****)
            cont schedCont (k : PT.fiber) = 
              do assert(NotEqual(k, nil))
              SchedulerAction.@forward(PT.PREEMPT(k))

            let schedCont : cont(PT.fiber) = promote(schedCont)
            do vpstore(VP_SCHED_CONT, vp, schedCont)

              (**** vp->dummyK ****)
            cont dummyK (x : unit) = 
              let _ : unit = SchedulerAction.@stop()
              return()

            let dummyK : PT.fiber = promote(dummyK)
            do vpstore(VP_DUMMYK, vp, dummyK)

              (**** vp->shutdownCont ****)
            (* Signals must be masked before invoking the shutdown function. *)
            cont shutdownCont (_ : unit) =
              do assert(Equal(vp, host_vproc))
              let cnt : int = I32FetchAndAdd(&0(shutdownCnt), ~1)
              fun wait () : () =
                if I32Eq (#0(shutdownCnt), 0) 
                  then do ccall VProcExit(vp)
                       return ()
                  else do Pause()
                       apply wait()
              apply wait()

            let shutdownCont : PT.fiber = promote(shutdownCont)
            do vpstore(VP_SHUTDOWN_CONT, vp, shutdownCont)

  #else

            fun schedCont (k : PT.fiber) : () = 
              do assert(NotEqual(k, nil))
              do SchedulerAction.@forward(PT.PREEMPT(k))
              return ()

            let schedCont : fun(PT.fiber / -> ) = promote(schedCont)
            do vpstore(VP_SCHED_CONT, vp, schedCont)

              (**** vp->dummyK ****)  
              (* dummyK field is treated as a multi-shot continuation in RTS *)
            fun initDummyK (_ : unit) : unit =
              let dummyK : PT.fiber = ccall NewStack (vp, initDummyK)
              let dummyK : PT.fiber = promote(dummyK)
              do vpstore(VP_DUMMYK, vp, dummyK)
              do SchedulerAction.@stop()
              return (UNIT)

            let dummyK : PT.fiber = ccall NewStack (vp, initDummyK)
            let dummyK : PT.fiber = promote(dummyK)
            do vpstore(VP_DUMMYK, vp, dummyK)

              (**** vp->shutdownCont ****)
            (* Signals must be masked before invoking the shutdown function. *)
            fun shutdownCont (_ : unit) : () =
              do assert(Equal(vp, host_vproc))
              let cnt : int = I32FetchAndAdd(&0(shutdownCnt), ~1)
              fun wait () : () =
                if I32Eq (#0(shutdownCnt), 0) 
                  then do ccall VProcExit(vp)
                       return ()
                  else do Pause()
                       apply wait()
              apply wait()

            let shutdownCont : fun(unit / -> ) = promote(shutdownCont)
            do vpstore(VP_SHUTDOWN_CONT, vp, shutdownCont)

  #endif

              (**** vp->currentFLS ****)
            let fls : FLS.fls = FLS.@new(UNIT / exh)
            let fls : FLS.fls = promote(fls)
            do vpstore(CURRENT_FLS, vp, fls)

              (**** vp->actionStk ****)
            let act : PT.sched_act = apply mkAct (vp / exh)
            let stk : [PT.sched_act, any] = vpload (VP_ACTION_STK, vp)
            let item : [PT.sched_act, any] = alloc (act, (any)stk)
            let item : [PT.sched_act, any] = promote (item)
            do vpstore (VP_ACTION_STK, vp, item)
            return()
          (* end of initVPFields *)

          do VProc.@for-each-vproc(initVPFields / exh)
          cont startLeadK (_ : PT.unit) = 
            return()
          let act : PT.sched_act = apply mkAct (self / exh)
          SchedulerAction.@run(self, act, startLeadK)
      ;

    )

  end
