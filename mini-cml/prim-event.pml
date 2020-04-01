(* prim-event.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PrimEvent = struct

    type 'a cont = _prim (cont(any))

    datatype event_status = WAITING | CLAIMED | SYNCHED

  (* BOM types *)
    _primcode (
	typedef event_status = event_status;
	typedef event_state = ![event_status];
	typedef poll_fn = fun( / -> bool);
	typedef do_fn = fun(vproc, cont(any) / exh -> );
	typedef blk_fn = fun(vproc, event_state, FLS.fls, cont(any) / exh -> );
      )

  end
