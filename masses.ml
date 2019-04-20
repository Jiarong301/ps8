(*
                       Point masses with forces
                          CS51 Problem Set 8
                         -.-. ... ..... .----
 *)

open Points ;;
     
let cFRAMESIZE = 500 ;;

class mass =
  let currid = ref 0 in

  fun (initx : float) (inity : float) (m : float) ->
    object (this)
           
      (* A mass is located at a point *)
      inherit point initx inity as super
                                     
      (* Unique identifier for the mass *)
      val id = currid := !currid + 1; !currid
      method get_id = id
                        
      (* The mass itself *)
      val mass = m

      (*..............................................................
      Your part goes here: Provide the implementations of the move and
      restore_pos methods. The move method should differ from the
      corresponding method for points in that the coordinates that the
      mass moves to should be "clipped" to stay within a square frame,
      as defined by cFRAMESIZE above. That is, the move will stay
      within 0 and cFRAMESIZE in both dimensions.
      ..............................................................*)

       (*** Movement of the mass ***)
      val mutable oldx = initx
      val mutable oldy = inity

      (* move p -- Moves mass directly to the position of p, storing
         previous position for possible later restoring, clipping to frame
         boundary while we're at it. *)
      method! move  (p : point) : unit = 
        oldx <- this#x; oldy <- this#y;
        let clipped (q : float) = if q <= 0. then 0. else if q > (float_of_int cFRAMESIZE) then (float_of_int cFRAMESIZE)
        else q in super#move (new point (clipped p#x) (clipped p#y))

      (* restore_pos -- Restore position of mass to the single previous stored
         position. Multiple restore points are not supported. That is, if
         you move from position p1 to p2 and then move again to position
         p3, restore_pos will get you back to p2, but restoring again will
         not get you back to p1. Rather, it will return you to p3. *)
      method restore_pos : unit = this#move (new point initx inity)


      (* Forces on the mass *)
      val frc : point = new point 0. 0.    (* accumulator for forces *)

      method set_force (p: point) : unit =
        frc#move p
      method reset_force : unit =
	       this#set_force (new point 0. 0.)
      method get_force = frc
      method add_force p =
        frc#move (frc#plus p)
      method apply_force =
        this#move (this#plus (this#get_force#scale (1. /. mass)));
        this#reset_force
      method scale_force factor =
        this#set_force (this#get_force#scale factor)
                       
      (* I/O methods *)
      method reveal =
        let x, y = this#round in
        Graphics.draw_circle x y 3

      method describe =
        let x, y = this#pos in
        Printf.printf "Mass %d: %f, %f\n%!" this#get_id x y
    end

(*====================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) each part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
....................................................................*)

let minutes_spent_on_part () : int = 50 ;;
