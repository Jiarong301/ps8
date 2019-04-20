(* 
                Mutable points with vector arithmetic
                          CS51 Problem Set 8
                         -.-. ... ..... .----
 *)

class point (x0 : float) (y0 : float) =   
	object (this)
	val mutable x = x0
	val mutable y = y0

	(* Accessing aspects of the point *)
    method x : float = x
    method y : float = y 

    (* pos -- Returns the position of the point *)
    method pos : float * float = (x, y)
           
    (* round -- Returns the position of the point with coordinates
       rounded to the nearest int *)
    method round : int * int = (int_of_float (x +. 0.5), int_of_float (y +. 0.5))

    (* move -- Moves the point destructively by changing the point's
       coordinates to the position of its argument. Thus,

          let new point 1. 2. in
          (point#move new point 3. 4.)#pos

       gives (3., 4.), not (4., 6.) *)
    method move (p : point) : unit = x <- p#x; y <- p#y
    method scale (f: float): point = new point (x *. f) (y *. f)
    method plus  (p : point) : point = new point (p#x +. x) (y +. p#y)
    method minus (p : point) : point = new point (x -. p#x) (y -. p#y)
    method norm : float = sqrt (x*.x +. y*.y)
    method distance (p : point): float = sqrt ((x -. p#x)*.(x -. p#x) +. (y -. p#y)*.(y -. p#y))                     
    method unit_vector: point = this#scale (1. /. (this#norm))

end


   
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) each part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 30 ;;
