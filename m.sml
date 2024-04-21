(* just some notes on modules *)
signature DOESA = sig
  val a : (int -> int);
end


signature DOESB = sig
  val b : (int -> int);
end



structure As : DOESA = struct
fun a x = x
fun b y = y
end

structure Bs  = struct
fun a x = x+1
fun b y = y+1
end

structure BB : DOESB = Bs;


fun main () =
    (PolyML.print(As.a(1)); print "\n";
     PolyML.print(Bs.b(1)); print "\n";
     PolyML.print(Bs.a(1)); print "\n";
     PolyML.print(BB.b(1)); print "\n";

     print "OK\n");
