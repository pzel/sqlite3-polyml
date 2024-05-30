signature ASSERT = sig
  type testresult = (string * bool);
  type tcase;
  type raisesTestExn;
  val It : string -> (unit -> raisesTestExn) -> tcase;
  val T : (unit -> raisesTestExn) -> tcase;
  val succeed : string -> raisesTestExn;
  val fail : string -> raisesTestExn;
  val == : (''a * ''a) -> raisesTestExn;
  val =/= : (''a * ''a) -> raisesTestExn;
  val != : (exn * (unit -> 'z)) -> raisesTestExn;
  val =?= : (''a * ''a) -> ''a;
  val runTest : tcase -> testresult;
  val runTests : tcase list -> unit;
end


structure Assert = struct

exception TestOK of string * string;
exception TestErr of string * string;
type raisesTestExn = unit;
datatype raisesTestExn = Never of unit;
infixr 2 == != =/= =?=;

fun N (a: 'a) : raisesTestExn = Never (ignore a);

type testresult = (string * bool);
datatype tcase = TC of (string * (unit -> raisesTestExn))

fun It desc tcase = TC(desc, tcase)
fun T tcase = TC("", tcase)

fun succeed (msg : string) : raisesTestExn =
    raise TestOK (msg, msg)

fun fail (msg : string) : raisesTestExn =
    raise TestErr (msg, "~explicit fail~")

fun (left : ''a) == (right : ''a) : raisesTestExn =
    N(if left = right
           then raise TestOK (PolyML.makestring left, PolyML.makestring right)
           else raise TestErr (PolyML.makestring left, PolyML.makestring right))

fun (left : ''a) =/= (right : ''a) : raisesTestExn =
    N(if left = right
           then raise TestErr (PolyML.makestring left, PolyML.makestring right)
           else raise TestOK (PolyML.makestring left, PolyML.makestring right))

fun (expected : exn) != (f : (unit -> 'z)) : raisesTestExn =
    (N(ignore(f())
            handle e => let val (exp, got) = (exnMessage expected, exnMessage e);
                        in if exp = got
                           then raise TestOK (exp, got)
                           else raise TestErr (exp, got)
                        end);
     (* We ran left() without any errors, even though we expected them.
        This makes the current test case a failure. *)
     raise TestErr (exnMessage expected, "~ran successfully~"))

fun (left : ''a) =?= (right : ''a) : ''a =
    if left = right
    then left
    else raise (TestErr (PolyML.makestring left, PolyML.makestring right))



fun runTest ((TC (desc,f)) : tcase) : testresult =
    let fun fmt (result, data) = String.concat([result, " ", desc, "\n\t", data, "\n"]);
        fun ppExn (e : exn) : string =
            let val loc = PolyML.exceptionLocation e;
                val locmsg =
                    case loc of
                        SOME l  => [#file l, ":", Int.toString (#startLine l)]
                      | NONE => ["<unknown location>"]
            in concat (["exception ", exnMessage e, " at: "] @ locmsg)
            end;
    in
                       (* this outcome is likely unreachable now that raisesTestExn is opaque *)
      ( f ();             (fmt ("ERROR", "~no assertion in test body~"), false))
      handle
      TestOK(a,b) =>   (fmt ("OK",  a^" = "^b), true)
      | TestErr(a,b) => (fmt ("FAILED", a^" <> "^b),  false)
      | exn =>          (fmt("ERROR", ppExn exn), false)
    end

fun runTests (tests : tcase list) =
    let
      val results = map runTest tests;
      val errors = List.filter (fn (_, n) => not n) results;
      val successes = List.filter (fn (_, n) => n) results;
      val error_count = length errors;
      val test_count = length results;
      val p = fn s => ignore(print (s ^"\n"));
      val i = Int.toString;
      val error_ratio = concat [i error_count, "/", i test_count];
      val success_ratio = concat [i test_count, "/", i test_count]
    in
      if error_count = 0
      then p ("ALL TESTS PASSED: " ^ success_ratio)
      else (p "";
            (* app (p o #1) successes; *) (* TODO: make this optional *)
            app (p o #1) errors;
            p ("\nTESTS FAILED: " ^ error_ratio ^ "\n");
            OS.Process.exit(OS.Process.failure))
    end

end : ASSERT
