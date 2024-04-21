local
  exception TestOK of string * string;
  exception TestErr of string * string;
  type raises_OK_or_ERR = unit
in
infixr 2 == != =/=
type testresult = (string * int);
datatype tcase =
   T of (unit -> raises_OK_or_ERR)
 | IT of (string * (unit -> raises_OK_or_ERR))

fun It desc tcase = IT(desc, tcase)

fun succeed (msg : string) : raises_OK_or_ERR =
    raise TestOK (msg, msg)

fun fail (msg : string) : raises_OK_or_ERR =
    raise TestErr (msg, "~explicit fail~")

fun (left : ''a) == (right : ''a) : raises_OK_or_ERR =
  ignore(if left = right
         then raise TestOK (PolyML.makestring left, PolyML.makestring right)
         else raise TestErr (PolyML.makestring left, PolyML.makestring right))

fun (left : ''a) =/= (right : ''a) : raises_OK_or_ERR =
  ignore(if left = right
         then raise TestErr (PolyML.makestring left, PolyML.makestring right)
         else raise TestOK (PolyML.makestring left, PolyML.makestring right))

fun (expected : exn) != (f : (unit -> 'z)) : raises_OK_or_ERR =
  (ignore(ignore(f()) handle e => let val (exp, got) = (exnMessage expected, exnMessage e)
                                     in if exp = got
                                        then raise TestOK (exp, got)
                                        else raise TestErr (exp, got)
                                     end);
   (* we ran left() without any errors, which we expected. This is a failure *)
   raise TestErr (exnMessage expected, "(ran successfully)"))

fun ppExn (e : exn) : string =
    let val loc = PolyML.exceptionLocation e;
        val locmsg =
            case loc of
                SOME l  => [#file l, ":", Int.toString (#startLine l)]
              | NONE => ["<unknown location>"]
    in concat (["exception ", exnMessage e, " at: "] @ locmsg)
    end

fun runTest (T f : tcase) : testresult = runTest (IT ("", f))
  | runTest ((IT (desc,f)) : tcase) : testresult =
    let fun fmt (result, data) = String.concat([result, " ", desc, "\n\t", data])
    in
      (f();             (fmt ("ERROR", "~no assertion~"), 1))
      handle
       TestOK(a,b) =>   (fmt ("OK",  a^" # "^b), 0)
     | TestErr(a,b) => (fmt ("FAILED", a^" # "^b),  1)
     | exn =>          (fmt("ERROR", ppExn exn), 1)
    end

end (* local *)

fun runTests (tests : tcase list) =
let
  val results = map runTest tests;
  val errors = List.filter (fn (_, n) => n = 1) results;
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
        app (p o #1) errors;
        p ("\n\nFAILED " ^ error_ratio ^ "\n");
        OS.Process.exit(OS.Process.failure))
end;

exception TestExn of string;

fun main() =
    runTests
    (* nice tests *)
    (* [ T(fn() => 3 + 3 == 6), T(fn() => {a=1} == {a=1})];  *)
    (* failing tests *)
     [
      T(fn () => 3 + 2 == 4),
      T(fn()=> 3 + 3 == ~1),
      T(fn()=> {a = "a", b=2} == {a = "A", b=2}),
      T(fn()=> Subscript != (fn()=> tl(tl[1]))),
      T(fn()=> Empty != (fn()=> hd [1])),
      T(fn()=> ignore(2 + 2)),
      T(fn()=> TestExn("hello") != (fn()=> raise TestExn("hello"))), (*passes*)
      T(fn()=> TestExn("hellop") != (fn()=> raise TestExn("hello"))), (*fails*)
      T(fn()=> fail("this should never happen"))
     ]
