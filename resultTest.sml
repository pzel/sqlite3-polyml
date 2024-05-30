use "assert.sml";
use "result.sml";

open Assert;
infixr 2 == =?= != =/=;

val resultTests = [
    It "can create an ok value" (
        fn _ => case Result.ok(1) of
                    Result.Ok 1 => succeed "created"
                  | _ => fail "not created"),

    It "can create an error value" (
        fn _ => case Result.error("a") of
                    Result.Error "a" => succeed "created"
                  | _ => fail "not created"),

    It "can get the valOf for an ok datum" (
        fn _ => Result.valOf (Result.ok(1)) 0 == 1),

    It "can get the default valOf for an error datum" (
        fn _ => Result.valOf (Result.error(1)) 0 == 0)

];


fun main () =
  runTests resultTests
