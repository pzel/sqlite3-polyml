use "assert.sml";
use "sql.sml";

structure S = Sqlite3
open Sqlite3 (* for error codes *)

fun freshName () : string =
    concat [ "tmp/"
           , (LargeInt.toString o Time.toNanoseconds) (Time.now())
           , ".sqlite"];


val openCloseTests = [
    It "can open a db file" (
        fn _ =>
           let val (res, db) = S.openDb(freshName())
           in res == SQLITE_OK
           end),

    It "can close a db file" (
        fn _ =>
           let val (SQLITE_OK, SOME db) = S.openDb(freshName())
               val res = S.close(db)
           in res == SQLITE_OK
           end),

    It "can't open a file it doesn't own" (
        fn _ =>
           case S.openDb "/dev/mem" of
               (SQLITE_OK, SOME _) => fail "opened bad file"
             | (res, NONE) => res == SQLITE_CANTOPEN)

];

val statementTests = [
    It "can prepare a statement" (
        fn _ =>
           let val (SQLITE_OK, SOME db) = S.openDb(freshName());
               val res = S.prepare(db, "create table t (v int)")
           in res == SQLITE_OK
           end),

    It "can step a statement and get SQLITE_DONE when its done" (
        fn _ =>
           let val (SQLITE_OK, SOME db) = S.openDb(freshName());
               val SQLITE_OK  = S.prepare(db, "create table t (v int)")
               val res = S.step(db)
           in res == SQLITE_DONE
           end),

    It "can finalize a statement" (
        fn _ =>
           let val (SQLITE_OK, SOME db) = S.openDb(freshName());
               val SQLITE_OK = S.prepare(db, "create table t (v int)")
               val SQLITE_DONE = S.step(db);
               val res = S.finalize(db);
           in res == SQLITE_OK
           end)

];


fun givenTable (ddl : string) =
    let val (SQLITE_OK, SOME db) = S.openDb(freshName());
        val SQLITE_OK = S.prepare(db, ddl)
        val SQLITE_DONE = S.step(db);
        val SQLITE_OK = S.finalize(db);
    in db
    end;

val bindTests = [
    It "can get a bind parameter count" (
        fn _ =>
           let val db = givenTable "create table t (i int, j int)";
               val res = S.prepare(db, "insert into t values (?,?)")
               val count = S.bindParameterCount(db)
           in count == 2
           end),

    It "can bind two integer values" (
        fn _ =>
           let val db = givenTable "create table t (i int, j int)";
               val res = S.prepare(db, "insert into t values (?,?)")
               val res = S.bind(db, [S.SqlInt 0, S.SqlInt 3])
           in res == true
           end),

    It "can bind int64 values" (
        fn _ =>
           let val db = givenTable "create table t (i int, j int)";
               val res = S.prepare(db, "insert into t values (?,?)")
               val res = S.bind(db, [S.SqlInt64 0, S.SqlInt64 3])
           in res == true
           end),

    It "can bind double values" (
        fn _ =>
           let val db = givenTable "create table t (p double, q double)";
               val res = S.prepare(db, "insert into t values (?,?)")
               val res = S.bind(db, [S.SqlDouble 0.0, S.SqlDouble 30.0])
           in res == true
           end),

    It "can bind text values" (
        fn _ =>
           let val db = givenTable "create table t (t text, u text, v text)";
               val res = S.prepare(db, "insert into t values (?,?,?)")
               val res = S.bind(db, [S.SqlText "a", S.SqlText "b", S.SqlText "c"])
           in res == true
           end),

    It "can bind NULL values" (
        fn _ =>
           let val db = givenTable "create table t (a int, b double, c text)";
               val res = S.prepare(db, "insert into t values (?,?,?)")
               val res = S.bind(db, [S.SqlNull, S.SqlNull, S.SqlNull])
           in res == true
           end)
]

val stepTests = [
    It "can step through a bound statement" (
        fn _ =>
           let val db = givenTable (concat ["create table t ",
                                            "(a int, b double, c text)"])
               val SQLITE_OK = S.prepare(db, "insert into t values (?,?,?)")
               val true = S.bind(db, [S.SqlInt 3, S.SqlDouble 2.0, S.SqlText "a"])
               val res = S.step(db)
           in res == SQLITE_DONE
           end),

    It "can finalize a stepped-through statement" (
        fn _ =>
           let val db = givenTable (concat ["create table t ",
                                            "(a int, b double, c text)"])
               val SQLITE_OK = S.prepare(db, "insert into t values (?,?,?)")
               val true = S.bind(db, [S.SqlInt 3, S.SqlDouble 2.0, S.SqlText "a"])
               val SQLITE_DONE = S.step(db)
               val res = S.finalize(db)
           in res == SQLITE_OK
           end),

    It "can step as many times as there are rows in result" (
        fn _ =>
           let
               val db = givenTable ("create table t (a int, b double, c text)")
               fun insert (i, d, t) = (
                   S.prepare(db, "insert into t values (?,?,?)");
                   S.bind(db, [S.SqlInt i, S.SqlDouble d, S.SqlText t]);
                   S.step(db));

               val _ = (insert (1,   2.0, "a");
                        insert (10,  20.0, "b");
                        insert (100, 200.0, "c"));
               val SQLITE_OK = S.finalize(db)

               val SQLITE_OK = S.prepare(db, "select * from t");
               val a = S.step(db);
               val b = S.step(db);
               val c = S.step(db);
               val d = S.step(db);

           in (a,b,c,d) == (SQLITE_ROW,SQLITE_ROW,SQLITE_ROW,SQLITE_DONE)
           end
    )

]



val runQueryTests = [
    It "can insert in one go" (
        fn _=>
           let val db = givenTable "create table t (a int, b double, c text)";
               val res = S.runQuery "insert into t values (?,?,?)" [
                       S.SqlInt 1, S.SqlDouble 2.0, S.SqlText "X"] db;
           in length res == 0 end
    ),

    It "can read a row" (
        fn _=>
           let val db = givenTable "create table f (a int, b double, c text)";
               val _ = S.runQuery "insert into f values (?,?,?)" [
                       S.SqlInt 1, S.SqlDouble 2.0, S.SqlText "\226\141\186\226\141\181"] db;
               val res = S.runQuery "select * from f" [] db;
               val (row::[]) = res
           in case row of
                  [S.SqlInt 1, S.SqlDouble _, S.SqlText "\226\141\186\226\141\181"] => succeed "selected"
                | other => fail ("failed:" ^ (PolyML.makestring other))
           end
    )

]


fun main () =
    let val lowLevelTests = openCloseTests @ statementTests @ bindTests @ stepTests
        val highLevelTests = runQueryTests
    in runTests (lowLevelTests @ highLevelTests) end;
