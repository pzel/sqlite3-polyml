use "assert.sml";
use "sql.sml";

fun freshName () : string =
    concat [ "tmp/"
           , (LargeInt.toString o Time.toNanoseconds) (Time.now())
           , ".sqlite"];


val openCloseTests = [
    It "can open a db file" (
        fn _ =>
           let val (res, db) = Sqlite.openDb(freshName())
           in res == 0
           end),

    It "can close a db file" (
        fn _ =>
           let val (0, SOME db) = Sqlite.openDb(freshName())
               val res = Sqlite.close(db)
           in res == 0
           end)
];

val statementTests = [

    It "can prepare a statement" (
        fn _ =>
           let val (0, SOME db) = Sqlite.openDb(freshName());
               val res = Sqlite.prepare(db, "create table t (v int)")
           in res == 0
           end),

    It "can step a statement and get 101 when its done" (
        fn _ =>
           let val (0, SOME db) = Sqlite.openDb(freshName());
               val 0  = Sqlite.prepare(db, "create table t (v int)")
               val res = Sqlite.step(db)
           in res == 101
           end),

    It "can finalize a statement" (
        fn _ =>
           let val (0, SOME db) = Sqlite.openDb(freshName());
               val 0 = Sqlite.prepare(db, "create table t (v int)")
               val 101 = Sqlite.step(db);
               val res = Sqlite.finalize(db);
           in res == 0
           end)

];


fun givenTable (ddl : string) =
    let val (0, SOME db) = Sqlite.openDb(freshName());
        val 0 = Sqlite.prepare(db, ddl)
        val 101 = Sqlite.step(db);
        val 0 = Sqlite.finalize(db);
    in db
    end;


structure S = Sqlite

val bindTests = [
    It "can get a bind parameter count" (
        fn _ =>
           let val db = givenTable "create table t (i int, j int)";
               val res = Sqlite.prepare(db, "insert into t values (?,?)")
               val count = Sqlite.bindParameterCount(db)
           in count == 2
           end),

    It "can bind two integer values" (
        fn _ =>
           let val db = givenTable "create table t (i int, j int)";
               val res = Sqlite.prepare(db, "insert into t values (?,?)")
               val res = Sqlite.bind(db, [S.SqlInt 0, S.SqlInt 3])
           in res == true
           end),

    It "can bind int64 values" (
        fn _ =>
           let val db = givenTable "create table t (i int, j int)";
               val res = Sqlite.prepare(db, "insert into t values (?,?)")
               val res = Sqlite.bind(db, [S.SqlInt64 0, S.SqlInt64 3])
           in res == true
           end),

    It "can bind double values" (
        fn _ =>
           let val db = givenTable "create table t (p double, q double)";
               val res = Sqlite.prepare(db, "insert into t values (?,?)")
               val res = Sqlite.bind(db, [S.SqlDouble 0.0, S.SqlDouble 30.0])
           in res == true
           end),

    It "can bind text values" (
        fn _ =>
           let val db = givenTable "create table t (t text, u text, v text)";
               val res = Sqlite.prepare(db, "insert into t values (?,?,?)")
               val res = Sqlite.bind(db, [S.SqlText "a", S.SqlText "b", S.SqlText "c"])
           in res == true
           end),

    It "can bind NULL values" (
        fn _ =>
           let val db = givenTable "create table t (a int, b double, c text)";
               val res = Sqlite.prepare(db, "insert into t values (?,?,?)")
               val res = Sqlite.bind(db, [S.SqlNull, S.SqlNull, S.SqlNull])
           in res == true
           end)



]


fun main () =
  runTests (openCloseTests @ statementTests @ bindTests)
