open Assert;
infixr 2 == =?= != =/=;

infix 1 >>=
fun op >>= (v, f) = v >| (Either.bindRight f)

structure S = Sqlite3;

fun freshName () : string =
    concat [ "tmp/"
           , (LargeInt.toString o Time.toNanoseconds) (Time.now())
           , ".sqlite"];

fun givenDb () : S.db =
    case S.openDb(freshName()) of
        (INR db) => db
      | (INL err) => raise (Fail ("givenDb: failed to open db" ^ PolyML.makestring err))

fun givenTable (ddl : string) =
    let val db = givenDb ()
        val res = S.prepare ddl db
    in case res  of
           INR stmt => (ignore (S.step stmt) ; S.finalize stmt; db)
         | INL err => raise (Fail ("givenTable: error" ^ PolyML.makestring err))
    end;


val openCloseTests = [

  It "returns a db when a file can be opened" (
    fn _=>
       case S.openDb (freshName ()) of
           INL e => fail ("Error " ^ PolyML.makestring e)
         | INR _ => succeed "OK"),

  It "returns the when a file cannot be opened" (
    fn _=>
       case S.openDb "/foobar" of
           INL e => e == S.SQLITE_CANTOPEN
         | INR db => fail ("GOT DB: " ^ PolyML.makestring db)),

  It "can close a db file" (
    fn _ =>
       case S.openDb(freshName()) of
           (INR db) => INR S.SQLITE_OK == S.close db
         | (INL err) => fail ("failed to open: " ^ (PolyML.makestring err))),

  It "cannot close a db file multiple times" (
    fn _ =>
       let val db = givenDb ()
       in   [INR S.SQLITE_OK, INL S.SQLITE_MISUSE, INL S.SQLITE_MISUSE] == [
             S.close db, S.close db, S.close db]
       end)

]


fun forceR (e : ('a, 'b) either) : 'b =
    case e of
        INR r => r
      | INL l => raise Fail("EXPECTED INR, GOT INL " ^ (PolyML.makestring l));

fun valToString (v: S.value) : string =
    let open Sqlite3
    in case v of SqlInt i => Int.toString i
               | SqlInt64 i => Int.toString i
               | SqlDouble r => Real.toString r
               | SqlText t => t
               | SqlBlob v => Byte.bytesToString v
               | SqlNull => "NULL"
    end

val statementTests = [
  It "can prepare a statement" (
    fn _ => let val st = S.prepare "create table t (v int)" (givenDb())
            in Either.isRight st == true
            end),

  It "cannot prepare a statement that is invalid SQL" (
    fn _ =>
       let val db = givenDb ()
           val res = S.prepare "hello world" db
       in (Either.asLeft res) == SOME S.SQLITE_ERROR
       end),

  It "can step a statement and get S.SQLITE_DONE when its done" (
    fn _ =>
       let val db = givenDb ()
           val res = S.prepare "create table t (v int)" db
                               >| Either.bindRight S.step
                               >| Either.asRight
       in res == SOME S.SQLITE_DONE
       end),


  It "can finalize a statement" (
    fn _ =>
       let val db = givenDb ()
           val stmt = S.prepare "create table t (v int)" db >| forceR
           val _ = S.step stmt
           val res = S.finalize stmt
       in Either.asRight res == SOME S.SQLITE_OK
       end),

  It "can't finalize a statement twice" (
    fn _ =>
       let val db = givenDb ()
           val stmt = S.prepare "create table t (v int)" db >| forceR
           val _ = S.step stmt
           val _ = true =?= Either.isRight (S.finalize stmt)
           val res = S.finalize stmt
       in Either.asLeft res == SOME S.SQLITE_MISUSE
       end)

];

fun assertSameStmt (x, y) =
    if PolyML.pointerEq(x,y)
    then Assert.succeed("pointers equal")
    else Assert.fail("pointers not equal")

val bindTests = [
  It "can get a bind parameter count" (
    fn _ =>
       let val db = givenTable "create table t (i int, j int)";
           val stmt = S.prepare "insert into t values (?,?)" db >| forceR
           val count = S.bindParameterCount stmt
       in count == 2
       end),

  It "can get a zero parameter count" (
    fn _ =>
       let val db = givenTable "create table t (i int, j int)";
           val stmt = S.prepare "insert into t values (1,2)" db >| forceR
           val count = S.bindParameterCount stmt
       in count == 0
       end),

  It "can bind two integer values" (
    fn _ =>
       let val db = givenTable "create table t (i int, j int)";
           val stmt = S.prepare "insert into t values (?,?)" db >| forceR
           val res = S.bind [S.SqlInt 0, S.SqlInt 3] stmt >| forceR
       in assertSameStmt(res, stmt)
       end),

  It "can bind int64 values" (
    fn _ =>
       let val db = givenTable "create table t (i int, j int)";
           val stmt = S.prepare "insert into t values (?,?)" db >| forceR
           val res = S.bind [S.SqlInt64 0, S.SqlInt64 3] stmt >| forceR
       in assertSameStmt(res, stmt)
       end),

  It "can bind double values" (
    fn _ =>
       let val db = givenTable "create table t (p double, q double)";
           val stmt = S.prepare "insert into t values (?,?)" db >| forceR
           val res = S.bind [S.SqlDouble 0.0, S.SqlDouble 30.0] stmt >| forceR
       in assertSameStmt(res, stmt)
       end),

  It "can bind text values" (
    fn _ =>
       let val db = givenTable "create table t (t text, u text, v text)";
           val stmt = S.prepare "insert into t values (?,?,?)" db >| forceR
           val res = S.bind [S.SqlText "a", S.SqlText "b", S.SqlText "c"] stmt >| forceR
       in assertSameStmt(res, stmt)
       end),

  It "can bind Blob values" (
    fn _ =>
       let val db = givenTable "create table t (b blob)";
           val stmt = S.prepare "insert into t values (?)" db >| forceR
           val vec = Word8Vector.fromList([0w0,0w1,0w2,0w3])
           val res = S.bind [S.SqlBlob vec] stmt >| forceR
       in assertSameStmt(res, stmt)
       end),

  It "can bind NULL values" (
    fn _ =>
       let val db = givenTable "create table t (a int, b double, c text)";
           val stmt = S.prepare "insert into t values (?,?,?)" db >| forceR
           val res = S.bind [S.SqlNull, S.SqlNull, S.SqlNull] stmt >| forceR
       in assertSameStmt(res, stmt)
       end)
]


val stepTests = [
  It "can step a bound statement" (
    fn _ =>
       let val db = givenTable "create table t (a int, b double, c text)";
           val res = S.prepare "insert into t values (?,?,?)" db
                               >>= (S.bind [S.SqlInt 3,
                                            S.SqlDouble 2.0,
                                            S.SqlText "a"])
                               >>= (S.step)
       in forceR res == S.SQLITE_DONE
       end),

  It "can finalize a stepped-through statement" (
    fn _ =>
       let val db = givenTable "create table t (a int, b double, c text)"
           val stmt = S.prepare "insert into t values (?,?,?)" db
                               >| Either.bindRight(S.bind [S.SqlInt 3,
                                                           S.SqlDouble 2.0,
                                                           S.SqlText "a"])
                               >| forceR
           val stepped = S.step stmt >| forceR
           val res = S.finalize stmt >| forceR
       in res == S.SQLITE_OK
       end),

  It "can step as many times as there are rows in result" (
    fn _ =>
       let
         val db = givenTable "create table t (a int, b double, c text)";
         fun insert (i, d, t) =
             (let val stmt = S.prepare "insert into t values (?,?,?)" db
                  val res = stmt >>= S.bind [S.SqlInt i, S.SqlDouble d, S.SqlText t]
                                 >>= S.step
              in Either.mapRight S.finalize stmt
              end)

         val _ = (insert (1,   2.0, "a");
                  insert (10,  20.0, "b");
                  insert (100, 200.0, "c"))

         val select = S.prepare "select * from t" db >| forceR
         val a = S.step select >| forceR;
         val b = S.step select >| forceR;
         val c = S.step select >| forceR;
         val d = S.step select >| forceR;

       in (a,b,c,d) == (S.SQLITE_ROW,S.SQLITE_ROW,S.SQLITE_ROW,S.SQLITE_DONE)
       end
  )

]


val runQueryTests = [
  It "can run query, binding the passed parameters" (
    fn _=>
       let val db = givenTable "create table t (a int, b double, c text)";
           val res = S.runQuery "insert into t values (?,?,?)" [
                 S.SqlInt 1, S.SqlDouble 2.0, S.SqlText "X" ] db
       in length (forceR res) == 0
       end
  ),

  It "will return left if parameters can't be bound" (
    fn _=>
       let val db = givenTable "create table t (a int not null)";
           val res = S.runQuery "insert into t values (?)" [S.SqlNull ] db
       in Either.asLeft res == SOME S.SQLITE_CONSTRAINT
       end
  ),

  It "will return left if parameters don't match formal count" (
    fn _=>
       let val db = givenTable "create table t (a int, b int)";
           val res = S.runQuery "insert into t values (?)" [S.SqlInt 1] db
       in Either.asLeft res == SOME S.SQLITE_ERROR
       end
  ),

  It "can read a row" (
    fn _=>
       let val db = givenTable "create table f (a int, b double, c text)";
           val _ = S.runQuery "insert into f values (?,?,?)"
                              [S.SqlInt 1,
                               S.SqlDouble 2.22,
                               S.SqlText "\226\141\186\226\141\181"]
                              db;
           val res = S.runQuery "select * from f" [] db;
           val _ = 1 =?= length (forceR res)
       in case hd(forceR res) of
              [S.SqlInt 1,
               S.SqlDouble doubleVal,
               S.SqlText "\226\141\186\226\141\181"] => if Real.==(doubleVal, 2.22)
                                                        then succeed "selected"
                                                        else fail "real not matched"
            | other => fail ("failed:" ^ (PolyML.makestring other))
       end
  ),

  It "can execute a sql string directly" (
    fn _=>
       let val db = givenTable "create table f (a int, b text)";
           val _ = S.SQLITE_OK =?= S.execute "insert into f values (1,'a')" db;
           val res = S.runQuery "select * from f" [] db
       in map (map valToString) (forceR res) == [["1", "a"]]
       end
  ),

  It "can execute a sql string directly, returning error if encountered" (
    fn _=>
       let val db = givenTable "create table f (a int, b text)";
           val res = S.execute "insert into f values (1,'a',2)" db;
       in res == S.SQLITE_ERROR
       end
  ),

  It "can read multiple rows with one query" (
    fn _=>
       let val db = givenTable "create table f (a int, b text)";
           val _ = S.SQLITE_OK =?= S.execute "insert into f values (1,'c')" db;
           val _ = S.SQLITE_OK =?= S.execute "insert into f values (2,'b')" db;
           val _ = S.SQLITE_OK =?= S.execute "insert into f values (3,'a')" db;
           val res = S.runQuery "select * from f order by b" [] db
       in map (map valToString) (forceR res) == [["3", "a"], ["2", "b"], ["1", "c"]]
       end
  ),

  It "can read and write unicode literals" (
    fn _=>
       let val db = givenTable "create table f (t text)";
           val _ = S.SQLITE_OK =?= S.execute "insert into f values ('こんにちは')" db;
           val _ = S.SQLITE_OK =?= S.execute "insert into f values ('世界')" db;
           val res = S.runQuery "select * from f" [] db
       in map (map valToString) (forceR res) == [["こんにちは"],["世界"]]
       end
  ),

  It "can bind unicode literals" (
    fn _=>
       let val db = givenTable "create table f (t text)";
           val _ = S.runQuery "insert into f values (?)" [S.SqlText "今"] db;
           val res = S.runQuery "select * from f" [] db
       in map (map valToString) (forceR res) == [["今"]]
       end
  ),

  It "can bind blobs" (
    fn _=>
       let val db = givenTable "create table f (b blob)";
           val v = Word8Vector.tabulate(1024, Word8.fromInt)
           val _ = S.runQuery "insert into f values (?)" [S.SqlBlob v] db;
           val res = S.runQuery "select * from f" [] db
       in case (hd (hd (forceR res))) of
              S.SqlBlob v => succeed "blob made it through"
            | _ => fail "blob got mangled"
       end
  )

];

val memoryTests = [
  It "does not leak memory when using runQuery"
     (fn _=>
         let val db = givenDb ();
             val _ = S.execute "create table b (i int, t text)" db;
             fun newString i = S.SqlText (Int.toString i);
             fun loopStep i =
                 if i = 1024
                 then ()
                 else (S.runQuery "insert into b values (?,?)" [S.SqlInt i, newString i] db;
                       S.runQuery "select * from b limit 10" [] db;
                       loopStep (i+1))
         in (print "START\n";
             loopStep 1;
             print "END\n";
             S.close db;
             succeed "OK")
         end)
]


fun main () =
    let val lowLevelTests = openCloseTests @ statementTests @ bindTests @ stepTests
        val highLevelTests = runQueryTests
    in runTests (lowLevelTests @ highLevelTests)
    end;
