use "assert.sml";
use "sql.sml";

val openCloseTests = [
  It "can open a db file" (fn _ =>
      Sqlite.openDb("foo.db") == 0
   )

];



fun main () =
  runTests openCloseTests