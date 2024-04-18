local

open Foreign

datatype sqliteType =
         SQLITE_INTEGER  (* 1 *)
       | SQLITE_FLOAT    (* 2 *)
       | SQLITE_TEXT     (* 3 *)
       | SQLITE_BLOB     (* 4 *)
       | SQLITE_NULL     (* 5 *)

val cSqliteType : sqliteType conversion = makeConversion
      {ctype = LowLevel.cTypeInt32,
       load = fn (m : Memory.voidStar) =>
                 case Word32.toIntX(Memory.get32(m, 0w0)) of
                     1 => SQLITE_INTEGER
                   | 2 => SQLITE_FLOAT
                   | 3 => SQLITE_TEXT
                   | 4 => SQLITE_BLOB
                   | 5 => SQLITE_NULL
                   | other => raise (Foreign ("Got unknown sqliteType: " ^ Int.toString other)),
       store = fn (m: Memory.voidStar, t: sqliteType) =>
                  let val i = case t of
                                  SQLITE_INTEGER => 1
                                | SQLITE_FLOAT => 2
                                | SQLITE_TEXT => 3
                                | SQLITE_BLOB => 4
                                | SQLITE_NULL => 5
                  in (Memory.set32(m, 0w0, Word32.fromInt(i)); fn _ => ())
                  end}

val libsqlite3 = loadLibrary "libsqlite3.so";
val sym = getSymbol libsqlite3;
val ** = cStar cPointer;

val c_openDb = buildCall2 (sym "sqlite3_open", (cString, **), cInt);
val c_close = buildCall1 (sym "sqlite3_close", cPointer, cInt);
val c_prepare = buildCall6 (sym "sqlite3_prepare_v3", (cPointer, cString, cInt, cUint, **, **), cInt);
val c_bind = buildCall3 (sym "sqlite3_bind_int", (cPointer, cInt, cInt), cInt);
val c_step = buildCall1 (sym "sqlite3_step", (cPointer), cInt);
val c_finalize = buildCall1 (sym "sqlite3_finalize", (cPointer), cInt);
val c_columnCount = buildCall1 (sym "sqlite3_column_count", (cPointer), cInt);
val c_columnType = buildCall2 (sym "sqlite3_column_type", (cPointer, cInt), cSqliteType);

val c_columnBlob = buildCall2 (sym "sqlite3_column_blob", (cPointer, cInt), cPointer);
val c_columnDouble = buildCall2 (sym "sqlite3_column_double", (cPointer, cInt), cDouble);
val c_columnInt = buildCall2 (sym "sqlite3_column_int", (cPointer, cInt), cInt);
val c_columnInt64 = buildCall2 (sym "sqlite3_column_int64", (cPointer, cInt), cInt64);
val c_columnText = buildCall2 (sym "sqlite3_column_text", (cPointer, cInt), cPointer);
val c_columnBytes = buildCall2 (sym "sqlite3_column_bytes", (cPointer, cInt), cInt);
val c_columnValue = buildCall2 (sym "sqlite3_column_value", (cPointer, cInt), cPointer); (* sqlvalue *)


type 'a array = 'a Array.array;

in

structure Sqlite = struct
  fun openDb (filename : string) =
  let val dbH = ref (Memory.malloc 0w1)
  in c_openDb ("hello.sql", dbH)
  end
  end
end



(*
fun main () =
    let
        val dbH = ref (Memory.malloc 0w1);
        val initRes = c_openDb ("hello.sql", dbH)
        val stmtString = "insert into a values (?)";
        val stmt = ref (Memory.malloc 0w1);
        val pzTail = ref (Memory.malloc 0w1);
        val r0 = c_prepare(!dbH, stmtString, ~1, 0, stmt, pzTail);
        val _ = print ("r0=" ^ (Int.toString r0 ^ "\n"));
        val r1 = c_bind(!stmt, 1, 12);
        val _ = print ("r1=" ^ (Int.toString r1 ^ "\n"));
        val r2 = c_step(!stmt);
        val _ = print ("r2=" ^ (Int.toString r2 ^ " should be 101 \n"));
        val r3 = c_finalize(!stmt);
        val _ = print ("r3=" ^ (Int.toString r3 ^ "\n"));
        val stmtString1 = "select * from a limit 1";
        val r4 = c_prepare(!dbH, stmtString1, ~1, 0, stmt, pzTail);
        val _ = print ("r4=" ^ (Int.toString r3 ^ "\n"));
        val r5 = c_step(!stmt);
        val _ = print ("r5=" ^ (Int.toString r5 ^ " \n"));
        val r6 = if r5 = 100 then c_columnCount (!stmt) else ~1
        val _ = print ("r6=" ^ (Int.toString r6 ^ " \n"));
        val r7 = c_columnType (!stmt, r6-1);
        val _ = print ("r7=" ^ (PolyML.makestring r7 ^ " \n"));

        val r8 = c_columnInt (!stmt, r6-1);
        val _ = print ("r8=" ^ (PolyML.makestring r8 ^ " \n"));

        val closeRes = c_close (!dbH);
    in
      print (concat [
                     "init=",PolyML.makestring initRes, " ",
                     (* "exec=",PolyML.makestring execRes, " ", *)
                     "close=",PolyML.makestring closeRes, " ",
                     "\n"])
    end
*)