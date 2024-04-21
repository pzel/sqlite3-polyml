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

val c_step = buildCall1 (sym "sqlite3_step", (cPointer), cInt);
val c_finalize = buildCall1 (sym "sqlite3_finalize", (cPointer), cInt);
val c_columnCount = buildCall1 (sym "sqlite3_column_count", (cPointer), cInt);
val c_columnType = buildCall2 (sym "sqlite3_column_type", (cPointer, cInt), cSqliteType);

val c_bindParameterCount = buildCall1 (sym "sqlite3_bind_parameter_count", (cPointer), cInt);
val c_bindInt = buildCall3 (sym "sqlite3_bind_int", (cPointer, cInt, cInt32), cInt);
val c_bindInt64 = buildCall3 (sym "sqlite3_bind_int64", (cPointer, cInt, cInt64), cInt);
val c_bindDouble = buildCall3 (sym "sqlite3_bind_double", (cPointer, cInt, cDouble), cInt);


(*
The fifth argument to the BLOB and string binding interfaces controls or
indicates the lifetime of the object referenced by the third parameter. These
three options exist:
(1) A destructor to dispose of the BLOB or string after
SQLite has finished with it may be passed. It is called to dispose of the BLOB
or string even if the call to the bind API fails, except the destructor is not
called if the third parameter is a NULL pointer or the fourth parameter is
negative.
(2) The special constant, SQLITE_STATIC, may be passed to indicate
that the application remains responsible for disposing of the object. In this
case, the object and the provided pointer to it must remain valid until either
the prepared statement is finalized or the same SQL parameter is bound to
something else, whichever occurs sooner.
(3) The constant, SQLITE_TRANSIENT,
may be passed to indicate that the object is to be copied prior to the return
from sqlite3_bind_*(). The object and pointer to it must remain valid until
then. SQLite will then manage the lifetime of its private copy. val

*)
val c_bindText = buildCall5 (sym "sqlite3_bind_text", (cPointer,cInt,cString,cInt,cInt), cInt);
val c_bindNull = buildCall2 (sym "sqlite3_bind_null", (cPointer, cInt), cInt);


val c_columnBlob = buildCall2 (sym "sqlite3_column_blob", (cPointer, cInt), cPointer);
val c_columnDouble = buildCall2 (sym "sqlite3_column_double", (cPointer, cInt), cDouble);
val c_columnInt = buildCall2 (sym "sqlite3_column_int", (cPointer, cInt), cInt);
val c_columnInt64 = buildCall2 (sym "sqlite3_column_int64", (cPointer, cInt), cInt64);
val c_columnText = buildCall2 (sym "sqlite3_column_text", (cPointer, cInt), cPointer);
val c_columnBytes = buildCall2 (sym "sqlite3_column_bytes", (cPointer, cInt), cInt);
val c_columnValue = buildCall2 (sym "sqlite3_column_value", (cPointer, cInt), cPointer); (* sqlvalue *)


type 'a array = 'a Array.array;

type stmt = Memory.voidStar ref;
type db = {dbHandle: Memory.voidStar ref, stmt: stmt, values: Memory.voidStar ref list}


type sqliteErrorCode = int;

in

structure Sqlite = struct
  (* Value s stored in sqlite3_value objects can be integers, floating point
    values, strings, BLOBs, or NULL.
  *)
  datatype Value = SqlInt of int
                 | SqlInt64 of int
                 | SqlDouble of real
                 | SqlText of string
                 | SqlNull

  fun openDb (filename : string) : (sqliteErrorCode * db option) =
      let val dbH = ref (Memory.malloc 0w0);
          val res = c_openDb (filename, dbH);
      in if res = 0
         then (res, SOME {dbHandle=dbH, stmt=ref (Memory.malloc 0w0), values=[]})
         else (res, NONE) before Memory.free (!dbH)
      end

  fun close ({dbHandle,...} : db) : sqliteErrorCode =
      c_close (!dbHandle);

  fun prepare ({dbHandle,stmt,...} : db, input : string) : sqliteErrorCode =
      c_prepare(!dbHandle, input, ~1, 0, stmt, ref Memory.null);

  fun step ({stmt,...} : db) : sqliteErrorCode = c_step(!stmt);

  fun finalize ({stmt,...} : db) : sqliteErrorCode = c_finalize(!stmt);

  fun bindParameterCount ({stmt,...} : db) : int =
      c_bindParameterCount(!stmt);

  (* private, let's use opaque modules for this later *)
  fun bindValue (stmt : stmt, idx: int, value: Value) : sqliteErrorCode =
      case value of
          SqlInt i => c_bindInt(!stmt, idx, i)
        | SqlInt64 i => c_bindInt64(!stmt, idx, i)
        | SqlDouble f => c_bindDouble(!stmt, idx, f)
        | SqlText s => c_bindText(!stmt, idx, s, ~1, ~1)
        | SqlNull => c_bindNull(!stmt, idx);

  fun bind (db : db, values : Value list) : bool =
      (* todo check that parameter count and len(values) is the same *)
      let val iota = List.tabulate(length values, fn i => i+1)
          val zipped = ListPair.zip(iota, values);
          val res = map (fn (idx,v) => bindValue(#stmt db, idx, v)) zipped
      in
          List.all (fn r => r = 0) res
      end



end

end (* local *)


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
