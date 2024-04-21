signature SQLITE3 = sig
    (* The aim is to mirror closely the C interface *)
    type db;
    type sqliteErrorCode;
    datatype value = SqlInt of int
                   | SqlInt64 of int
                   | SqlDouble of real
                   | SqlText of string
                   | SqlNull;

    val openDb : string -> (sqliteErrorCode * db option);
    val close : db -> sqliteErrorCode;
    val prepare : (db * string) -> sqliteErrorCode;
    val step : db -> sqliteErrorCode;
    val finalize : db -> sqliteErrorCode;
    val bind : (db * value list) -> bool;
    val bindParameterCount : db -> int; 
end

structure Sqlite = struct

open Foreign
val libsqlite3 = loadLibrary "libsqlite3.so";
val sym = getSymbol libsqlite3;

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

val c_openDb = buildCall2 (sym "sqlite3_open", (cString, cStar cPointer), cInt);
val c_close = buildCall1 (sym "sqlite3_close", cPointer, cInt);
val c_prepare = buildCall6 (sym "sqlite3_prepare_v3",
                            (cPointer, cString, cInt, cUint, cStar cPointer, cStar cPointer),
                            cInt);

val c_step = buildCall1 (sym "sqlite3_step", (cPointer), cInt);
val c_finalize = buildCall1 (sym "sqlite3_finalize", (cPointer), cInt);
val c_columnCount = buildCall1 (sym "sqlite3_column_count", (cPointer), cInt);
val c_columnType = buildCall2 (sym "sqlite3_column_type", (cPointer, cInt), cSqliteType);

val c_bindParameterCount = buildCall1 (sym "sqlite3_bind_parameter_count", (cPointer), cInt);
val c_bindInt = buildCall3 (sym "sqlite3_bind_int", (cPointer, cInt, cInt32), cInt);
val c_bindInt64 = buildCall3 (sym "sqlite3_bind_int64", (cPointer, cInt, cInt64), cInt);
val c_bindDouble = buildCall3 (sym "sqlite3_bind_double", (cPointer, cInt, cDouble), cInt);
val c_bindText = buildCall5 (sym "sqlite3_bind_text", (cPointer,cInt,cString,cInt,cInt), cInt);
val c_bindNull = buildCall2 (sym "sqlite3_bind_null", (cPointer, cInt), cInt);

val c_columnBlob = buildCall2 (sym "sqlite3_column_blob", (cPointer, cInt), cPointer);
val c_columnDouble = buildCall2 (sym "sqlite3_column_double", (cPointer, cInt), cDouble);
val c_columnInt = buildCall2 (sym "sqlite3_column_int", (cPointer, cInt), cInt);
val c_columnInt64 = buildCall2 (sym "sqlite3_column_int64", (cPointer, cInt), cInt64);
val c_columnText = buildCall2 (sym "sqlite3_column_text", (cPointer, cInt), cPointer);
val c_columnBytes = buildCall2 (sym "sqlite3_column_bytes", (cPointer, cInt), cInt);
val c_columnValue = buildCall2 (sym "sqlite3_column_value", (cPointer, cInt), cPointer); (* sqlvalue *)


type stmt = Memory.voidStar ref;


(* Signature interface below *)

type sqliteErrorCode = int;
type db = {dbHandle: Memory.voidStar ref, stmt: stmt, values: Memory.voidStar ref list}
datatype value = SqlInt of int
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
fun bindValue (stmt : stmt, idx: int, value: value) : sqliteErrorCode =
    case value of
        SqlInt i => c_bindInt(!stmt, idx, i)
      | SqlInt64 i => c_bindInt64(!stmt, idx, i)
      | SqlDouble f => c_bindDouble(!stmt, idx, f)
      | SqlText s => c_bindText(!stmt, idx, s, ~1 (* up to NUL *), ~1 (* Transient *))
      | SqlNull => c_bindNull(!stmt, idx);

fun bind (db : db, values : value list) : bool =
    (* todo check that parameter count and len(values) is the same *)
    let val iota = List.tabulate(length values, fn i => i+1)
        val zipped = ListPair.zip(iota, values);
        val res = map (fn (idx,v) => bindValue(#stmt db, idx, v)) zipped
    in
        List.all (fn r => r = 0) res
    end

end

structure Sqlite3 : SQLITE3 = Sqlite
