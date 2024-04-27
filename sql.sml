use "sqliteCodes.sml";

signature SQLITE3 = sig
    (* The aim is to mirror closely the C interface *)
    type db;
    datatype sqliteResultCode =
             SQLITE_OK
           | SQLITE_ERROR
           | SQLITE_INTERNAL
           | SQLITE_PERM
           | SQLITE_ABORT
           | SQLITE_BUSY
           | SQLITE_LOCKED
           | SQLITE_NOMEM
           | SQLITE_READONLY
           | SQLITE_INTERRUPT
           | SQLITE_IOERR
           | SQLITE_CORRUPT
           | SQLITE_NOTFOUND
           | SQLITE_FULL
           | SQLITE_CANTOPEN
           | SQLITE_PROTOCOL
           | SQLITE_EMPTY
           | SQLITE_SCHEMA
           | SQLITE_TOOBIG
           | SQLITE_CONSTRAINT
           | SQLITE_MISMATCH
           | SQLITE_MISUSE
           | SQLITE_NOLFS
           | SQLITE_AUTH
           | SQLITE_FORMAT
           | SQLITE_RANGE
           | SQLITE_NOTADB
           | SQLITE_NOTICE
           | SQLITE_WARNING
           | SQLITE_ROW
           | SQLITE_DONE ;

    datatype value = SqlInt of int
                   | SqlInt64 of int
                   | SqlDouble of real
                   | SqlText of string
                   | SqlNull;

    val openDb : string -> (sqliteResultCode * db option);
    val close : db -> sqliteResultCode;
    val prepare : (db * string) -> sqliteResultCode;
    val step : db -> sqliteResultCode;
    val finalize : db -> sqliteResultCode;
    val bind : (db * value list) -> bool;
    val bindParameterCount : db -> int;

    (* high-level functions *)
    val runQuery : string -> value list -> db -> value list list;
end


structure Sqlite3 :> SQLITE3 = struct

open Foreign
val libsqlite3 = loadLibrary "libsqlite3.so";
val sym = getSymbol libsqlite3;
open SqliteCodes;


datatype sqliteType =
         SQLITE_INTEGER
       | SQLITE_FLOAT
       | SQLITE_TEXT
       | SQLITE_BLOB
       | SQLITE_NULL

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

val cSqliteResultCode : sqliteResultCode conversion = makeConversion
      {ctype = LowLevel.cTypeInt32,
       load = fn (m: Memory.voidStar) =>
                 let val n = Word32.toIntX(Memory.get32(m,0w0)) in
                     case SqliteCodes.fromInt(n) of
                         SOME c => c
                       | NONE => raise (Foreign ("Got unsupported sqliteResultCode: "
                                                 ^ Int.toString(n))) end,
       store = fn (m: Memory.voidStar, r: sqliteResultCode) =>
                  raise (Foreign ("Trying to store sqliteResultCode" ^ (PolyML.makestring r)))
      }

val c_openDb = buildCall2 (sym "sqlite3_open", (cString, cStar cPointer), cSqliteResultCode);
val c_close = buildCall1 (sym "sqlite3_close", cPointer, cSqliteResultCode);
val c_prepare = buildCall6 (sym "sqlite3_prepare_v3",
                            (cPointer, cString, cInt, cUint, cStar cPointer, cStar cPointer),
                            cSqliteResultCode);

val c_step = buildCall1 (sym "sqlite3_step", (cPointer), cSqliteResultCode);
val c_finalize = buildCall1 (sym "sqlite3_finalize", (cPointer), cSqliteResultCode);
val c_columnCount = buildCall1 (sym "sqlite3_column_count", (cPointer), cInt);
val c_columnType = buildCall2 (sym "sqlite3_column_type", (cPointer, cInt), cSqliteType);

val c_bindParameterCount = buildCall1 (sym "sqlite3_bind_parameter_count", (cPointer), cInt);
val c_bindInt = buildCall3 (sym "sqlite3_bind_int", (cPointer, cInt, cInt32), cSqliteResultCode);
val c_bindInt64 = buildCall3 (sym "sqlite3_bind_int64", (cPointer, cInt, cInt64), cSqliteResultCode);
val c_bindDouble = buildCall3 (sym "sqlite3_bind_double", (cPointer, cInt, cDouble), cSqliteResultCode);
val c_bindText = buildCall5 (sym "sqlite3_bind_text", (cPointer,cInt,cString,cInt,cInt), cSqliteResultCode);
val c_bindNull = buildCall2 (sym "sqlite3_bind_null", (cPointer, cInt), cSqliteResultCode);

val c_columnBlob = buildCall2 (sym "sqlite3_column_blob", (cPointer, cInt), cPointer);
val c_columnDouble = buildCall2 (sym "sqlite3_column_double", (cPointer, cInt), cDouble);
val c_columnInt = buildCall2 (sym "sqlite3_column_int", (cPointer, cInt), cInt);
val c_columnInt64 = buildCall2 (sym "sqlite3_column_int64", (cPointer, cInt), cInt64);
val c_columnText = buildCall2 (sym "sqlite3_column_text", (cPointer, cInt), cPointer);
val c_columnBytes = buildCall2 (sym "sqlite3_column_bytes", (cPointer, cInt), cInt);


type stmt = Memory.voidStar ref;


(* Signature interface below *)

type db = {dbHandle: Memory.voidStar ref, stmt: stmt, values: Memory.voidStar ref list}
datatype value = SqlInt of int
                 | SqlInt64 of int
                 | SqlDouble of real
                 | SqlText of string
                 | SqlNull

fun openDb (filename : string) : (sqliteResultCode * db option) =
    let val dbH = ref (Memory.malloc 0w0);
        val res = c_openDb (filename, dbH);
    in if res = SQLITE_OK
       then (res, SOME {dbHandle=dbH, stmt=ref (Memory.malloc 0w0), values=[]})
       else (res, NONE) before Memory.free (!dbH)
    end

fun close ({dbHandle,...} : db) : sqliteResultCode =
    c_close (!dbHandle);

fun prepare ({dbHandle,stmt,...} : db, input : string) : sqliteResultCode =
    c_prepare(!dbHandle, input, ~1, 0, stmt, ref Memory.null);

fun step ({stmt,...} : db) : sqliteResultCode = c_step(!stmt);

fun finalize ({stmt,...} : db) : sqliteResultCode = c_finalize(!stmt);

fun bindParameterCount ({stmt,...} : db) : int =
    c_bindParameterCount(!stmt);

fun bindValue (stmt : stmt, idx: int, value: value) : sqliteResultCode =
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
        List.all (fn r => r = SQLITE_OK) res
    end

fun getRes (stmt : stmt) : value list =
    case c_columnCount(!stmt) of
        0 => []
      | n => getColumns(stmt,0,n,[])
and getColumns (stmt, idx, total, acc) : value list =
    let val thisColumn = case c_columnType(!stmt, idx) of
                             SQLITE_INTEGER => SqlInt (c_columnInt (!stmt, idx))
                           | SQLITE_FLOAT => SqlDouble (c_columnDouble (!stmt, idx))
                           | SQLITE_TEXT => SqlText (readText (stmt, idx))
                           | SQLITE_BLOB => raise (Fail "BLOB not supported right now")
                           | SQLITE_NULL => SqlNull
    in if idx = total-1
       then rev (thisColumn::acc)
       else getColumns(stmt, idx+1, total, thisColumn::acc)
    end
and readText (stmt: stmt, idx : int) : string =
    let val size = c_columnBytes(!stmt, idx)
        val mem = c_columnText(!stmt, idx)
        val getEl = fn idx => Char.chr(Word8.toInt(Memory.get8(mem, Word.fromInt(idx))))
        val rawBytes = List.tabulate(size, getEl)
    in String.implode rawBytes
    end

fun stepThrough (db as {stmt,...}: db, acc : value list list) =
    case step(db) of
        SQLITE_DONE => rev acc
      | SQLITE_ROW => stepThrough(db, getRes stmt :: acc)
      | code => raise Fail("Sqlite error:" ^ PolyML.makestring(code))

(* High-level interface *)
fun runQuery (sql : string) (params : value list) (db: db) : value list list =
    if prepare(db, sql) <> SQLITE_OK
    then raise (Fail "TODO: failed to Prepare statement")
    else if bind(db, params) <> true
    then raise (Fail "TODO2")
    else stepThrough(db, []) before ignore (finalize db)

end
