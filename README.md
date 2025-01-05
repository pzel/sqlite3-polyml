# Sqlite3-polyml

## This is only a toy!


This is an FFI binding for libsqlite3, for use in Poly/ML programs.

It can be compiled with [polymlb](https://github.com/vqns/polymlb).


# The API

```
signature SQLITE3 = sig
    type db;
    datatype sqliteResultCode =
             SQLITE_OK
           | SQLITE_ERROR
           | <... lots of codes omitted here ...>
           | SQLITE_ROW
           | SQLITE_DONE ;

    datatype value = SqlInt of int
                   | SqlInt64 of int
                   | SqlDouble of real
                   | SqlText of string
                   | SqlBlob of Word8Vector.vector
                   | SqlNull;

    (* low-level API *)
    val openDb : string -> (sqliteResultCode * db option);
    val close : db -> sqliteResultCode;
    val prepare : (db * string) -> sqliteResultCode;
    val step : db -> sqliteResultCode;
    val finalize : db -> sqliteResultCode;
    val bind : (db * value list) -> bool;
    val bindParameterCount : db -> int;

    (* high-level API *)
    val runQuery : string -> value list -> db -> value list list;
    val execute : string -> db -> sqliteResultCode;
end
```


# Running the tests

`make test`
