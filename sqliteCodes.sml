structure SqliteCodes = struct
datatype sqliteResultCode =
   SQLITE_OK        (*  0   /* Successful result *)
 | SQLITE_ERROR     (*  1   /* Generic error *)
 | SQLITE_INTERNAL  (*  2   /* Internal logic error in SQLite *)
 | SQLITE_PERM      (*  3   /* Access permission denied *)
 | SQLITE_ABORT     (*  4   /* Callback routine requested an abort *)
 | SQLITE_BUSY      (*  5   /* The database file is locked *)
 | SQLITE_LOCKED    (*  6   /* A table in the database is locked *)
 | SQLITE_NOMEM     (*  7   /* A malloc() failed *)
 | SQLITE_READONLY  (*  8   /* Attempt to write a readonly database *)
 | SQLITE_INTERRUPT (*  9   /* Operation terminated by sqlite3_interrupt()*)
 | SQLITE_IOERR     (* 10   /* Some kind of disk I/O error occurred *)
 | SQLITE_CORRUPT   (* 11   /* The database disk image is malformed *)
 | SQLITE_NOTFOUND  (* 12   /* Unknown opcode in sqlite3_file_control() *)
 | SQLITE_FULL      (* 13   /* Insertion failed because database is full *)
 | SQLITE_CANTOPEN  (* 14   /* Unable to open the database file *)
 | SQLITE_PROTOCOL  (* 15   /* Database lock protocol error *)
 | SQLITE_EMPTY     (* 16   /* Internal use only *)
 | SQLITE_SCHEMA    (* 17   /* The database schema changed *)
 | SQLITE_TOOBIG    (* 18   /* String or BLOB exceeds size limit *)
 | SQLITE_CONSTRAINT(* 19   /* Abort due to constraint violation *)
 | SQLITE_MISMATCH  (* 20   /* Data type mismatch *)
 | SQLITE_MISUSE    (* 21   /* Library used incorrectly *)
 | SQLITE_NOLFS     (* 22   /* Uses OS features not supported on host *)
 | SQLITE_AUTH      (* 23   /* Authorization denied *)
 | SQLITE_FORMAT    (* 24   /* Not used *)
 | SQLITE_RANGE     (* 25   /* 2nd parameter to sqlite3_bind out of range *)
 | SQLITE_NOTADB    (* 26   /* File opened that is not a database file *)
 | SQLITE_NOTICE    (* 27   /* Notifications from sqlite3_log() *)
 | SQLITE_WARNING   (* 28   /* Warnings from sqlite3_log() *)
 | SQLITE_ROW       (* 100  /* sqlite3_step() has another row ready *)
 | SQLITE_DONE      (* 101  /* sqlite3_step() has finished executing *)
;

fun toInt(c : sqliteResultCode) : int =
    case c of SQLITE_OK  =>  0
            | SQLITE_ERROR =>  1
            | SQLITE_INTERNAL =>  2
            | SQLITE_PERM =>  3
            | SQLITE_ABORT =>  4
            | SQLITE_BUSY =>  5
            | SQLITE_LOCKED =>  6
            | SQLITE_NOMEM =>  7
            | SQLITE_READONLY =>  8
            | SQLITE_INTERRUPT =>  9
            | SQLITE_IOERR => 10
            | SQLITE_CORRUPT => 11
            | SQLITE_NOTFOUND => 12
            | SQLITE_FULL => 13
            | SQLITE_CANTOPEN => 14
            | SQLITE_PROTOCOL => 15
            | SQLITE_EMPTY => 16
            | SQLITE_SCHEMA => 17
            | SQLITE_TOOBIG => 18
            | SQLITE_CONSTRAINT => 19
            | SQLITE_MISMATCH => 20
            | SQLITE_MISUSE => 21
            | SQLITE_NOLFS => 22
            | SQLITE_AUTH => 23
            | SQLITE_FORMAT => 24
            | SQLITE_RANGE => 25
            | SQLITE_NOTADB => 26
            | SQLITE_NOTICE => 27
            | SQLITE_WARNING => 28
            | SQLITE_ROW => 100
            | SQLITE_DONE => 101 ;

fun fromInt (i : int) : sqliteResultCode option =
    case i of 0 => SOME SQLITE_OK
           |  1 => SOME SQLITE_ERROR
           |  2 => SOME SQLITE_INTERNAL
           |  3 => SOME SQLITE_PERM
           |  4 => SOME SQLITE_ABORT
           |  5 => SOME SQLITE_BUSY
           |  6 => SOME SQLITE_LOCKED
           |  7 => SOME SQLITE_NOMEM
           |  8 => SOME SQLITE_READONLY
           |  9 => SOME SQLITE_INTERRUPT
           | 10 => SOME SQLITE_IOERR
           | 11 => SOME SQLITE_CORRUPT
           | 12 => SOME SQLITE_NOTFOUND
           | 13 => SOME SQLITE_FULL
           | 14 => SOME SQLITE_CANTOPEN
           | 15 => SOME SQLITE_PROTOCOL
           | 16 => SOME SQLITE_EMPTY
           | 17 => SOME SQLITE_SCHEMA
           | 18 => SOME SQLITE_TOOBIG
           | 19 => SOME SQLITE_CONSTRAINT
           | 20 => SOME SQLITE_MISMATCH
           | 21 => SOME SQLITE_MISUSE
           | 22 => SOME SQLITE_NOLFS
           | 23 => SOME SQLITE_AUTH
           | 24 => SOME SQLITE_FORMAT
           | 25 => SOME SQLITE_RANGE
           | 26 => SOME SQLITE_NOTADB
           | 27 => SOME SQLITE_NOTICE
           | 28 => SOME SQLITE_WARNING
           | 100 => SOME SQLITE_ROW
           | 101 => SOME SQLITE_DONE
           | _ =>  NONE ;

end
