module Postgres.Data.ResultStatus


||| See the `ExecStatusType` from libpq for more information.
||| constructors below are derived from libpq options by
||| dropping the "PGRES_" prefix
public export 
data ResultStatus = EMPTY_QUERY
                  | COMMAND_OK
                  | TUPLES_OK
                  | COPY_OUT
                  | COPY_IN
                  | BAD_RESPONSE
                  | NONFATAL_ERROR
                  | FATAL_ERROR
                  | COPY_BOTH
                  | SINGLE_TUPLE
                  | OTHER Int

export
Show ResultStatus where
  show EMPTY_QUERY    = "EMPTY_QUERY"
  show COMMAND_OK     = "COMMAND_OK"
  show TUPLES_OK      = "TUPLES_OK"
  show COPY_OUT       = "COPY_OUT"
  show COPY_IN        = "COPY_IN"
  show BAD_RESPONSE   = "BAD_RESPONSE"
  show NONFATAL_ERROR = "NONFATAL_ERROR"
  show FATAL_ERROR    = "FATAL_ERROR"
  show COPY_BOTH      = "COPY_BOTH"
  show SINGLE_TUPLE   = "SINGLE_TUPLE"
  show (OTHER x)      = "OTHER " ++ (show x)
