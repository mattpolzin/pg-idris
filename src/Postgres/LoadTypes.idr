module Postgres.LoadTypes

import Postgres.Data.PostgresType
import Postgres.Data.Conn
import Postgres.Query
import Postgres.Result
import Data.Vect

export
tmp : Conn -> IO ()
tmp conn =
  do let query = "SELECT t.oid, t.typname from pg_type as t where t.typname in ('char', 'bool', 'text', 'varchar', 'json', 'int2', 'int4', 'int8', 'float4', 'float8', 'time', 'timestamp', 'timestamptz', 'timetz', 'numeric', 'uuid', 'jsonb', 'cstring')"
     Right (r ** c ** resultset) <- pgStringResultsQuery query conn 
       | Left err => putStrLn $ "ERROR: " ++ err
     for_ resultset $ \row =>
       for_ row $ \col =>
         putStrLn col

