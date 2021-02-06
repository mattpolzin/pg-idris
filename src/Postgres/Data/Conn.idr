module Postgres.Data.Conn

||| Internal phantom type used to mark pointers related to
||| DB connections.
export
data PGconn : Type

public export
data Conn : Type where
  MkConn : Ptr PGconn -> Conn

%name Conn conn
