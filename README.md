# pg-idris

Experimental Postgres library for Idris 2.

Early days with no real plans for completion, but you might still find something here informative.

This project is going to see tons of breaking changes as I experiment with approaches to support Postgres from an Idris interface.

## Status

Currently supports:
- Connecting to database.
- Arbitrary string commands with the result status returned.
- Arbitrary string queries with response parsed as JSON.
- Arbitrary string queries with result columns all interpreted as Strings.
- String queries with result rows containing given expected Idris types.
- Table queries (table dictates field names and types available to select or insert).
  - Inner joins.
  - Left joins.
- Listen for notifications on a particular channel.
- Request next unprocessed notification.

## Dependencies

This library currently depends on bleeding edge of the Idris 2 project. At any time you might need to build and install Idris 2 from its `main` branch in order to build and use this library.

You also need `libpq` (the Postgres C client implementation) installed to run apps using this library and `libpq-dev` to build this library from source.

If you want to use this library with the NodeJS backend for Idris 2, you will need both the `libpq` system libray and also the `@mattpolzin/libpq-bare` NodeJS library installed (see [install section](#install-the-nodejs-library) below. You will need [node-gyp](https://github.com/nodejs/node-gyp) to build the native component of the NodeJS `libpq-bare` library.

To build and install this library you will need to have `clang` on MacOS (comes out of box) and `gcc` on Linux (depending on the distro might need to be installed).

### Idris Packages

The `indexed` Idris package is needed at version `0.0.5` or greater. It is cloned, built, and installed locally to this project's `depends` folder when you run `make`. You can find the package at https://github.com/mattpolzin/idris-indexed.

## Usage

You can take a look at the `Main.idr` and `example.ipkg` in the Example folder for an example of using the library. 

### Install the library
Run `make install` to build and install the library. Make sure that the libraries install directory (`$(idris2 --libpath)/pg-idris/lib`) is in your `LD_LIBRARY_PATH` environment variable so that pg-idris can be found when building an app that uses it. An alternative is to copy the contents of that lib directory into the directory of any executable you build that depends on it.

### Install the NodeJS library (optional)
You only need to do this if you are targeting the NodeJS backend for Idris 2.

You can install this library globally with `npm install -g @mattpolzin/libpq-bare` or you can install it locally to your current project with `npm install @mattpolzin/libpq-bare` or you can create a `package.json` file for your project if it doesn't already have one and specify a dependency on `@mattpolzin/libpq-bare` there.

The important thing is that wherever you put your executable JS file (the thing Idris2 builds and by default dumps to `build/exec/<executable-name>`) you have a `node_modules` directory containing the `libpq-bare` NodeJS library (unless you've installed `libpq-bare` globally).

### Include the package
When running `idris2`, pass the `-p pg-idris`, `-p indexed`, and `-p contrib` command line arguments.

If you have a package manifest, add `pg-idris`, `indexed`, and `contrib` to the list of `depends`:
```
package yourpackage

...

depends = contrib, pg-idris
```

### High level usage
There are some lower level functions available, but the high level usage is currently as follows.

#### Establish a connection
First, `import Postgres` and if you want to use `tableQuery` and `tableSelect` (more on these below) then `import Postgres.Data.PostgresTable` as well.

You can use the `Database` type to open a connection, work with it, and then close it again.
```idris
import Postgres
import Postgres.Data.PostgresTable

openAndClose : (url : String) -> Database () Closed (const Closed)
openAndClose url =
  do initDatabase
     OK <- openDatabase url
       | Failed err => pure () -- connection error!
--   ?databaseRoutine
     closeDatabase
```

The type of the hole `?databaseRoutine` is `Database ?a Open (\x => Open)`; in other words, you can do anything there that operates on an open database connection and does not close it.

You'll need to run your `Database` through `evalDatabase` to produce IO.

Opening and closing and producing IO are a bit boring, so you can use `withDB` to hide those details and focus on `?databaseRoutine`:
```idris
runRoutine : HasIO io => (url : String) -> io (Either String ())
runRoutine url =
  withDB url ?databaseRoutine
```

#### Write a routine
For now, there's not much you'll want to do inside your routine other than prepare and execute a command:
```idris
execCommand : Database () Open (const Open)
execCommand = do liftIO' $ putStrLn "Woo! Running a command!"
                 exec ?postgresCommand
```

The type of the hole `?postgresCommand` is `Connection -> IO ()`. In other words, anything you might want to do given an open Postgres connection.

#### Run commands
The following commands are currently available.
```idris
namespace CommandReference
  ||| Query the database interpreting all columns as strings.
  stringQuery : (header : Bool) -> (query : String) -> Connection -> IO (Either String (StringResultset header))

  ||| Query the database expecting a JSON result is returned.
  jsonQuery : (query : String) -> Connection -> IO (Maybe JSON)

  ||| Query the database expecting the given array of types in each
  ||| row of the result returned.
  expectedQuery : {cols : Nat} 
               -> (expected : Vect cols Type) 
               -> (query : String) 
               -> {auto castable : (All Castable expected)} 
               -> Connection 
               -> IO (Either String (rows ** Vect rows (HVect expected)))

  ||| Perform the given command and instead of parsing the response
  ||| just report the result status. This is useful when you don't
  ||| care if/what the response looks like, just whether the command
  ||| worked
  perform : (command : String) -> Connection -> IO ResultStatus

  ||| Query the given table in the database mapping each row to the given Idris type.
  ||| @param table The table to query against.
  ||| @param cols A Vect of tuples where the first element is a column name to select and
  |||             the second element is an Idris type to cast the column value to.
  ||| @param conn A database connection.
  export
  tableQuery : PostgresTable t =>
               {n : _}
            -> (table : t)
            -> (cols : Vect n (ColumnIdentifier, Type))
            -> HasMappings IdrCast table cols =>
               (conn : Connection)
            -> IO (Either String (rowCount ** Vect rowCount (HVect (Builtin.snd <$> cols))))

  ||| Insert the given values into the given table.
  ||| @param table The table to insert into.
  ||| @param cols A Vect of column names to insert into (does
  |||             not need to be every column in the table but
  |||             there is not currently protection against omitting
  |||             a column with no default value).
  ||| @param values The values to insert.
  ||| @param conn A database connection.
  export
  tableInsert : {n : _}
             -> (table : PersistedTable)
             -> (cols : Vect n String)
             -> {colTypes : Vect n Type}
             -> (values : HVect colTypes)
             -> HasMappings PGCast table (zip (MkColumnId (aliasOrName table) <$> cols) colTypes) =>
                (conn : Connection)
             -> IO ResultStatus

  ||| Start listening for notifications on the given channel.
  listen : (channel : String) -> Connection -> IO ResultStatus

  ||| Gets the next notification _of those sitting around locally_.
  ||| Returns `Nothing` if there are no notifications.
  |||
  ||| See `libpq` documentation on `PQnotifies` for details on the
  ||| distinction between retrieving notifications from the server and
  ||| getting the next notification that has already been retrieved.
  |||
  ||| NOTE: This function _does_ consume input to make sure no notification
  |||  sent by the server but not processed by the client yet gets
  |||  missed.
  nextNotification : Connection -> IO (Maybe Notification)
```

It's worth mentioning that the `stringQuery` success case can either have a header (with column names) or not:
```idris
  StringResultset : (header : Bool) -> Type
  StringResultset False = (rows ** cols ** Vect rows (Vect cols (Maybe String)))
  StringResultset True = (rows ** cols ** (Vect cols ColHeader, Vect rows (Vect cols (Maybe String))))
```

The higher level APIs providing a degree of safety are `jsonQuery`, `expectedQuery`, `tableQuery`, and `tableInsert`.

`jsonQuery` just expects 1 column in the result and attempts to parse it as JSON.

An example use of `expectedQuery` would be getting a list of Postgres tables and whether each one has indices:
```idris
execQuery : Database ? Open (const Open)
execQuery = exec $
  expectedQuery [String, String, Bool] "select schemaname, tablename, hasindexes from pg_tables limit 10"
```
You can support additional types for `expectedQuery` by creating new implementations of the `DBStringCast` interface. Idris's `:doc DBStringCast` will tell you what types are supported out of box by `pg-idris`.

Using `tableInsert` and `tableQuery` involes creating a representation of a table in Idris. Then you can safely insert or select subsets of the table's columns:
```idris
||| A table named "first_table" in Postgres. This table may have been created with the following CREATE TABLE statment:
|||  CREATE TABLE first_table (id integer primary key, name text not null, age integer)
table1 : PersistedTable
table1 = PT "first_table" [
  ("id",   col NonNullable PInteger)
, ("name", col NonNullable PString)
, ("age",  col Nullable    PInteger)
]

execInsert : Database ? Open (const Open)
execInsert = exec $
  DB.tableInsert table1 ["name", "age"] ["Matt", the (Maybe Nat) (Just 30)]

execSelect : Database ? Open (const Open)
execSelect = exec $
  DB.tableQuery table1 [("name", String), ("age", Maybe Integer)]
```

You can also select results out of table joins (currently only inner-joins and left-joins):
```idris
table2 : PersistedTable
table2 = PT "second_table" [
  ("id",             col NonNullable PInteger)
, ("first_table_id", col NonNullable PInteger)
, ("location",       col NonNullable PString)
]

execJoin : Database ? Open (const Open)
execJoin = exec $
  DB.tableQuery (innerJoin table1 table2 (On "table1.id" "table2.first_table_id"))
                [ ("table1.name",     String)
                , ("table2.location", String)
                ]
```

