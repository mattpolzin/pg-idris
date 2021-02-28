# pg-idris

The beginnings of a Postgres library for Idris 2.

Early days with no real plans for completion, but you might still find something here informative.

This project is going to see tons of breaking changes as I experiment with approaches to support Postgres from an Idris interface.

## Status

Currently supports:
- Connecting to database.
- Arbitrary string commands with the result status returned.
- Arbitrary string queries with response parsed as JSON.
- Arbitrary string queries with result columns all interpreted as Strings.
- String queries with result rows containing given expected Idris types.
- Listen for notifications on a particular channel.
- Request next unprocessed notification.

## Dependencies

This library currently depends on bleeding edge of the Idris 2 project. At any time you might need to build and install Idris 2 from its `master` branch in order to build and use this library.

You also need `libpq` (the Postgres C client implementation) installed to run apps using this library and `libpq-dev` to build this library from source.

To build and install this library you will need to have `clang` on MacOS (comes out of box) and `gcc` on Linux (depending on the distro might need to be installed).

## Usage

You can take a look at the `Main.idr` and `example.ipkg` in the Example folder for an example of using the library. 

### Install the library
Run `make install` to build and install the library. Make sure that the libraries install directory (`$(idris2 --libpath)/pg-idris/lib`) is in your `LD_LIBRARY_PATH` environment variable so that pg-idris can be found when building an app that uses it. An alternative is to copy the contents of that lib directory into the directory of any executable you build that depends on it.

### Include the package
When running `idris2`, pass the `-p pg-idris` and `-p contrib` command line arguments.

If you have a package manifest, add `pg-idris` and `contrib` to the list of `depends`:
```
package yourpackage

...

depends = contrib, pg-idris
```

### High level usage
There are some lower level functions available, but the high level usage is currently as follows.

#### Establish a connection
First, `import Postgres`.

You can use the `Database` type to open a connection, work with it, and then close it again.
```idris
openAndClose : (url : String) -> Database () Closed (const Closed)
openAndClose url =
  do initDatabase
     OK <- openDatabase url
       | Failed err => pure () -- connection error!
     ?databaseRoutine
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

So far the most type safety you can get is via either the `jsonQuery` or the `expectedQuery`. The former just expects 1 column in the result and attempts to parse it as JSON. An example use of the latter would be getting a list of Postgres tables and whether each one has indices:
```idris
expectedQuery [String, String, Bool] "select schemaname, tablename, hasindexes from pg_tables limit 10" conn
```
You can find the Idris types supported by `expectedQuery` under the definition of [`HasDefaultType`](https://github.com/mattpolzin/pg-idris/blob/main/src/Postgres/Data/PostgresValue.idr#L107).
