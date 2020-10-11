# pg-idris

The beginnings of a Postgres library for Idris 2.

## Status

Early days with no real plans for completion, but you might still find something here informative.

I am focusing on getting Postgres Notifications and JSON query results working because those happen to be the things I need for the project at hand.

## Usage

Run the `make install` to build and install the library (including a workaround I found I needed on OS X that copies the library output to a `.dylib`).

You can take a look at the `Main.idr` and `example.ipkg` in the Example folder for an example of using the library. 
Of particular note is the option the package file specifies to include the `pg-idris` package.
