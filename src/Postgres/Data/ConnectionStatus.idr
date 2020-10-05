module Postgres.Data.ConnectionStatus

public export
data ConnectionStatus = OK
                      | BAD
                      | STARTED
                      | MADE
                      | AWAITING_RESPONSE
                      | AUTH_OK
                      | SETENV
                      | SSL_STARTUP
                      | NEEDED
                      | OTHER Int

export
Show ConnectionStatus where
    show OK                = "OK"
    show BAD               = "BAD"
    show STARTED           = "STARTED"
    show MADE              = "MADE"
    show AWAITING_RESPONSE = "AWAITING_RESPONSE"
    show AUTH_OK           = "AUTH_OK"
    show SETENV            = "SETENV"
    show SSL_STARTUP       = "SSL_STARTUP"
    show NEEDED            = "NEEDED"
    show (OTHER x)         = "OTHER " ++ (show x)
