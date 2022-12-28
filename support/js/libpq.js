const libpq = require('libpq-bare')

const libpq_PQconnectdb = libpq.PQconnectdb
const libpq_PQstatus = libpq.PQstatus
const libpq_PQexec = libpq.PQexec
const libpq_PQclear = libpq.PQclear
const libpq_PQresultStatus = libpq.PQresultStatus
const libpq_PQresultErrorMessage = libpq.PQresultErrorMessage
const libpq_PQntuples = libpq.PQntuples
const libpq_PQnfields = libpq.PQnfields
const libpq_PQfname = libpq.PQfname
const libpq_PQfformat = libpq.PQfformat
const libpq_PQftype = libpq.PQftype
const libpq_PQgetisnull = libpq.PQgetisnull
const libpq_PQgetvalue = libpq.PQgetvalue
const libpq_PQconsumeInput = libpq.PQconsumeInput
const libpq_PQfinish = libpq.PQfinish

const libpq_connErrorMessage = (conn) => {
  // do I need to copy it here like i do for the c helper?
	return libpq.PQerrorMessage(conn);
}
