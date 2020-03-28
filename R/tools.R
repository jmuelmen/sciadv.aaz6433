#' Save a data.frame to an SQL database
#'
#' @param df @code{data.frame} to convert
#' @param fname output SQLite db file name
#' @param name name of table in db
#' @return produced table as a \code{tbl}
#' @export
df.to.db <- function(df, db.name = "wcrain", tbl.name) {
    db <- dplyr::src_postgres(db.name, "postgresql01",
                              user = "pgadmin", password = "oeDa9vah")
    dplyr::copy_to(db, df, tbl.name,
                   indexes = list("lon", "lat"), ## index everything except file names and MD5 sum
                   temporary = FALSE)
    return(dplyr::tbl(db, tbl.name))
}
