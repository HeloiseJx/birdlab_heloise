#' Import sql file in R
#'
#' @param filepath the path of the query as sql file
#'
#' @return
#' @export
#'
#' @examples
read_sql_query <- function(filepath){
  con = file(filepath, "r")
  lines <- readLines(con)
  for (i in seq_along(lines)){
    lines[i] <- gsub("\\t", " ", lines[i])
    if(grepl("--",lines[i]) == TRUE){
      lines[i] <- paste(sub("--","/*",lines[i]),"*/")
    }
  }
  sql.string <- paste(lines, collapse = " ")
  close(con)
  return(sql.string)
}