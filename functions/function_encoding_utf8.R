#forçage codage UTF8 champs character
Encoding_utf8 <- function(x) {
  Encoding(x) <- "UTF-8"
  return(x)
}