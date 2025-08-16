#' Read bank statement
#' @param path A path.
#' @export
read <- function(path) {
  if (!(is.character(path) && length(path) == 1)) {
    stop("Path is not a character of length one.")
  }
  if (!file.exists(path)) {
    stop("File does not exist.")
  }
  read.csv2(path)
}
