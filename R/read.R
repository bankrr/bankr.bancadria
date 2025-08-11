#' Read bank statement
#' @param path A path.
#' @export
read <- function(path) {
  if (!chkr.utils::is_string(path)) {
    stop("Path is not a character of length one.")
  }
  if (!file.exists(path)) {
    stop("File does not exist.")
  }
  read.csv2(path)
}
