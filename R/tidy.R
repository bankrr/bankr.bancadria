#' Tidy bank statement
#' @param dat A data.frame as returned by [read()].
#' @export
tidy <- function(dat) {
  validate(dat)
  colnames(dat) <- tolower(colnames(dat))
  colnames(dat)[colnames(dat) == "x"] <- "currency"
  colnames(dat)[colnames(dat) == "data"] <- "date"

  dat_sub <- dat[, setdiff(colnames(dat), "x.1")]

  dat_sub[["date"]] <- as.Date(dat_sub[["date"]], format = "%d/%m/%Y")
  dat_sub[["valuta"]] <- as.Date(dat_sub[["valuta"]], format = "%d/%m/%Y")

  # Clean DARE column: remove dots, replace comma with dot
  if ("dare" %in% colnames(dat_sub)) {
    dat_sub[["dare"]] <- gsub("\\.", "", dat_sub[["dare"]])
    dat_sub[["dare"]] <- gsub(",", ".", dat_sub[["dare"]])
  }

  # Clean AVERE column: remove dots, replace comma with dot
  if ("avere" %in% colnames(dat_sub)) {
    dat_sub[["avere"]] <- gsub("\\.", "", dat_sub[["avere"]])
    dat_sub[["avere"]] <- gsub(",", ".", dat_sub[["avere"]])
  }

  dat_sub[["description"]] <- tolower(gsub(
    "\\sCausale.+|^Ordinante:\\s",
    "",
    dat_sub[["descrizione.operazione"]]
  ))

  # Convert DARE and AVERE to numeric
  # if ("dare" %in% colnames(dat_sub)) {
  #   dat_sub[["dare"]] <- as.numeric(dat_sub[["dare"]])
  #   if (anyNA(dat_sub[["dare"]])) {
  #     stop("Failed to convert DARE values to numeric. Check for invalid number formats.")
  #   }
  # }

  if ("avere" %in% colnames(dat_sub)) {
    dat_sub[["avere"]] <- as.numeric(dat_sub[["avere"]])
    if (anyNA(dat_sub[["avere"]])) {
      stop(
        "Failed to convert AVERE values to numeric. Check for invalid number formats."
      )
    }
  }

  # Remove balance rows that are not actual transactions using regex
  if ("descrizione.operazione" %in% colnames(dat_sub)) {
    balance_pattern <- "^(Saldo contabile|Saldo liquido|Disponibilit\u00e0 al|Saldo SBF per conti unici al|Saldo iniziale)"
    exclude_rows <- grepl(
      balance_pattern,
      dat_sub[["descrizione.operazione"]],
      perl = TRUE
    )
    dat_sub <- dat_sub[!exclude_rows, ]
  }

  dat_sub
}
