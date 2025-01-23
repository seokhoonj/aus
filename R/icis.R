
#' @export
check_icis_names <- function(icis) {
  icis_var <- local(.ICIS_VAR, envir = .AUS_ENV)
  diff_var <- setdiff(icis_var, names(icis))
  if (length(diff_var) > 0)
    stop("No columns: ", paste(diff_var, collapse = ", "), ".", call. = FALSE)
}

#' @export
process_icis <- function(claim = claim, main = main) {
  jaid::valid_cols(claim, local(.CLAIM_COLUMNS, envir = .AUS_ENV))
  date_cols <- c("inq_date", "clm_date", "hos_sdate", "hos_edate")
  if (data.table::is.data.table(claim)) {
    claim <- copy(claim)
    claim[, (date_cols) := lapply(.SD, function(x) format(x, "%Y%m%d")), .SDcols = date_cols]
  } else {
    claim[, date_cols] <- lapply(claim[, date_cols], function(x) format(x, "%Y%m%d"))
  }
  ICIS <- reticulate::import("underwriter.icis")$ICIS
  icis <- ICIS(claim = claim, main = main)
  icis$process()
  return(icis$merged)
}
