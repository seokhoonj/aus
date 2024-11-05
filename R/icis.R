
#' @export
check_icis_names <- function(icis) {
  icis_var <- local(.ICIS_VAR, envir = .AUS_ENV)
  diff_var <- setdiff(icis_var, names(icis))
  if (length(diff_var) > 0)
    stop("No columns: ", paste(diff_var, collapse = ", "), ".", call. = FALSE)
}
