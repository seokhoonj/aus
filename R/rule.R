
#' @export
check_rule_names <- function(rule) {
  rule_var <- local(.RULE_VAR, envir = .AUS_ENV)
  diff_var <- setdiff(rule_var, names(rule))
  if (length(diff_var) > 0)
    stop("No columns: ", paste(diff_var, collapse = ", "), ".", call. = FALSE)
}

#' @export
split_rule_band <- function(df, var, split = "~", any = "0~99999", etc = "0~99999") {
  jaid::assert_class(df, "data.frame")
  var <- rlang::as_name(rlang::enquo(var))
  jaid::has_cols(df, var, error_raise = TRUE)
  var_min <- paste0(var, "_min")
  var_max <- paste0(var, "_max")
  re <- sprintf("^[0-9]+$|^[0-9]+%s[0-9]+$|^%s[0-9]+$|^[0-9]+%s$",
                split, split, split)
  i_any <- which(df[[var]] == "any")
  i_etc <- which(!grepl(sprintf("%s|%s", re, "any"), df[[var]]))
  col_min <- jaid::get_pattern("^[0-9]+|^[0-9]+$", df[[var]])
  col_max <- jaid::get_pattern("[0-9]+$|^[0-9]+$", df[[var]])
  df[, (var_min) := col_min]
  df[, (var_max) := col_max]
  any_min <- jaid::get_pattern("^[0-9]+|^[0-9]+$", any)
  any_max <- jaid::get_pattern("[0-9]+$|^[0-9]+$", any)
  etc_min <- jaid::get_pattern("^[0-9]+|^[0-9]+$", etc)
  etc_max <- jaid::get_pattern("[0-9]+$|^[0-9]+$", etc)
  data.table::set(df, i = i_any, j = var_min, value = any_min)
  data.table::set(df, i = i_any, j = var_max, value = any_max)
  data.table::set(df, i = i_etc, j = var_min, value = etc_min)
  data.table::set(df, i = i_etc, j = var_max, value = etc_max)
  data.table::setcolorder(df, c(var_min, var_max), after = var)
  cols <- c(var_min, var_max)
  df[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  return(df)
}

get_new_term <- function(fterm, vterm) {
  cmp <- as.numeric(fterm) >= as.numeric(gsub("i", "", vterm))
  ifelse(cmp != TRUE | is.na(cmp), jaid::paste_list(list(fterm, vterm), sep = ","),
         ifelse(cmp == TRUE, fterm, ""))
}
