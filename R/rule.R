
#' @export
replace_a_with_b <- function(df, cols, a = "미입력", b = "any") {
  class <- sapply(df, class)
  if (missing(cols))
    cols <- names(class)[which(class == "character")]
  df[, `:=`((cols), lapply(.SD, function(x) ifelse(x == a, b, x))), .SDcols = cols]
  invisible(df[])
}

#' @export
split_band <- function(df, var, split = "~", any = "0~99999", etc = "0~99999") {
  jaid::assert_class(df, "data.frame")
  var <- rlang::as_name(rlang::enquo(var))
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
  return(df)
}

