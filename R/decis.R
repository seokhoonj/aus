
#' @export
get_mod_decis <- function(df, id_var = c("id"), decis_var = c("life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving"), type = c(1, 2)) {
  col <- decis_var[1L]
  ds <- jaid::split_str(df[[col]], split = ",")
  data.table::setattr(ds, "names", df[[id_var]])
  dt <- utils::stack(ds)
  data.table::setDT(dt)
  data.table::setnames(dt, c(col, id_var))
  data.table::setcolorder(dt, id_var)
  dt[, code  := lapply(.SD, function(x) gsub("\\([0-9i]+\\)", "", x)), .SDcols = col]
  dt[, fterm := lapply(.SD, function(x) gsub("[()]", "", jaid::get_pattern_all("\\([0-5]+\\)" , x, collapse = ","))), .SDcols = col]
  dt[, vterm := lapply(.SD, function(x) gsub("[()]", "", jaid::get_pattern_all("\\([0-5]+i\\)", x, collapse = ","))), .SDcols = col]
  dt1 <- dt[code != "E0xx", .(fterm = max(fterm), vterm = max(vterm)), .(id, code)]
  dt2 <- dt[code == "E0xx", .(id, code, fterm, vterm)]
  db <- data.table::rbindlist(list(dt1, dt2))
  db[, nterm := get_new_term(fterm, vterm)]
  db[, fun  := ifelse(jaid::count_pattern("\\,", nterm) > 0, "Max", "")]
  db[, term := ifelse(nterm != "" & fun == "", sprintf("(%s)", nterm), nterm)]
  db[, term := ifelse(nterm != "" & fun != "", sprintf("(%s[%s])", fun, nterm), term)]
  db[, decis := paste0(code, term)]
  data.table::setorderv(db, c(id_var, "decis"))
  if (type[[1L]] == 1) {
    db <- db[, .(decis = jaid::paste_sort_uni_str(decis, collapse = ",")), .(id)]
  }
  data.table::setnames(db, "decis", col)
  return(db[])
}

#' change_final_decis <- function(x) {
#'   ifelse(
#'     grepl("D0", x) | jaid::count_pattern("E0", x) >= 4, "D0", ifelse(
#'       grepl("NR|U0", x), "U0", ifelse(
#'         grepl("RA|A0|E0", x), gsub("S0|,S0$", "", x), x)
#'     )
#'   )
#' }

#' @export
change_final_decis <- function(x) {
  ifelse(
    grepl("D0", x) | jaid::count_pattern("E0", x) >= 4, "D0", ifelse(
      grepl("U0", x), "U0", ifelse(
        grepl("NR", x), "NR", ifelse(
          grepl("RA|A0|E0", x), gsub("S0|,S0$", "", x), x)
      )
    )
  )
}

# change_stat_decis <- function(x) {
#   ifelse(
#     grepl("D0", x) | jaid::count_pattern("E0", x) >= 4, "D0", ifelse(
#       grepl("NR|U0", x), "U0", ifelse(
#         grepl("RA|A0|E0", x), "EA", x)
#     )
#   )
# }

#' @export
change_stat_decis <- function(x) {
  ifelse(
    grepl("D0", x) | jaid::count_pattern("E0", x) >= 4, "D0", ifelse(
      grepl("U0", x), "U0", ifelse(
        grepl("NR", x), "NR", ifelse(
          grepl("A0|C0|CM|E0|RA|RQ", x), "EX", x)
      )
    )
  )
}
