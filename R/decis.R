
#' @export
apply_rule <- function(diz_book, rule, icis, decis_var, limit = 1e7) {
  # expect cartesian rows
  n <- expect_cartesian_rows(diz_book, rule, icis)
  if (n > limit)
    stop("Total rows are ", jaid::as_comma(n), " (> ", jaid::as_comma(limit), ")",
         call. = FALSE)

  # icis with diz_cd and sub_chk (if kcd is NA, sub_chk is NA)
  icis[diz_book, `:=`(diz_cd = diz_cd, sub_chk = sub_chk), on = .(kcd)]
  jaid::set_col_order(icis, .(diz_cd) , before = kcd)
  jaid::set_col_order(icis, .(sub_chk), after  = main_yn)

  # icis with rule_yn
  rule_yn_table <- unique(rule[, .(diz_cd, rule_yn = 1)])
  icis[rule_yn_table, on = .(diz_cd), rule_yn := i.rule_yn]
  icis[is.na(rule_yn), rule_yn := 0]
  jaid::set_col_order(icis, rule_yn, after = diz_cd)

  # icis x rule (cartesian)
  decl_var  <- local(.DECL_VAR , envir = .AUS_ENV)
  cond_var  <- local(.COND_VAR , envir = .AUS_ENV)
  if (missing(decis_var))
    decis_var <- local(.DECIS_VAR, envir = .AUS_ENV)

  jaid::timeit(icis <- rule[icis, on = .(diz_cd), allow.cartesian = TRUE])
  icis <- icis[, .SD, .SDcols = c("id", "gender", "age", "rule_yn", "diz_cd", "kcd", "kcd_seq", "main_yn", "sub_chk", "hos_day", "hos_cnt", "sur_cnt", "elp_day", cond_var, decis_var)]
  icis[ is.na(kcd) & is.na(diz_cd), (decis_var) := lapply(.SD, function(x) ifelse(is.na(x), "S0", x)), .SDcols = decis_var]
  icis[!is.na(kcd) & is.na(diz_cd), (decis_var) := lapply(.SD, function(x) ifelse(is.na(x), "NR", x)), .SDcols = decis_var]
  icis[rule_yn == 0, (decis_var) := lapply(.SD, function(x) ifelse(is.na(x), "NR", x)), .SDcols = decis_var]
  icis[rule_yn == 0 | (
    age     >=     age_min & age     <=     age_max &
    hos_day >= hos_day_min & hos_day <= hos_day_max &
    sur_cnt >= sur_cnt_min & sur_cnt <= sur_cnt_max &
    elp_day >= elp_day_min & elp_day <= elp_day_max
  )]
}

get_new_term <- function(fterm, vterm) {
  cmp <- as.numeric(fterm) >= as.numeric(gsub("i", "", vterm))
  ifelse(cmp != TRUE | is.na(cmp), jaid::paste_list(list(fterm, vterm), sep = ","),
         ifelse(cmp == TRUE, fterm, ""))
}

#' @export
get_mod_decis <- function(df, id_var = c("id"), decis_var = c("life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving"), type = c(1, 2)) {
  jaid::assert_class(df, "data.table")
  col <- decis_var[1L]
  ds <- jaid::split_str(df[[col]], split = ",")
  data.table::setattr(ds, "names", df[[id_var]])
  dt <- utils::stack(ds)
  data.table::setDT(dt)
  data.table::setnames(dt, c(col, id_var))
  data.table::setcolorder(dt, id_var)
  dt[, code  := lapply(.SD, function(x) gsub("\\([0-9i]+\\)", "", x)), .SDcols = col]
  dt[, fterm := lapply(.SD, function(x) gsub("[()]", "", jaid::get_pattern_all("\\([0-9]+\\)" , x, collapse = ","))), .SDcols = col]
  dt[, vterm := lapply(.SD, function(x) gsub("[()]", "", jaid::get_pattern_all("\\([0-5]+i\\)", x, collapse = ","))), .SDcols = col]
  dt_a0 <- dt[code == "A0", .(fterm = as.character(sum(as.numeric(fterm)-100)+100), vterm = ""), .(id, code)]
  dt_e0 <- dt[grepl("E0[0-9]{2}", code), .(id, code, fterm, vterm)]
  if (nrow(dt_e0) > 0)
    dt_e0 <- dt_e0[, .(fterm = max(fterm), vterm = max(vterm)), .(id, code)]
  dt_ex <- dt[!grepl("A0|E0[0-9]{2}", code), .(id, code, fterm, vterm)]
  db <- data.table::rbindlist(list(dt_a0, dt_e0, dt_ex))
  db[, nterm := get_new_term(fterm, vterm)]
  db[, fun   := ifelse(jaid::count_pattern("\\,", nterm) > 0, "Max", "")]
  db[, term  := ifelse(nterm != "" & fun == "", sprintf("(%s)", nterm), nterm)]
  db[, term  := ifelse(nterm != "" & fun != "", sprintf("(%s[%s])", fun, nterm), term)]
  db[, decis := paste0(code, term)]
  data.table::setorderv(db, c(id_var, "decis"))
  if (type[[1L]] == 1) {
    db <- db[, .(decis = jaid::paste_sort_uni_str(decis, collapse = ",")), .(id)]
  }
  data.table::setnames(db, "decis", col)
  return(db[])
}

#' @export
get_final_from_applied <- function(applied, decis_var) {
  jaid::assert_class(applied, "data.table")
  if (missing(decis_var))
    decis_var <- local(.DECIS_VAR, envir = .AUS_ENV)
  final <- applied[, lapply(.SD, function(x) jaid::paste_sort_uni_str(x, collapse = ",")),
                   .(id, gender, age), .SDcols = decis_var]
  final_list <- lapply(decis_var, function(x) get_mod_decis(final, decis_var = x))
  final <- Reduce(function(...) merge(..., by = "id"), final_list)
  final[, (decis_var) := lapply(.SD, change_final_decis), .SDcols = decis_var]
  return(final)
}



get_a0 <- function(x) {
  x <- gsub("A0|[()]", "", jaid::get_pattern_all("A0\\([0-9]+\\)", x))
  x[x == ""] <- "0"
  as.numeric(x)
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
    grepl("D0", x) | jaid::count_pattern("E0", x) > 4 | get_a0(x) > 300, "D0", ifelse(
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
    grepl("D0", x) | jaid::count_pattern("E0", x) > 4 | get_a0(x) > 300, "D0", ifelse(
      grepl("U0", x), "U0", ifelse(
        grepl("NR", x), "NR", ifelse(
          grepl("A0|C0|CM|E0|RA|RQ", x), "EX", x)
      )
    )
  )
}
