
.AUS_ENV <- NULL
.onLoad <- function(libname, pkgname) {
  .AUS_ENV <<- new.env()
  assign(".COND_VAR" , c("age_min", "age_max", "elp_day_min", "elp_day_max", "sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max"), envir = .AUS_ENV)
  assign(".DECI_VAR" , c("life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving"), envir = .AUS_ENV)
  assign(".DECL_VAR" , c("gender", "age", "diz_cd", "kcd", "elp_day", "sur_cnt", "hos_day"), envir = .AUS_ENV)
  assign(".ICIS_VAR" , c("id", "gender", "age", "kcd", "kcd_seq", "main_yn", "hos_day", "sur_cnt", "elp_day"), envir = .AUS_ENV)
  assign(".RULE_VAR" , c("diz_cd", "diz_nm", "order", "decl_yn", "age_min", "age_max", "elp_day_min", "elp_day_max","sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max", "recur", "recover", "treat", "severe", "cause", "life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving", "pass"), envir = .AUS_ENV)
  assign(".CLAIM_COLUMNS", c("id", "kcd0", "kcd1", "kcd2", "kcd3", "kcd4", "inq_date", "clm_date", "hos_sdate", "hos_edate", "hos_day", "hos_cnt", "out_cnt", "sur_cnt"))
}
