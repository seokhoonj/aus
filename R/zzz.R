
.AUS_ENV <- NULL
.onLoad <- function(libname, pkgname) {
  .AUS_ENV <<- new.env()
  assign(".DECL_VAR" , c("gender", "age", "rule_yn", "diz_cd", "kcd", "elp_day", "sur_cnt", "hos_day"), envir = .AUS_ENV)
  assign(".COND_VAR" , c("age_min", "age_max", "elp_day_min", "elp_day_max", "sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max"), envir = .AUS_ENV)
  assign(".DECIS_VAR", c("life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving"), envir = .AUS_ENV)
  assign(".RULE_VAR" , c("diz_cd", "diz_nm", "order", "decl_yn", "age", "elp_day", "sur_cnt", "hos_day", "out_day", "recur", "recover", "treat", "severe", "cause", "life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving"), envir = .AUS_ENV)
}
