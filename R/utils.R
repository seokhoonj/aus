
expect_cartesian_rows <- function(diz_book, rule, icis) {
  rule_n <- rule[decl_yn == 0, .(n = .N), .(diz_cd)]
  icis_n <- icis[, .(ni = .N), .(kcd)]
  icis_n[diz_book, on = .(kcd), diz_cd := diz_cd]
  icis_n = icis_n[, .(ni = sum(ni)), .(diz_cd)]
  icis_n[rule_n, on = .(diz_cd), nr := i.n]
  icis_n[is.na(nr), nr := 1]
  icis_n[, n := ni * nr]
  sum(icis_n$n)
}
