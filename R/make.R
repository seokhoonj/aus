

mk_rule <- function(kcd, ...) {
  return(expand.grid(list(kcd, ...)))
}

mk_standard_rule <- function(kcd, decl = c("0", "1"), age = "any", elapsed = c("0-30", "31-1825"),
                             surgery = "any", hospitalization = "any", relapse = "any",
                             recovery = c("치료중", "장애또는합병증", "완치"),
                             treatment = "any", severity = "any", cause = "any",
                             decision = c("life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving")) {
  dt <- expand.grid(
    kcd = kcd,
    decl = decl,
    age = age,
    elapsed = elapsed,
    surgery = surgery,
    hospitalization = hospitalization,
    relapse = relapse,
    recovery = recovery,
    treatment = treatment,
    severity = severity,
    cause = cause
  )
  data.table::setDT(dt)
  lapply(decision, function(x) data.table::set(dt, j = x, value = ""))
  return(dt)
}
