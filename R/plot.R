
#' @export
ruleplot <- function(rule, code,
                     vars = c("decl", "age", "elapsed", "sur_cnt", "hos_days", "reccur", "recovery", "treatment", "sever", "cause", "out_days"),
                     decision = c("life_diz", "life_acc", "dis_diz", "dis_acc", "brain", "heart", "cancer", "hos_diz", "sur_diz", "hos_acc", "sur_acc", "mr_diz", "mr_acc", "ltc1", "ltc2", "saving"),
                     labelnode = list(decl = c("미고지" = 0, "고지" = 1)),
                     prune = list(), keep = list(),
                     showuniform = TRUE, showlegend = FALSE, horiz = TRUE,
                     splitwidth = 20, vsplitwidth = 8, font = "Comic Sans MS",
                     varnamebold = TRUE, legendpointsize = 14, pngknit = TRUE) {
  jaid::assert_class(rule, "data.frame")
  rule_part <- rule[rule$diz_cd == code,]
  if (!showuniform) {
    lvls <- sapply(rule_part, jaid::unilen)
    uniq <- names(lvls[lvls == 1L])
    vars <- setdiff(vars, uniq)
  }
  vars <- c(vars, decision)
  vars <- paste(vars, collapse = " ")
  name <- rule_part$diz_nm[1L]
  title <- sprintf("%s (%s)", code, name)
  vtree::vtree(rule_part, vars = vars, horiz = horiz, title = title,
               prune = prune, keep = keep, labelnode = labelnode,
               splitwidth = splitwidth, vsplitwidth = vsplitwidth,
               showpct = FALSE, showcount = FALSE, showrootcount = FALSE,
               showlegend = showlegend, showlpct = FALSE,
               showlegendsum = FALSE, shownodelabels = TRUE,
               varnamebold = varnamebold, legendpointsize = legendpointsize,
               font = font, pngknit = pngknit, maxNodes = 1e5)
}
