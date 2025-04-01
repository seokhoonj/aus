
#' @export
rule_plot <- function(rule, diz_cd,
                      decl_var = c("decl_yn", "age", "elp_day", "sur_cnt", "hos_day", "recur", "recover", "treat", "severe", "cause", "out_day"),
                      deci_var,
                      labelnode = list(decl_yn = c("N" = 0, "Y" = 1)),
                      prune = list(), keep = list(),
                      showuniform = TRUE, showlegend = FALSE, horiz = TRUE,
                      splitwidth = 20, vsplitwidth = 8, font = "Comic Sans MS",
                      varnamebold = TRUE, legendpointsize = 14, pngknit = TRUE) {
  jaid::assert_class(rule, "data.frame")
  if (missing(deci_var))
    deci_var <- local(.DECI_VAR, envir = .AUS_ENV)
  code <- title <- diz_cd
  rule_part <- rule[rule$diz_cd == code,]
  if (!showuniform) {
    lvls <- sapply(rule_part, jaid::unilen)
    uniq <- names(lvls[lvls == 1L])
    decl_var <- setdiff(decl_var, uniq)
  }
  vars <- jaid::match_cols(rule_part, c(decl_var, deci_var))
  vars <- paste(vars, collapse = " ")
  name <- rule_part$diz_nm[1L]
  if (!is.null(name))
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

#' @export
srule_plot <- function(rule, diz_cd,
                       decl_var = c("decl_yn", "age", "elp_day", "sur_cnt", "hos_day", "recur", "recover", "treat", "severe", "cause", "out_day"),
                       deci_var,
                       labelnode = list(decl = c("N" = 0, "Y" = 1)),
                       prune = list(), keep = list(),
                       showuniform = TRUE, showlegend = FALSE, horiz = TRUE,
                       splitwidth = 20, vsplitwidth = 8, font = "Comic Sans MS",
                       varnamebold = TRUE, legendpointsize = 14) {
  jaid::assert_class(rule, "data.frame")
  if (missing(deci_var))
    deci_var <- local(.DECI_VAR, envir = .AUS_ENV)
  code <- title <- diz_cd
  rule_part <- rule[rule$diz_cd == code,]
  if (!showuniform) {
    lvls <- sapply(rule_part, jaid::unilen)
    uniq <- names(lvls[lvls == 1L])
    decl_var <- setdiff(decl_var, uniq)
  }
  vars <- jaid::match_cols(rule_part, c(decl_var, deci_var))
  vars <- paste(vars, collapse = " ")
  name <- rule_part$diz_nm[1L]
  if (!is.null(name))
    title <- sprintf("%s (%s)", code, name)
  vtree::svtree(rule_part, vars = vars, horiz = horiz, title = title,
                prune = prune, keep = keep, labelnode = labelnode,
                splitwidth = splitwidth, vsplitwidth = vsplitwidth,
                showpct = FALSE, showcount = FALSE, showrootcount = FALSE,
                showlegend = showlegend, showlpct = FALSE,
                showlegendsum = FALSE, shownodelabels = TRUE,
                varnamebold = varnamebold, legendpointsize = legendpointsize,
                font = font, maxNodes = 1e5)
}
