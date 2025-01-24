
#' @export
get_stat_no_rule <- function(applied) {
  stat_nr <- applied[grepl("NR", life_diz), .(n = data.table::uniqueN(id)), .(diz_cd)]
  data.table::setorder(stat_nr, -n, diz_cd)
  stat_nr[, prop := n / sum(n)]
  stat_nr[, diz_cd := factor(diz_cd, levels = unique(diz_cd))]
  jaid::set_stat_by(stat_nr, value_var = prop)
  return(stat_nr[])
}

#' @export
plot_stat_no_rule <- function(applied, rank = 1:20) {
  stat_nr <- applied[grepl("NR", life_diz), .(n = data.table::uniqueN(id)), .(diz_cd)]
  data.table::setorder(stat_nr, -n, diz_cd)
  stat_nr[, prop := n / sum(n)]
  stat_nr <- stat_nr[rank]
  stat_nr[, diz_cd := factor(diz_cd, levels = unique(diz_cd))]
  jaid::set_stat_by(stat_nr, value_var = prop)
  ggbar(stat_nr, x = diz_cd, y = prop, ymax = max(prop) * 1.1,
        label = sprintf("%.1f (%.1f)", round(prop * 100, 1), round(cprop * 100, 1)),
        label_size = 3, label_hjust = -.1) +
    coord_flip() +
    scale_x_limit_reverse(stat_nr$diz_cd) +
    labs(title = "'No Rule' proportion by disease codes (Rank 1-20)") +
    theme_view()
}

#' @export
get_stat_from_final <- function(final, decis_var) {
  jaid::assert_class(final, "data.table")
  if (missing(decis_var))
    decis_var <- local(.DECIS_VAR, envir = .AUS_ENV)
  final[, (decis_var) := lapply(.SD, change_stat_decis), .SDcols = decis_var]
  n <- nrow(final)
  dm <- data.table::melt(final, id.vars = "id", value.vars = decis_var,
                         variable.name = "coverage", value.name = "decis")
  ds <- dm[, .(n = .N), .(coverage, decis)]
  ds[, prop := n / sum(n), .(coverage)]
  return(ds[])
}

#' @export
plot_stat_from_final <- function(final, decis_var) {
  jaid::assert_class(final, "data.table")
  if (missing(decis_var))
    decis_var <- local(.DECIS_VAR, envir = .AUS_ENV)
  final[, (decis_var) := lapply(.SD, change_stat_decis), .SDcols = decis_var]
  n <- nrow(final)
  dm <- data.table::melt(final, id.vars = "id", value.vars = decis_var,
                         variable.name = "coverage", value.name = "decis")
  ds <- dm[, .(n = .N), .(coverage, decis)]
  ds[, prop := n / sum(n), .(coverage)]
  ggbar(ds, x = coverage, y = prop, ymax = max(prop) * 1.1, group = decis, fill = decis,
        label = sprintf("%.0f", prop * 100), label_size = 3, label_angle = 90,
        label_hjust = -.1) +
    scale_y_comma() +
    labs(title = "Exact Measurement of Auto Underwriting System",
         subtitle = sprintf("- %s Unit Test", jaid::as_comma(n))) +
    theme_view(x.angle = 90, y.size = 0, legend.position = "top", legend.justification = "left") +
    theme(legend.title = element_blank())
}

#' @export
plot_stat_from_final_temp <- function(final, decis_var) {
  jaid::assert_class(final, "data.table")
  if (missing(decis_var))
    decis_var <- local(.DECIS_VAR, envir = .AUS_ENV)
  final[, (decis_var) := lapply(.SD, change_stat_decis), .SDcols = decis_var]
  n <- nrow(final)
  dm <- data.table::melt(final, id.vars = "id", value.vars = decis_var,
                         variable.name = "coverage", value.name = "decis")
  ds <- dm[, .(n = .N), .(coverage, decis)]
  ds[, prop := n / sum(n), .(coverage)]
  ggbar(ds, x = coverage, y = prop, ymax = max(prop) * 1.1, group = decis, fill = decis) +
    scale_y_comma() +
    labs(title = "Exact Measurement of Auto Underwriting System",
         subtitle = sprintf("- %s Unit Test", jaid::as_comma(n))) +
    theme_view(x.angle = 90, y.size = 0, legend.position = "top", legend.justification = "left") +
    theme(legend.title = element_blank())
}
