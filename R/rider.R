#' @export
set_rider_category <- function(df, rider) {
  rider <- rlang::as_string(rlang::ensym(rider))
  df[, main_category := ifelse(
    grepl("사망", get(rider)), "life", ifelse(
      grepl("정기", get(rider)), "term", ifelse(
        grepl("장해", get(rider)), "dis", ifelse(
          grepl("진단", get(rider)), "dia", ifelse(
            grepl("입원", get(rider)), "hos", ifelse(
              grepl("수술", get(rider)), "sur", ifelse(
                grepl("통원", get(rider)), "out", ifelse(
                  grepl("치료", get(rider)), "trt", ifelse(
                    grepl("골절", get(rider)), "frac", ifelse(
                      grepl("간병", get(rider)), "nursing", ifelse(
                        grepl("의료비", get(rider)), "mr", ifelse(
                          grepl("장기요양", get(rider)), "ltc", ifelse(
                            grepl("산정특례", get(rider)), "sp_calc", ifelse(
                              grepl("납입면제", get(rider)), "waiver", NA)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )]
  df[, rider_category := ifelse(
    grepl("사망", get(rider)), "life", ifelse(
      grepl("암", get(rider)), "can", ifelse(
        grepl("뇌", get(rider)), "brain", ifelse(
          grepl("심", get(rider)), "heart", ifelse(
            grepl("남성질환|남성특정질환", get(rider)), "men", ifelse(
              grepl("여성질환|여성특정질환", get(rider)), "women", ifelse(
                grepl("치매", get(rider)), "dementia", NA)
            )
          )
        )
      )
    )
  )]
}
