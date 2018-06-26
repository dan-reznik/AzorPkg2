# (C) 2018 Azor Diagnostics

#' @importFrom purrr transpose discard prepend
#' @importFrom purrr map map2 map2_lgl map2_dfr map_chr
#' @importFrom magrittr %>%
#' @importFrom dplyr filter between select pull mutate
#' @importFrom assertthat assert_that
#' @importFrom stringr str_to_upper str_to_lower str_trim str_c
#' @importFrom tidyr unnest spread
#' @importFrom tibble as_tibble data_frame
#' @importFrom lubridate ymd
#' @importFrom rlang enquo

# git remote set-url origin https://github.com/dan-reznik/AzorPkg.git
# 

validate_sex <- function(sex,
                         # can be vectorized
                         value,
                         val_min_hom,val_max_hom,
                         val_min_mul,val_max_mul) {
  val_min <- NA_real_
  val_max <- NA_real_
  sex <- str_to_upper(sex)
  if(!is.na(sex)&(sex%in%c("M","F"))) {
    if(sex=="M") {
      val_min <- val_min_hom
      val_max <- val_max_hom
    } else { # "F"
      val_min <- val_min_mul
      val_max <- val_max_mul
    }
  }

  if(is.na(value)|value<0|is.na(val_min)|is.na(val_max))
    list(value=value,min=val_min,max=val_max,valid=NA)
  else
    list(value=value,
         min=val_min,max=val_max,
         valid=between(value,val_min,val_max))
}

validate_age <- function(df_key_matched,sex,age_days,
                         value) {
  if(nrow(df_key_matched)==1)
    df_age_matched <- df_key_matched
  else {
    if(is.na(age_days)) return(list(value=value,min=NA,max=NA,valid=NA))
    df_age_matched <- df_key_matched %>%
      filter(map2_lgl(age_min_days,age_max_days,
                      ~between(age_days,.x,.y)))
    }
  assert_that(nrow(df_age_matched)==1)
  # there should be only one line here
  validate_sex(sex,
               value,
               val_min_hom=df_age_matched$homem_min,
               val_max_hom=df_age_matched$homem_max,
               val_min_mul=df_age_matched$mulher_min,
               val_max_mul=df_age_matched$mulher_max)
}

validate_exam_result <- function(sex,
                                 age_in_days_at_exam,
                                 exam_id_valor,
                                 exam_value) {
  if(is.null(exam_id_valor)|is.na(exam_id_valor))
    list(value=exam_value,min=NA,max=NA,valid=NA)
  else {
    # exam_key <- exam_key %>% str_trim %>% str_to_lower
    df_chave <- df_ref_rules %>% # global
      filter(id_valor==exam_id_valor)
    if(nrow(df_chave)==0)
      list(value=exam_value,min=NA,max=NA,valid=NA)
    # '{"min":null,"max":null,"valid":null}'
    else
      validate_age(df_chave,sex,age_in_days_at_exam,exam_value)
  }
}

#' @export
validate_exams <- function(sex,
                           birth_ymd,exam_ymd,
                           exam_id_vec,exam_value_vec) {
  #to do:
  dl <- length(exam_id_vec)-length(exam_value_vec)
  if(dl>0) exam_id_vec <- exam_id_vec[1:length(exam_value_vec)]
  else if (dl<0)
    exam_value_vec <- exam_value_vec[1:length(exam_id_vec)]
  #else if(dl<0) exam_value_vec <- exam_value_vec[1:(length(exam_value_vec)+dl)]
  age_in_days_at_exam <- as.integer(ymd(exam_ymd)-ymd(birth_ymd))
  # deletes from both vectors any NA id's
  na_vec <- is.na(exam_id_vec)

  exam_value_vec <- exam_value_vec[!na_vec]
  exam_id_vec <- exam_id_vec[!na_vec]

  map2_dfr(exam_id_vec,exam_value_vec,
          ~c(list(id_valor=.x),
             validate_exam_result(sex,age_in_days_at_exam,.x,.y)))
}

#' @export
invalid_exams <- function(sex,birth_ymd,exam_ymd,
                          exam_id_vec,exam_value_vec)
  validate_exams(sex,
                 birth_ymd,exam_ymd,
                 exam_id_vec,exam_value_vec) %>%
  filter(!valid) %>% select(-valid)

#teste
# AzorPkg::invalid_exams("M","19660720","20180104",
# exame_ale_20180104$id_valor,
# exame_ale_20180104$valor_numerico) %>%
# pull(id_valor) %>%
# map_chr(id_to_chave)

#' @export
chave_to_id <- function(chave) df_ref_dict %>%
  filter(descr_valor==(chave %>% str_trim %>% str_to_lower)) %>%
  pull(id_valor)

#' @export
id_to_chave <- function(id) df_ref_dict %>%
  filter(id_valor==id) %>%
  pull(descr_valor)

#' @export
id_to_snippet <- function(id) {
  df_ref_dict %>%
    filter(id_valor%in%id) %>%
    pull(imagem_referencia) %>%
    str_c("https://dan-reznik.ocpu.io/AzorPkg2/",.)
}

#' @export
test_df <- function() data_frame(x=c(1,2),y=c(3,NA),z=c(4,NA),w=c(5,NA))
