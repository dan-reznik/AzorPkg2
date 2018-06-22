# (C) 2018 Azor Diagnostics

# library(magrittr)
# library(dplyr)
# library(purrr)
#library(tibble)
# library(stringr)
# library(readr)
# library(tidyr)
# library(lubridate)
# library(jsonlite)
# library(assertthat)

#' @importFrom purrr transpose discard map2 prepend map2_lgl map
#' @importFrom magrittr %>%
#' @importFrom dplyr filter between select pull mutate
#' @importFrom assertthat assert_that
#' @importFrom stringr str_to_upper str_to_lower str_trim
#' @importFrom tidyr unnest spread
#' @importFrom tibble as_tibble data_frame
#' @importFrom lubridate ymd
#' @importFrom rlang enquo

# xxx
# git remote set-url origin https://github.com/dan-reznik/AzorPkg.git

# Test integrity of data: age_min_days and age_max_day is NA in exams with single row (n==1), and that there are no NAs in exams with more than one age range (n>1)
#
# df_ref_test <- df_ref_rules %>%
#   group_by(Chave) %>%
#   summarise(n=if_else(n()==1,"1",">1"),
#             na_count_age_min=sum(is.na(age_min_days)),
#             na_count_age_max=sum(is.na(age_max_days))) %>%
#   select(-Chave) %>%
#   count(n,na_count_age_min,na_count_age_max,sort=T)
#
# assert_that(df_ref_test %>% nrow == 2)
# assert_that(all(df_ref_test$n==c("1",">1")))
# assert_that(all(df_ref_test$na_count_age_max==c(1,0)))
# assert_that(all(df_ref_test$na_count_age_min==c(1,0)))

# Unexported

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

# this won't work with openCPU because it expects a list of lists and fromJSON will produce a datafram
lol_to_df <- function(lol) lol %>%
  transpose() %>%
  as_tibble() %>%
  unnest()

validate_age <- function(df_key_matched,sex,age_days,
                         # can vectorize
                         value) {
  if(nrow(df_key_matched)==1) # age_min_days e age_max_days == NA
    df_age_matched <- df_key_matched
  else {
    if(is.na(age_days)) return(list(value=value,min=NA,max=NA,valid=NA))
    df_age_matched <- df_key_matched %>%
      filter(map2_lgl(age_min_days,age_max_days,
                      ~between(age_days,.x,.y)))
    #print(sprintf("age=%d; nrow(df_age_matched) = %d",age_days,nrow(df_age_matched)))
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
  #to do: dl <- length(exam_id_vec)-length(exam_value_vec)
  #if(dl>0) exam_value_vec <- c(exam_value_vec,rep(NA,dl))
  #else if(dl<0) exam_value_vec <- exam_value_vec[1:(length(exam_value_vec)+dl)]
  age_in_days_at_exam <- as.integer(ymd(exam_ymd)-ymd(birth_ymd))
  # deletes from both vectors any NA id's
  na_vec <- is.na(exam_id_vec)

  exam_value_vec <- exam_value_vec[!na_vec]
  exam_id_vec <- exam_id_vec[!na_vec]

  map2(exam_id_vec,exam_value_vec,
       ~c(list(id_valor=.x),validate_exam_result(sex,age_in_days_at_exam,
                                                 .x,.y))) %>%
    lol_to_df
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
  filter(descr_valor==chave %>% str_trim %>% str_to_lower) %>%
  pull(id_valor)

#' @export
id_to_chave <- function(id) df_ref_dict %>%
  filter(id_valor==id) %>%
  pull(descr_valor)

extract_list_named <- function(df, col) { # shortest most elegant fastest (see below)
  col <- enquo(col)
  df %>%
    mutate(.id=row_number(),
           .l=map(!!col,names)) %>%
    unnest(.l,!!col) %>%
    spread(.l,!!col,convert=T) %>%
    select(-.id)
}

# @export
validate_exams_df <- function(df_exam, # df_exam: id, valor
                              sex,birth_ymd,exam_date_ymd) {
  age_in_days_at_exam <- as.integer(ymd(exam_date_ymd)-ymd(birth_ymd))
  # print(age_in_days_at_exam)

  # deletes from both vectors any NA id's
  df_exam %>%
    filter(!is.na(id_valor)) %>%
    mutate(valid=map2(id_valor, value,~validate_exam_result(sex,age_in_days_at_exam,.x,.y)))  %>%
    extract_list_named(valid) %>%
    select(id_valor,min,max,value,valid)
}

# @export
invalid_exams_df <- function(df_exam,sex,birth_ymd,exam_ymd)
  validate_exams_df(df_exam,sex,birth_ymd,exam_ymd) %>%
  filter(!valid) %>% select(-valid)

# teste
# exame_ale_20180104 %>%
# select(id_valor,value=valor_numerico) %>%
# AzorPkg::invalid_exams_df("M","19660720","20180104") %>%
# pull(id_valor) %>%
# map_chr(id_to_chave)

#' @export
test_df <- function() data_frame(x=c(1,2),y=c(3,NA),z=c(4,NA),w=c(5,NA))
