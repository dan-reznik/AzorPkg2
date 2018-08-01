# (C) 2018 Azor Diagnostics

#' @importFrom purrr transpose discard prepend
#' @importFrom purrr map map2 map2_lgl map2_dfr map_chr
#' @importFrom magrittr %>%
#' @importFrom dplyr rename count arrange filter between select pull mutate bind_cols everything first last
#' @importFrom assertthat assert_that
#' @importFrom stringr str_to_upper str_to_lower str_trim str_c fixed str_split
#' @importFrom tidyr unnest spread
#' @importFrom tibble as_tibble data_frame
#' @importFrom lubridate ymd
#' @importFrom rlang enquo
#' @importFrom lubridate ymd

# git remote set-url origin https://github.com/dan-reznik/AzorPkg.git
# place all whites in the current dir and sepia them via (overwrites)
# mogrify -path . -sepia-tone 80% *.png -verbose

list_valid <- function(value=NA,min=NA,max=NA,ref_png=NA,valid=NA,id_interval=NA)
  list(value=value,
       min=min,
       max=max,
       ref_png=ref_png,
       valid=valid,
       id_interval=id_interval)

validate_sex <- function(sex,
                         # can be vectorized
                         value,
                         val_min_hom,val_max_hom,
                         val_min_mul,val_max_mul,
                         png_marked_homem,png_marked_mulher,
                         id_interval) {
  val_min <- NA_real_
  val_max <- NA_real_
  sex <- str_to_upper(sex)
  if(!is.na(sex)&(sex%in%c("M","F"))) {
    if(sex=="M") {
      val_min <- val_min_hom
      val_max <- val_max_hom
      png_marked <- png_marked_homem
    } else { # "F"
      val_min <- val_min_mul
      val_max <- val_max_mul
      png_marked <- png_marked_mulher
    }
  }

  if(is.na(value)|value<0|is.na(val_min)|is.na(val_max))
    list_valid(value=value,min=val_min,max=val_max,ref_png=png_marked,id_interval=id_interval)
  else
    list_valid(value=value,
               min=val_min,max=val_max,
               ref_png=png_marked,
               valid=between(value,val_min,val_max),
               id_interval=id_interval)
}

img_path <- function(img_name) str_c("https://dan-reznik.ocpu.io/AzorPkg2/marked/",
                                     img_name,
                                     ".png")

validate_age <- function(df_key_matched,sex,age_days,
                         value) {
  if(nrow(df_key_matched)==1)
    df_age_matched <- df_key_matched
  else {
    if(is.na(age_days)) return(list_valid(value=value))
    df_age_matched <- df_key_matched %>%
      dplyr::filter(map2_lgl(age_min_days,age_max_days,
                             ~between(age_days,.x,.y)))
    }
  # there should be only one match
  assertthat::assert_that(nrow(df_age_matched)==1)
  validate_sex(sex,
               value,
               val_min_hom=df_age_matched$homem_min,
               val_max_hom=df_age_matched$homem_max,
               val_min_mul=df_age_matched$mulher_min,
               val_max_mul=df_age_matched$mulher_max,
               png_marked_homem=img_path(df_age_matched$imagem_regra_homem),
               png_marked_mulher=img_path(df_age_matched$imagem_regra_mulher),
               id_interval=df_age_matched$id_interval)
}

validate_exam_result <- function(sex,
                                 age_in_days_at_exam,
                                 exam_id_valor,
                                 exam_value) {
  if(is.null(exam_id_valor)|is.na(exam_id_valor))
    list_valid(value=exam_value)
  else {
    # exam_key <- exam_key %>% str_trim %>% str_to_lower
    df_chave <- df_ref_rules %>% # global
      dplyr::filter(id_valor==exam_id_valor)
    if(nrow(df_chave)==0)
      list_valid(value=exam_value)
    # '{"min":null,"max":null,"valid":null}'
    else
      validate_age(df_chave,sex,age_in_days_at_exam,exam_value)
  }
}

age_in_days <- function(exam_ymd, birth_ymd) as.integer(ymd(exam_ymd)-ymd(birth_ymd))

#' @export
chave_to_id <- function(chave) df_ref_dict %>%
  dplyr::filter(descr_valor%in%(chave %>% str_trim %>% str_to_lower)) %>%
  pull(id_valor)

#' @export
id_to_chave <- function(id) df_ref_dict %>%
  dplyr::filter(id_valor%in%id) %>%
  pull(descr_valor)

#' @export
id_to_snippet <- function(id) df_ref_dict %>%
  dplyr::filter(id_valor%in%id) %>%
  pull(imagem_referencia) %>%
  # todo: coluna imagem_regra contem condicao para o caso de mais de uma imagem
  # {"var":"age","cond":"<","value":16,"units":"ano"}
  # workaround: pega o primeiro. nota: esta logica belongs in validate_exams
  str_split(fixed(";")) %>%
  map_chr(first) %>%
  str_c("https://dan-reznik.ocpu.io/AzorPkg2/unmarked/",.)

#' @export
validate_exams <- function(sex,
                           birth_ymd,exam_ymd,
                           exam_id_vec,exam_value_vec
                           , ref_png=F # unused
                           ) {
  #to do:
  dl <- length(exam_id_vec)-length(exam_value_vec)
  if(dl>0) exam_id_vec <- exam_id_vec[1:length(exam_value_vec)]
  else if (dl<0)
    exam_value_vec <- exam_value_vec[1:length(exam_id_vec)]
  #else if(dl<0) exam_value_vec <- exam_value_vec[1:(length(exam_value_vec)+dl)]
  age_in_days_at_exam <- age_in_days(exam_ymd, birth_ymd)
  # deletes from both vectors any NA id's
  na_vec <- is.na(exam_id_vec)

  exam_value_vec <- exam_value_vec[!na_vec]
  exam_id_vec <- exam_id_vec[!na_vec]

  df <- data_frame(id_valor=exam_id_vec) %>%
    bind_cols(
      map2_dfr(exam_id_vec,exam_value_vec,
               ~validate_exam_result(sex,age_in_days_at_exam,.x,.y)))
  # if (ref_png)
  #   df %>% bind_cols(ref_png=id_to_snippet(exam_id_vec))
  # else
    df
}

#' @export
invalid_exams <- function(sex,birth_ymd,exam_ymd,
                          exam_id_vec,exam_value_vec)
  validate_exams(sex,
                 birth_ymd,exam_ymd,
                 exam_id_vec,exam_value_vec) %>%
  dplyr::filter(!valid) %>% select(-valid)

#' @export
patient_date_range <- function(patient_id) {
  df <- df_exames %>% filter(id_paciente==patient_id)
  ymd_min <- df$ymd%>%min
  ymd_max <- df$ymd%>%max
  list(min=ymd_min,max=ymd_max)
}

#' @export
patient_date_counts <- function(patient_id) {
  df_exames %>%
    filter(id_paciente==patient_id) %>%
    arrange(ymd) %>%
    dplyr::count(ymd) %>%
    dplyr::rename(count=n)
}

#' @export
patient_results <- function(patient_id,ymd_min=NULL,ymd_max=NULL) {
  df <- df_exames %>% filter(id_paciente==patient_id)
  if(!is.null(ymd_min))
    df <- df %>% filter(ymd>=ymd(ymd_min))
  if(!is.null(ymd_max))
    df <- df %>% filter(ymd<=ymd(ymd_max))
  df
}

#' @export
test_df <- function() data_frame(x=c(1,2),y=c(3,NA),z=c(4,NA),w=c(5,NA))
