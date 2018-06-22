# Master Exam Table
df_ref_dict <- readr::read_rds("data-raw/df_ref_dict.rds")
# Business Rules (exam,sex,age)=>val_min,val_max
df_ref_rules <- readr::read_rds("data-raw/df_ref_rules.rds")

devtools::use_data(df_ref_dict,df_ref_rules,
                   overwrite = T,internal = T)
