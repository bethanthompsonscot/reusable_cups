library(targets)
library(tarchetypes)
source("scripts/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse",
                            "openxlsx",
                            "readxl"))

path_to_data <- function() {
  "data/single_use_cup_results.xlsx"
}

path_to_design <- function() {
  "data/design.csv"
}

list(
  # external
  tar_target(data_path,
             path_to_data(),
             format = "file"),
  tar_target(data_path_design,
             path_to_design(),
             format = "file"),
  tar_target(
    single_use_cups_df,
    read_xlsx(
      data_path,
      sheet = 1,
      col_names = T,
      na = ("99")
    ) %>% 
      filter(!is.na(CupOptionGroup))
  ),
  tar_target(
    single_use_cups_labelled_df,
    read_xlsx(data_path, sheet = 2, col_names = T) %>% 
      filter(!is.na(CupOptionGroup))
  ),
  # import apollo design framework
  # this has all attributes on one line
  tar_target(design_raw_df,
             read_csv(data_path_design)),
  # tidy up the apollo design framework
  tar_target(design_df,
             design_raw_df %>%
               create_design_df()),
  # create a df for all background variables to be used in latent class analysis
  tar_target(latent_variables, 
             create_latent_df(single_use_cups_df)),
  tar_target(pmt_variables,
             create_pmt_df(single_use_cups_df)),
  # create four option conjoint df for apollo with background variables
  tar_target(full_conjoint_all_df,
             single_use_cups_df %>% 
               filter(CupOptionGroup == 1) %>% 
               create_treatment_choice_df2() %>% 
               left_join(design_df, by = c("Card")) %>%
               left_join(hedon_util_df, by = c("record")) %>% 
               left_join(latent_variables, by = c("record")) %>% 
               left_join(pmt_variables, by = c("record")) %>% 
               select(-hGroup.y) %>% 
               rename(hGroup = hGroup.x)
  ),
  # create three option conjoint df for apollo with background with background variables
  tar_target(short_conjoint_all_df, 
             single_use_cups_df %>% 
               filter(CupOptionGroup == 2) %>% 
               create_treatment_choice_df2() %>% 
               left_join(design_df, by = c("Card")) %>%
               left_join(hedon_util_df, by = c("record")) %>% 
               left_join(latent_variables, by = c("record")) %>%
               left_join(pmt_variables, by = c("record")) %>% 
               select(-hGroup.y) %>% 
               rename(hGroup = hGroup.x)
  ),
  # treatment 1 df
  tar_target(
    full_conjoint_untreated_df,
    single_use_cups_df %>%
      filter(CupOptionGroup == 1 & hGroup == 1) %>%
      create_treatment_choice_df() %>%
      left_join(design_df, by = c("Card"))
  ),
  # treatment 2 df
  tar_target(
    full_conjoint_treated_df,
    single_use_cups_df %>%
      filter(CupOptionGroup == 1 & hGroup == 3) %>%
      create_treatment_choice_df() %>%
      left_join(design_df, by = c("Card")) %>%
      left_join(hedon_util_df, by = c("record"))
  ),
  # treatment 3 df
  tar_target(
    short_conjoint_untreated_df,
    single_use_cups_df %>%
      filter(CupOptionGroup == 2 & hGroup == 1) %>%
      create_treatment_choice_df() %>%
      left_join(design_df, by = c("Card")) %>%
      select(-DIS_REF)
  ),
  # treatment 4 df
  tar_target(
    short_conjoint_treated_df,
    single_use_cups_df %>%
      filter(CupOptionGroup == 2 & hGroup == 3) %>%
      create_treatment_choice_df () %>%
    left_join(design_df, by = c("Card")) %>%
      select(-DIS_REF) %>%
      left_join(hedon_util_df, by = c("record"))
),
# treatment 1 file
tar_target(
  write_full_untreated,
  write_csv(
    full_conjoint_untreated_df,
    "data/apollo/full_conjoint_untreated_df.csv"
  )
),
# treatment 2 file
tar_target(
  write_full_treated,
  write_csv(
    full_conjoint_treated_df,
    "data/apollo/full_conjoint_treated_df.csv"
  )
),
# treatment 3 file
tar_target(
  write_short_untreated,
  write_csv(
    short_conjoint_untreated_df,
    "data/apollo/short_conjoint_untreated_df.csv"
  )
),
# treatment 4 file
tar_target(
  write_short_treated,
  write_csv(
    short_conjoint_treated_df,
    "data/apollo/short_conjoint_treated_df.csv"
  )
),
# four option file
tar_target(
  write_full_all,
  write_csv(
    full_conjoint_all_df,
    "data/apollo/full_conjoint_all_df.csv"
  )
),
# three option file
tar_target(write_short_all,
           write_csv(
             short_conjoint_all_df, 
             "data/apollo/short_conjoint_all_df.csv"
           )))

