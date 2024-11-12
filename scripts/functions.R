replace_na_neutral <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~replace_na(., 3)))

}

replace_na_six <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~replace_na(., 6)))
  
}

replace_na_never <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~replace_na(., 6)))

}

reverse_and_rename_six <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~6 - ., .names = "{.col}_rev"))

}

reverse_six <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~6 - .))
  
}

reverse_and_rename_seven <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~7 - ., .names = "{.col}_rev"))
  
}

create_hedon_util_df <- function(data) {
  
  data %>% 
    select(starts_with("Q4x9"),
           record) %>% 
    pivot_longer(cols = starts_with("Q4x9")) %>% 
    mutate(hedon_util = case_when(
      name == "Q4x9r1" ~ "hedonic",
      name == "Q4x9r2" ~ "hedonic",
      name == "Q4x9r3" ~ "hedonic",
      name == "Q4x9r4" ~ "hedonic",
      name == "Q4x9r5" ~ "hedonic",
      name == "Q4x9r6" ~ "util",
      name == "Q4x9r7" ~ "util",
      name == "Q4x9r8" ~ "util",
      name == "Q4x9r9" ~ "util",
      name == "Q4x9r10" ~ "util",
      name == "Q4x9r11" ~ "hedonic", 
      TRUE ~ NA_character_
    )) %>% 
    group_by(record, hedon_util) %>% 
    summarise(mean_hedon_util_sum = mean(value)) %>% 
    pivot_wider(names_from = hedon_util, values_from = mean_hedon_util_sum)
}

create_latent_df <- function (data) {
  data %>% 
    mutate(Q3x8_combined = case_when(
      is.na(Q3x8) ~ 6,
      TRUE ~ Q3x8
    )) %>% 
    reverse_and_rename_seven(c("Q3x8_combined")) %>%
    mutate(Q3x9_10 = 
             case_when(
               is.na(Q3x10) ~ 3, # yes previously used reusable
               Q3x10 == 1 ~ 2, # yes previously aware returnable
               Q3x10 == 2 ~ 1, # no not previously aware returnable prior to 
               TRUE ~ NA_real_
             )) %>%
    reverse_six(c("Q1x7")) %>% # switch always to be five
    reverse_six(c("Q3x3")) %>% # switch more than once a day to be five
    mutate(Q3x7 = case_when( # switch so that yes, I own a reusable cup is 1 and otherwise 0
      Q3x7 == 2 ~ 0, 
      TRUE ~ Q3x7
    )) %>% 
    select(record,
           #hGroup,
           Q3x3, # frequency purchase takeaway drinks
           Q3x4, # how many average month
           Q3x6_2, # single use cup use
           Q3x7, # own refillable
           Q3x8, # use refillable
           Q3x8_combined_rev, # refillable own and frequency
           Q3x9, # previous use reusable
           Q3x10, # heard of reusable
           Q3x9_10, # used heard reusable combined
           Q1x7, # How often do you buy non-alcoholic drinks in takeaway cups where the cup is reusable? 
           Q1x2, # age
           Q1x1, # gender
           Q1x3, # education
           Q5x1, # income
           Q3x5 # purchase for who
    ) %>% 
    mutate(Q1x1 = 
             case_when(
               record == 1151 ~ 1, 
               record == 1156 ~ 2, 
               TRUE ~ Q1x1
             )) %>% 
    mutate(self_others = 
             case_when(Q3x5 == 3 ~ 2, 
                       TRUE ~ Q3x5))
}

create_pmt_df <- function(df) {
  
  df %>%
    replace_na_neutral(
      c(
        "Q4x1r1",
        "Q4x1r2",
        "Q4x1r3",
        "Q4x1r4",
        "Q4x1r5",
        "Q4x1r6",
        "Q4x1r7",
        "Q4x2r1",
        "Q4x2r2",
        "Q4x2r3",
        "Q4x2r4",
        "Q4x2r5",
        "Q4x2r6",
        "Q4x2r7"
      )
    ) %>%
    # switch the values so that 5 is highly likely and corresponds with direction of severity.
    reverse_and_rename_six(c(
        "Q4x1r1",
        "Q4x1r2",
        "Q4x1r3",
        "Q4x1r4",
        "Q4x1r5",
        "Q4x1r6",
        "Q4x1r7"
      )) %>% 
    reverse_and_rename_six(c("Q4x3r1", "Q4x3r2", "Q4x3r3", "Q4x3r4", "Q4x3r5", "Q4x3r6", "Q4x3r7")) %>% 
    reverse_and_rename_six(c("Q4x4r1", "Q4x4r3", "Q4x4r4", "Q4x4r5")) %>%
    reverse_and_rename_six(c("Q4x6r1", "Q4x6r2", "Q4x6r3", "Q4x6r4", "Q4x6r5", "Q4x6r6", "Q4x6r7", "Q4x6r8")) %>% 
    reverse_and_rename_six(c("Q4x7r1", "Q4x7r2", "Q4x7r3", "Q4x7r4", "Q4x7r5", "Q4x7r6", "Q4x7r7", "Q4x7r8")) %>% 
    mutate(sev_Q4x1_mean = rowMeans(across(starts_with("Q4x1") & ends_with("rev")), na.rm = TRUE),
           vul_Q4x2_mean = rowMeans(across(starts_with("Q4x2")), na.rm = TRUE),
           mal_Q4x3_mean = rowMeans(across(starts_with("Q4x3") & ends_with("rev")), na.rm = TRUE),
           self_eff_Q4x4_mean = rowMeans(across(c("Q4x4r1_rev", "Q4x4r2", "Q4x4r3_rev", "Q4x4r4_rev", "Q4x4r5_rev")), na.rm = TRUE),
           dis_ref_Q4x6_mean = rowMeans(across(starts_with("Q4x6") & ends_with("rev")), na.rm =TRUE),
           dis_ret_Q4x7_mean = rowMeans(across(starts_with("Q4x7") & ends_with("rev")), na.rm = TRUE),
           resp_eff_Q4x8_mean = rowMeans(across(starts_with("Q4x8") & !ends_with("rev")), na.rm = TRUE)) %>% 
    select(record, 
           contains("Q4"),
           hGroup,
           CupOptionGroup)
  
}

create_treatment_choice_df <- function(df) {
  
  df %>% 
    select(
      record,
      starts_with("C1_"),
      -C1_Version,
      -C1_Timer,
      CupOptionGroup,
      dpipeQ2x3,
      hGroup
    ) %>% 
    select(-CupOptionGroup,
           -dpipeQ2x3,
           -hGroup) %>% 
    pivot_longer(cols = -record) %>%
    mutate(name = str_remove(name, "^C1_")) %>% 
    rename(Card = name) %>% 
    rename(CHOICE = value) %>% 
    mutate(Card = as.double(Card))
  
}

create_treatment_choice_df2 <- function(df) {
  
  df %>% 
    select(
      record,
      starts_with("C1_"),
      -C1_Version,
      -C1_Timer,
      CupOptionGroup,
      dpipeQ2x3,
      hGroup
    ) %>% 
    select(-CupOptionGroup,
           -dpipeQ2x3) %>% 
    pivot_longer(cols = -c(record, hGroup)) %>%
    mutate(name = str_remove(name, "^C1_")) %>% 
    rename(Card = name) %>% 
    rename(CHOICE = value) %>% 
    mutate(Card = as.double(Card)) %>% 
    mutate(hGroup = case_when(
      hGroup == 1 ~ 0, 
      TRUE ~ 1
    )) 
}

create_design_df <- function(df) {
  
  df %>% 
    rename(DSC_RET = `Attribute 1\nDeposit scheme`,
           DAM_RET = `Attribute 2\nDeposit amount`,
           CHA_DIS = `Attribute 3\nCharge`) %>% 
    mutate(DIS_RET = case_when(
      Option == 2 ~ `Attribute 4\nDscount`,
      TRUE ~ NA
    )) %>% 
    mutate(DIS_REF = case_when(
      Option == 3 ~ `Attribute 4\nDscount`,
      TRUE ~ NA
    )) %>% 
    select(-`Attribute 4\nDscount`) %>% 
    mutate(DSC_RET = case_when(
      DSC_RET == 2 ~ 0, 
      DSC_RET == 1 ~ 1, 
      TRUE ~ NA
    )) %>% 
    mutate(DAM_RET = case_when(
      DAM_RET == 1 ~ 2,
      DAM_RET == 2 ~ 3, 
      DAM_RET == 3 ~ 4, 
      DAM_RET == 4 ~ 5, 
      DAM_RET == 5 ~ 6,
      TRUE ~ NA
    )) %>% 
    mutate(CHA_DIS = case_when(
      CHA_DIS == 1 ~ 0, 
      CHA_DIS == 2 ~ 0.15, 
      CHA_DIS == 3 ~ 0.25,
      CHA_DIS == 4 ~ 0.35, 
      CHA_DIS == 5 ~ 0.45,
      TRUE ~ NA
    )) %>% 
    mutate(DIS_RET = case_when(
      DIS_RET == 1 ~ 0, 
      DIS_RET == 2 ~ 0.15, 
      DIS_RET == 3 ~ 0.25,
      DIS_RET == 4 ~ 0.35, 
      DIS_RET == 5 ~ 0.45,
      TRUE ~ NA
    )) %>% 
    mutate(DIS_REF = case_when(
      DIS_REF == 1 ~ 0, 
      DIS_REF == 2 ~ 0.15, 
      DIS_REF == 3 ~ 0.25,
      DIS_REF == 4 ~ 0.35, 
      DIS_REF == 5 ~ 0.45,
      TRUE ~ NA
    )) %>% 
    rename(CHOICE = Option) %>%
    select(-CHOICE) %>%
    # Condense all values into one line
    pivot_longer(cols = -Card) %>%
    filter(!is.na(value)) %>%
    # Pivot wider again
    pivot_wider(names_from = name, values_from = value)
  
}