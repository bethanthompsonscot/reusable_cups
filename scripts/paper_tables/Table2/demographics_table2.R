tar_load(single_use_cups_labelled_df)
tar_load(single_use_cups_df)

##############################################################################################################
################################ Create Socio Demographic Tables ##############################################
##############################################################################################################

gender_df <- single_use_cups_labelled_df %>% 
  count(Q1x1) %>% 
  mutate(percentage = round(n / sum(n-2) * 100)) %>% 
  rename(gender = Q1x1) %>% 
  filter(gender != "Other") %>%
  mutate(Census = 
           case_when(
             gender == "Woman" ~ "51",
             gender == "Man" ~ "49", 
             TRUE ~ "No Quota"
           )) %>% 
  select(gender, percentage, Census) 

gender_df %>% write_csv("data/output/gender_df.csv")

# gender by cup group

single_use_cups_labelled_df$hGroup

gender_grouped_df <- single_use_cups_labelled_df %>% 
  filter(Q1x1 != "Other") %>%
  group_by(CupOptionGroup, hGroup) %>%
  count(Q1x1) %>% 
  mutate(percentage = round(n / sum(n-2) * 100)) %>% 
  rename(gender = Q1x1) %>% 
  mutate(Census = 
           case_when(
             gender == "Woman" ~ "51",
             gender == "Man" ~ "49", 
             TRUE ~ "No Quota"
           )) %>% 
  mutate(CupOptionGroup = 
           case_when(CupOptionGroup == "Group 1: see the full conjoint card (i.e., see the three alternatives of cups and the opt-out option)" ~ "Four Option", 
                     CupOptionGroup == "Group 2: see only the first two cup options (i.e., Single-Use Cup and Returnable cup + the opt-out option)" ~ "Three Option")) %>%
  mutate(hGroup = 
           case_when(hGroup == "Will see only CJ exercise" ~ "Untreated",
                     hGroup == "Will see Info screen (Q6x3) + CJ Exercise" ~ "Treated")) %>%
  select(CupOptionGroup, hGroup, gender, percentage, Census) 

gender_grouped_df %>% write_csv("data/output/gender_grouped_df.csv")
  
############################################################################################

age_df <- single_use_cups_df %>% 
  mutate(Age = case_when(
    hAge == 6 | hAge == 7 ~ 6, 
    TRUE ~ hAge
  )) %>%
  count(Age) %>% 
  mutate(percentage = round(n / sum(n) * 100)) %>% 
  mutate(Age = 
           case_when(
             Age == 1 ~ "18-24", 
             Age == 2 ~ "25-34",
             Age == 3 ~ "35-44", 
             Age == 4 ~ "45-54", 
             Age == 5 ~ "55-64", 
             Age == 6 ~ "65+",
             TRUE ~ NA_character_
           )) %>% 
  mutate(Census = 
           case_when(
             Age == "18-24" ~ "11", 
             Age == "25-34" ~ "16",
             Age == "35-44" ~ "15", 
             Age == "45-54" ~ "16",
             Age == "55-64" ~ "18",
             Age == "65+" ~ "24",
             TRUE ~ NA_character_
           )) %>% 
  select(Age, percentage, Census) 

age_df %>% write_csv("data/output/age_df.csv")

# grouped age 

grouped_age_df <- single_use_cups_df %>% 
  mutate(Age = case_when(
    hAge == 6 | hAge == 7 ~ 6, 
    TRUE ~ hAge
  )) %>%
  group_by(CupOptionGroup, hGroup) %>%
  count(Age) %>% 
  mutate(percentage = round(n / sum(n) * 100)) %>% 
  mutate(Age = 
           case_when(
             Age == 1 ~ "18-24", 
             Age == 2 ~ "25-34",
             Age == 3 ~ "35-44", 
             Age == 4 ~ "45-54", 
             Age == 5 ~ "55-64", 
             Age == 6 ~ "65+",
             TRUE ~ NA_character_
           )) %>% 
  mutate(Census = 
           case_when(
             Age == "18-24" ~ "11", 
             Age == "25-34" ~ "16",
             Age == "35-44" ~ "15", 
             Age == "45-54" ~ "16",
             Age == "55-64" ~ "18",
             Age == "65+" ~ "24",
             TRUE ~ NA_character_
           )) %>% 
  select(CupOptionGroup, hGroup, Age, percentage, Census) 

grouped_age_df %>% write_csv("data/output/grouped_age_df.csv")


########################################################################################

employment_df <- single_use_cups_labelled_df %>% 
  count(Q1x4)  %>% 
  rename(Employment = Q1x4) %>% 
  mutate(percentage = round(n / sum(n) * 100)) %>% 
  mutate(Census = case_when(
    Employment == "Employed (full or part-time)" ~ "57",
    Employment == "Self-employed" ~ "8",
    Employment == "Retired" ~ "16",
    Employment == "Stay-at-home parent/carer" ~ "4", 
    Employment == "Student" ~ "10", 
    Employment == "Unemployed" ~ "5", 
    Employment == "Other" ~ "No Quota",
    TRUE ~ NA_character_
  )) %>% 
  arrange(Employment) %>% 
  select(Employment, percentage, Census)

employment_df %>% write_csv("data/output/employment_df.csv")

# grouped employment

grouped_employment_df <- single_use_cups_labelled_df %>% 
  rename(Employment = Q1x4) %>% 
  mutate(Employment = case_when(
    Employment == "Other" | Employment == "Stay-at-home parent/carer" ~ "Stay-at-home parent or carer / Other",
    TRUE ~ Employment
  )) %>%
  mutate(CupOptionGroup = 
           case_when(CupOptionGroup == "Group 1: see the full conjoint card (i.e., see the three alternatives of cups and the opt-out option)" ~ "Four Option", 
                     CupOptionGroup == "Group 2: see only the first two cup options (i.e., Single-Use Cup and Returnable cup + the opt-out option)" ~ "Three Option")) %>%
  mutate(hGroup = 
           case_when(hGroup == "Will see only CJ exercise" ~ "Untreated",
                     hGroup == "Will see Info screen (Q6x3) + CJ Exercise" ~ "Treated")) %>%
  group_by(CupOptionGroup, hGroup) %>%
  count(Employment)  %>% 
  mutate(percentage = round(n / sum(n) * 100)) %>% 
  mutate(Census = case_when(
    Employment == "Employed (full or part-time)" ~ "57",
    Employment == "Self-employed" ~ "8",
    Employment == "Retired" ~ "16",
    Employment == "Stay-at-home parent or carer / Other" ~ "4 / No Quota", 
    Employment == "Student" ~ "10", 
    Employment == "Unemployed" ~ "5", 
    TRUE ~ NA_character_
  )) %>% 
  arrange(CupOptionGroup, hGroup, Employment) %>% 
  select(CupOptionGroup, hGroup, Employment, percentage, Census)

grouped_employment_df %>% write_csv("data/output/grouped_employment_df.csv")

#######################################################################################################

education_df <- single_use_cups_labelled_df %>% 
  rename(Education = Q1x3) %>% 
  mutate(Education = case_when(
    Education == "Postgraduate Degree or equivalent" | Education == "Doctorate or equivalent" ~ "Postgraduate / Doctorate",
    Education == "Incomplete Secondary Education" | Education == "Prefer not to answer" ~ "Other",
    TRUE ~ Education
  )) %>%
  count(Education)  %>% 
  mutate(percentage = round(n / sum(n) * 100)) %>% 
  select(Education, percentage) 
  

education_df %>% write_csv("data/output/education_df.csv")

# grouped education

grouped_education_df <- single_use_cups_labelled_df %>%
  rename(Education = Q1x3) %>%
  mutate(
    Education = case_when(
      Education == "Postgraduate Degree or equivalent" |
        Education == "Doctorate or equivalent" ~ "Postgraduate / Doctorate",
      Education == "Incomplete Secondary Education" |
        Education == "Prefer not to answer" ~ "Other",
      TRUE ~ Education
    )
  ) %>%
  mutate(
    CupOptionGroup =
      case_when(
        CupOptionGroup == "Group 1: see the full conjoint card (i.e., see the three alternatives of cups and the opt-out option)" ~ "Four Option",
        CupOptionGroup == "Group 2: see only the first two cup options (i.e., Single-Use Cup and Returnable cup + the opt-out option)" ~ "Three Option"
      )
  ) %>%
  mutate(
    hGroup =
      case_when(
        hGroup == "Will see only CJ exercise" ~ "Untreated",
        hGroup == "Will see Info screen (Q6x3) + CJ Exercise" ~ "Treated"
      )
  ) %>%
  group_by(CupOptionGroup, hGroup) %>%
  count(Education)  %>%
  mutate(percentage = round(n / sum(n) * 100)) %>%
  select(CupOptionGroup, hGroup, Education, percentage) 

grouped_education_df %>% write_csv("data/output/grouped_education_df.csv")

####################################################################################################
  
county_df <- single_use_cups_labelled_df %>% 
  count(Q5x2) %>% 
  mutate(percentage = round(n / 1300 * 100)) %>% 
  mutate(percentage = case_when(
    percentage == 0 ~ "< 1", 
    TRUE ~ as.character(percentage)
  ))

county_df %>% print(n = 32) %>% write_csv("data/output/county_df.csv")

single_use_cups_labelled_df %>% 
  count(Q5x2) %>% print(n = 50)

#################################################

income_df1 <- single_use_cups_labelled_df %>% 
  count(Q5x1) %>% 
  mutate(percentage = round(n / sum(n) * 100)) 
  
  
income_df1 %>% write_csv("data/output/income_df.csv")


grouped_income_df <- single_use_cups_labelled_df %>% 
  mutate(CupOptionGroup = 
           case_when(CupOptionGroup == "Group 1: see the full conjoint card (i.e., see the three alternatives of cups and the opt-out option)" ~ "Four Option", 
                     CupOptionGroup == "Group 2: see only the first two cup options (i.e., Single-Use Cup and Returnable cup + the opt-out option)" ~ "Three Option")) %>%
  mutate(
    hGroup =
      case_when(
        hGroup == "Will see only CJ exercise" ~ "Untreated",
        hGroup == "Will see Info screen (Q6x3) + CJ Exercise" ~ "Treated"
      )
  ) %>%
  group_by(CupOptionGroup, hGroup) %>%
  count(Q5x1) %>% 
  mutate(percentage = round(n / sum(n) * 100)) %>% 
  select(-n)

grouped_income_df %>% write_csv("data/output/grouped_income_df.csv")
