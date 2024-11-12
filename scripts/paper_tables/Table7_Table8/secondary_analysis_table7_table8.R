library(broom)

conditionals_four_option <- read_csv("data/mnl_output/4choice_latent_class_posterior_probabilities.csv")

conditionals_three_option <- read_csv("data/mnl_output/3choice_latent_class_posterior_probabilities.csv")

tar_load(pmt_variables)
tar_load(latent_variables)
tar_load(hedon_util_df)

##### create tables for secondary analysis

four_option_secondary_df <- conditionals_four_option %>% 
  left_join(pmt_variables, by = c("ID" = "record")) %>% 
  left_join(latent_variables, by = c("ID" = "record")) %>% 
  left_join(hedon_util_df, by = c("ID" = "record")) %>% 
  mutate(sev_vul_mean = rowMeans(across(c("sev_Q4x1_mean", "vul_Q4x2_mean")), na.rm = TRUE)) %>%
  #mutate(X2_transformed = if_else(X2 == 0, epsilon, if_else(X2 == 1, 1 - epsilon, X2))) %>%
  mutate(X2_binary = case_when(X2 < 0.5 ~ 0, 
                               X2 >= 0.5 ~ 1))

three_option_secondary_df <- conditionals_three_option %>% 
  left_join(pmt_variables, by = c("ID" = "record")) %>% 
  left_join(latent_variables, by = c("ID" = "record")) %>% 
  left_join(hedon_util_df, by = c("ID" = "record")) %>% 
  mutate(sev_vul_mean = rowMeans(across(c("sev_Q4x1_mean", "vul_Q4x2_mean")), na.rm = TRUE)) %>%
  #mutate(X2_transformed = if_else(X2 == 0, epsilon, if_else(X2 == 1, 1 - epsilon, X2))) %>%
  mutate(X2_binary = case_when(X2 < 0.5 ~ 0, 
                               X2 >= 0.5 ~ 1))

# Define the variables to test
variables_to_test <- c("sev_Q4x1_mean", 
                       "vul_Q4x2_mean", 
                       "mal_Q4x3_mean", 
                       "self_eff_Q4x4_mean", 
                       "dis_ref_Q4x6_mean", 
                       "dis_ret_Q4x7_mean", 
                       "resp_eff_Q4x8_mean",
                       "hedonic")

# check the t-test process.

sev_Q4x1_mean1 <- four_option_secondary_df %>% 
  filter(X2_binary == 1) %>% 
  dplyr::select(sev_Q4x1_mean)

sev_Q4x1_mean2 <- four_option_secondary_df %>% 
  filter(X2_binary == 0) %>% 
  dplyr::select(sev_Q4x1_mean)

t.test(sev_Q4x1_mean1, sev_Q4x1_mean2)

######################################################################################
########################## Four Cup Option ###########################################
###################################################################################

# Perform t-tests to loop over variables
final_four_option_result <- map_dfr(variables_to_test, ~ {
  t_test <- t.test(reformulate("X2_binary", response = .x), data = four_option_secondary_df)
  tidy(t_test) %>% 
    mutate(variable = .x)  
})

final_four_option_result <- final_four_option_result %>% 
  dplyr::select(variable,
                estimate1,
                estimate2,
                estimate,
                statistic,
                parameter,
                p.value)

final_four_option_result %>% write_csv("data/output/four_option_t_tests_secondary_analysis.csv")


###################################################################################################
############################### Three Option Result ###############################################
###################################################################################################

final_three_option_result <- map_dfr(variables_to_test, ~ {
  t_test <- t.test(reformulate("X2_binary", response = .x), data = three_option_secondary_df)
  tidy(t_test) %>% 
    mutate(variable = .x)  
})

final_three_option_result <-
  final_three_option_result %>% dplyr::select(variable,
                                              estimate1,
                                              estimate2,
                                              estimate,
                                              statistic,
                                              parameter,
                                              p.value)

final_three_option_result %>% write_csv("data/output/three_option_t_tests_secondary_analysis.csv")
