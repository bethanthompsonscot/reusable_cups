###### extract 3 cup treated and untreated ##############
mnl_3_cup_option_int_df <- read_csv("data/mnl_output/mnl_3_int_estimates.csv")

mnl_3_cup_option_int_df <- mnl_3_cup_option_int_df %>% 
  select(...1,
         Estimate,
         Rob.std.err.,
         `Rob.p-val(0)`) %>% 
  rename(Attribute = ...1,
         Robust_SE = Rob.std.err.,
         Robust_pvalue = `Rob.p-val(0)`)

mnl_3_cup_option_int_df %>% write_rds("data/output/mnl_3_cup_option_int_df.rds")


##### 3 option z tests #######
three_full <- read_rds("data/mnl_output/mnl_3_model.rds")
three_untreated <- read_rds("data/mnl_output/mnl_3_untreated_model.rds")
three_untreated_df <- read_csv("data/mnl_output/mnl_3_untreated_estimates.csv")
three_treated <- read_rds("data/mnl_output/mnl_3_treated_model.rds")
three_treated_df <- read_csv("data/mnl_output/mnl_3_treated_estimates.csv")

three_coef1 <- three_untreated$estimate
three_se1 <- three_untreated$robse

three_coef2 <- three_treated$estimate
three_se2 <- three_treated$robse

# Calculate the differences in coefficients
three_diff_coef <- three_coef1 - three_coef2

# Calculate the standard errors of the differences
three_se_diff <- sqrt(three_se1^2 + three_se2^2)

# Calculate the z-scores
three_z_scores <- three_diff_coef / three_se_diff


three_p_values <- 2 * (1 - pnorm(abs(three_z_scores)))

# Combine results into a data frame for better presentation
three_results <- data.frame(
  Attribute = names(three_coef1),
  Full_Sample_Coeff = three_full$estimate,
  Untreated_Coeff = three_coef1,
  Untreated_Rob_SE = three_se1,
  Untreated_P_Value = three_untreated_df$`Rob.p-val(0)`,
  Treated_Coeff = three_coef2,
  Treated_Rob_SE = three_se2,
  Treated_P_Value = three_treated_df$`Rob.p-val(0)`,
  Coeff_Diff = three_diff_coef,
  SE_Diff = three_se_diff,
  Z_Score = three_z_scores,
  P_Value = three_p_values
) %>% as_tibble()

# Display the results
print(three_results)
three_results %>% write_rds("data/output/mnl_three_options_results.rds")

three_results %>% write_csv("data/output/mnl_three_options_comparison_table.csv")

#### four option z tests #############
four_full <- read_rds("data/mnl_output/mnl_4_model.rds")
four_untreated <- read_rds("data/mnl_output/mnl_4_untreated_model.rds")
four_untreated_df <- read_csv("data/mnl_output/mnl_4_untreated_estimates.csv")
four_treated <- read_rds("data/mnl_output/mnl_4_treated_model.rds")
four_treated_df <- read_csv("data/mnl_output/mnl_4_treated_estimates.csv")

four_coef1 <- four_untreated$estimate
four_se1 <- four_untreated$robse

four_coef2 <- four_treated$estimate
four_se2 <- four_treated$robse

# Calculate the differences in coefficients
four_diff_coef <- four_coef1 - four_coef2

# Calculate the standard errors of the differences
four_se_diff <- sqrt(four_se1^2 + four_se2^2)

# Calculate the z-scores
four_z_scores <- four_diff_coef / four_se_diff


four_p_values <- 2 * (1 - pnorm(abs(four_z_scores)))

# Combine results into a data frame for better presentation
four_results <- data.frame(
  Attribute = names(four_coef1),
  Untreated_Coeff = four_coef1,
  Untreated_Rob_SE = four_se1,
  Untreated_P_Value = four_untreated_df$`Rob.p-val(0)`,
  Treated_Coeff = four_coef2,
  Treated_Rob_SE = four_se2,
  Treated_P_Value = four_treated_df$`Rob.p-val(0)`,
  Coeff_Diff = four_diff_coef,
  SE_Diff = four_se_diff,
  Z_Score = four_z_scores,
  P_Value = four_p_values
) %>% as_tibble()

# Display the results
print(four_results)
four_results %>% write_rds("data/output/mnl_four_options_results.rds")

four_results %>% write_csv("data/output/mnl_four_options_results.csv")
