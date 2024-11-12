

treatment_1 <- read_csv("data/mnl_output/mnl_4_untreated_estimates.csv")

treatment_1 %>% 
  select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>%
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) %>% 
  write_csv("data/output/mnl_4_treatment_1_table.csv")

treatment_2 <- read_csv("data/mnl_output/mnl_4_treated_estimates.csv")

treatment_2 %>% 
  select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>%
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) %>% 
  write_csv("data/output/mnl_4_treatment_2_table.csv")