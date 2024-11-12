treatment_3 <- read_csv("data/mnl_output/mnl_3_untreated_estimates.csv")

treatment_3 %>% 
  dplyr::select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>%
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) %>% 
  write_csv("data/output/mnl_3_treatment_3_table.csv")

treatment_4 <- read_csv("data/mnl_output/mnl_3_treated_estimates.csv")

treatment_4 %>% 
  dplyr::select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>%
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) %>% 
  write_csv("data/output/mnl_4_treatment_4_table.csv")