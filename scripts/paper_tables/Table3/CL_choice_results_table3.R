#############################################################################################
######################### Create Table 3 ####################################################
#############################################################################################

four_option_basic <- read_csv("data/mnl_output/mnl_4_estimates.csv")

four_option_basic %>% 
  select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>%
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) %>% 
  write_csv("data/output/mnl_4_basic_results_table.csv")

three_option_basic <- read_csv("data/mnl_output/mnl_3_estimates.csv")

three_option_basic %>% 
  select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>%
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) %>% 
  write_csv("data/output/mnl_3_basic_results_table.csv")




