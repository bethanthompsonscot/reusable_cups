##################################################################################################
############################## Create LC Table Appendix D For Paper ##############################
##################################################################################################

final_four_option_latent_class <- read_csv("data/mnl_output/lc-4choice-2classes-treatment-Q1x7-Q3x7-Q3x3-Q3x9_10-socio_estimates.csv")

final_three_option_latent_class <- read_csv("data/mnl_output/lc-3choice-2classes-treatment-Q1x7-Q3x7-Q3x3-Q3x9_10_socio_estimates.csv")

final_four_option_latent_class_df <- final_four_option_latent_class %>% 
  select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>% 
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) 

final_three_option_latent_class_df <- final_three_option_latent_class %>% 
  select(...1, Estimate, Rob.std.err., `Rob.p-val(0)`) %>% 
  rename(Attribute = ...1,
         Robust_Standard_Error = Rob.std.err.,
         Robust_p_value = `Rob.p-val(0)`) 

final_results_combined_df <- final_four_option_latent_class_df %>% 
  left_join(final_three_option_latent_class_df, by = "Attribute") %>% print(n = 40)

final_results_combined_df %>%  
  mutate(Attribute = case_when(
    Attribute == "m1_DIS" | Attribute == "m2_DIS" ~ "Disposable",
    Attribute == "m1_RET" | Attribute == "m2_RET" ~ "Returnable",
    Attribute == "m1_REF" | Attribute == "m2_REF" ~ "Refillable", 
    Attribute == "m1_OUT" | Attribute == "m2_OUT" ~ "No Purchase",
    Attribute == "m1_CHA_DIS" | Attribute == "m2_CHA_DIS" ~ "Charge",
    Attribute == "m1_DSC_RET" | Attribute == "m2_DSC_RET" ~ "Deposit Type", 
    Attribute == "m1_DAM_RET" | Attribute == "m2_DAM_RET" ~ "Deposit Amount", 
    Attribute == "m1_DIS_RET" | Attribute == "m2_DIS_RET" ~ "Discount Returnable",
    Attribute == "m1_DIS_REF" | Attribute == "m2_DIS_REF" ~ "Discount Refillable",
    Attribute == "m1_delt" ~ "Class 1 Delta", 
    Attribute == "m2_delt" ~ "Class 2 Delta", 
    Attribute == "m1_hGroup" ~ "Class 1 Treatment", 
    Attribute == "m2_hGroup" ~ "Class 2 Treatment", 
    Attribute == "m1_Q1x7" ~ "Class 1 Current Reusable Use", 
    Attribute == "m2_Q1x7" ~ "Class 2 Current Reusable Use",
    Attribute == "m1_Q3x3" ~ "Class 1 Frequency Takeaway Drink Purchase", 
    Attribute == "m2_Q3x3" ~ "Class 2 Frequency Takeaway Drink Purchase", 
    Attribute == "m1_Q3x7" ~ "Class 1 Refillable Cup Ownership and Use", 
    Attribute == "m2_Q3x7" ~ "Class 2 Refillable Cup Ownership and Use", 
    Attribute == "m1_Q3x9_10" ~ "Class 1 Returnable Use and Awareness", 
    Attribute == "m2_Q3x9_10" ~ "Class 2 Returnable Use and Awareness", 
    Attribute == "m1_Q1x2" ~ "Class 1 Age",
    Attribute == "m2_Q1x2" ~ "Class 2 Age",
    Attribute == "m1_Q1x1" ~ "Class 1 Gender", 
    Attribute == "m2_Q1x1" ~ "Class 2 Gender", 
    TRUE ~ NA_character_
  )) %>% 
  write_csv("data/output/latent_class_final_results_table.csv")