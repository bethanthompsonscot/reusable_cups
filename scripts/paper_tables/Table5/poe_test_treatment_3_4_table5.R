library(MASS)
library(Matrix)
library(dplyr)
library(readr)
library(tidyr)

# Load your data
treatment_1_df <- read_rds("data/mnl_output/mnl_3_untreated_model.rds")
treatment_1_covar_df <- read_csv("data/mnl_output/mnl_3_untreated_covar.csv") %>% 
  dplyr::select(-`...1`) 
treatment_2_df <- read_rds("data/mnl_output/mnl_3_treated_model.rds")
treatment_2_covar_df <- read_csv("data/mnl_output/mnl_3_treated_covar.csv") %>% 
  dplyr::select(-`...1`) 

# Convert to matrices and find the nearest positive definite matrices
covar_1 <- as.matrix(treatment_1_covar_df)
cov_matrix_group1 <- as.matrix(nearPD(covar_1)$mat)

covar_2 <- as.matrix(treatment_2_covar_df)
cov_matrix_group2 <- as.matrix(nearPD(covar_2)$mat)

# Extract the coefficients 
coefficients_1 <- treatment_1_df$estimate[2:7]
coefficients_2 <- treatment_2_df$estimate[2:7]

# Number of simulations and tests
n_sim <- 1000

# Custom function for two-sided Poe test with detailed proportions
two_sided_poe <- function(x, y) {
  m <- length(x)
  n <- length(y)
  
  # Calculate all pairwise differences between x and y
  pairwise_diff <- outer(x, y, "-")
  
  # Calculate proportions for positive and negative differences
  proportion_pos <- sum(pairwise_diff > 0) / (m * n)
  proportion_neg <- sum(pairwise_diff < 0) / (m * n)
  
  # Two-sided proportion is the sum of both positive and negative proportions
  proportion <- proportion_pos + proportion_neg
  
  # Calculate p-value for the two-sided test
  p_value <- 2 * min(proportion_pos, proportion_neg)
  
  return(list(proportion = proportion, p_value = p_value, proportion_pos = proportion_pos, proportion_neg = proportion_neg))
}

# Initialize the results data frame with columns for both tests
results <- data.frame(
  Coefficient = names(coefficients_1), 
  TwoSided_Proportion = NA, 
  TwoSided_PValue = NA, 
  Proportion_Pos = NA, 
  Proportion_Neg = NA,
  Mded_Proportion = NA,
  Mded_PValue = NA
)

# Loop through each coefficient and perform both tests
for (i in 1:length(coefficients_1)) {
  # Krinsky and Robb method
  set.seed(123)
  simulated_coefficients_group1 <- mvrnorm(n = n_sim, mu = coefficients_1, Sigma = cov_matrix_group1)
  simulated_coefficients_group2 <- mvrnorm(n = n_sim, mu = coefficients_2, Sigma = cov_matrix_group2)
  
  # Extract estimates for the current coefficient
  estimates_group1 <- simulated_coefficients_group1[, i]
  estimates_group2 <- simulated_coefficients_group2[, i]
  
  # Perform the two-sided Poe test
  two_sided_result <- two_sided_poe(estimates_group1, estimates_group2)
  
  # Perform the regular Poe test using mded
  mded_result <- mded(estimates_group1, estimates_group2)
  
  # Store the results from the two-sided test
  results$TwoSided_Proportion[i] <- two_sided_result$proportion
  results$TwoSided_PValue[i] <- two_sided_result$p_value
  results$Proportion_Pos[i] <- two_sided_result$proportion_pos
  results$Proportion_Neg[i] <- two_sided_result$proportion_neg
  
  # Store the results from the mded test
  results$Mded_Proportion[i] <- mded_result$stat
  # The p-value for mded can be considered as `1 - mded_result$stat` for a one-sided test
  results$Mded_PValue[i] <- 1 - mded_result$stat
}

# Print and save the results
print(results)
results %>% write_csv("data/output/treatment_comparison_3_4.csv")
