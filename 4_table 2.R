### Author: Lu Tian
### Date: Sep 26, 2024
### Project name: The Role of Social Ties in the Cognitive Function of Informal Caregivers:
### Findings from a Population-Based Propensity-Matched Analysis

### Part 4/4
# This is for Table 2 preparation.

###################################################################################################
# Table 2
###################################################################################################

# Variable names for the 'Term' column
variable_names <- c(
  "(Intercept)", "Caregiving (r13g198)", "Age", "Women", "Black", 
  "Hispanic", "Other", "Separated/Divorced", "Widowed", "Never Married",
  "High School", "Some College", "College", "Income (log)",
  "High BP", "Diabetes", "Cancer", "Heart Disease", "Stroke", "CESD"
)

# Function to add stars based on p-value significance levels
add_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

# Create the 'model1' data frame with estimates, standard errors, and p-values
model1 <- data.frame(
  Term = variable_names,  
  Estimate = as.numeric(summary_ols1_psm$estimate),
  Std_Error = as.numeric(summary_ols1_psm$std.error),
  p_value = as.numeric(summary_ols1_psm$p.value)
)

# Add stars to the estimates based on p-values and round values
model1 <- data.frame(
  Term = model1$Term,  
  Estimate_with_Stars = paste0(
    round(model1$Estimate, 3),  
    add_stars(model1$p_value)
  ),
  Std_Error = round(model1$Std_Error, 3)
)

# Format the output with estimates and standard errors underneath
model1 <- data.frame(
  Term = rep(model1$Term, each = 2),  
  Value = unlist(lapply(1:nrow(model1), function(i) {
    c(model1$Estimate_with_Stars[i], paste0("(", model1$Std_Error[i], ")"))
  })),
  stringsAsFactors = FALSE
)

# Print the new formatted table
print(model1)
write.csv(model1, file = "model1_output.csv", row.names = FALSE)


# Variable names for model2 (assuming they are the same or different based on your context)
variable_names_model2 <- c(
  "(Intercept)", "Caregiving (r13g198)", "Age", "Women", "Black", 
  "Hispanic", "Other", "Separated/Divorced", "Widowed", "Never Married",
  "High School", "Some College", "College", "Income (log)",
  "High BP", "Diabetes", "Cancer", "Heart Disease", "Stroke", "CESD"
)

# Create the 'model2' data frame with estimates, standard errors, and p-values from summary_ties_psm
model2 <- data.frame(
  Term = variable_names_model2,  
  Estimate = as.numeric(summary_ties_psm$estimate),  # Ensure numeric values
  Std_Error = as.numeric(summary_ties_psm$std.error),  # Ensure numeric values
  p_value = as.numeric(summary_ties_psm$p.value)  # Ensure numeric values
)

# Add stars to the estimates based on p-values
model2 <- data.frame(
  Term = model2$Term,  
  Estimate_with_Stars = paste0(
    round(model2$Estimate, 3),  # Round the coefficient
    add_stars(model2$p_value)   # Add stars based on significance levels
  ),
  Std_Error = round(model2$Std_Error, 3)  # Round the standard errors
)

# Format the output with estimates and standard errors underneath
model2 <- data.frame(
  Term = rep(model2$Term, each = 2),  # Repeat each term twice
  Value = unlist(lapply(1:nrow(model2), function(i) {
    # First row for estimate, second row for SE
    c(model2$Estimate_with_Stars[i], paste0("(", model2$Std_Error[i], ")"))
  })),
  stringsAsFactors = FALSE
)

# Print the new formatted table for model2
print(model2)
write.csv(model2, file = "model2_output.csv", row.names = FALSE)






# Variable names for model3 (assuming they are the same or based on your context)
variable_names_model3 <- c(
  "(Intercept)", "Caregiving (r13g198)", "ties", "Age", "Women", "Black", 
  "Hispanic", "Other", "Separated/Divorced", "Widowed", "Never Married",
  "High School", "Some College", "College", "Income (log)",
  "High BP", "Diabetes", "Cancer", "Heart Disease", "Stroke", "CESD"
)

# Create the 'model3' data frame with estimates, standard errors, and p-values from summary_ols1_f
model3 <- data.frame(
  Term = variable_names_model3,  
  Estimate = as.numeric(summary_ols1_f$estimate),  # Ensure numeric values
  Std_Error = as.numeric(summary_ols1_f$std.error),  # Ensure numeric values
  p_value = as.numeric(summary_ols1_f$p.value)  # Ensure numeric values
)

# Add stars to the estimates based on p-values
model3 <- data.frame(
  Term = model3$Term,  
  Estimate_with_Stars = paste0(
    round(model3$Estimate, 3),  # Round the coefficient
    add_stars(model3$p_value)   # Add stars based on significance levels
  ),
  Std_Error = round(model3$Std_Error, 3)  # Round the standard errors
)

# Format the output with estimates and standard errors underneath
model3 <- data.frame(
  Term = rep(model3$Term, each = 2),  # Repeat each term twice
  Value = unlist(lapply(1:nrow(model3), function(i) {
    # First row for estimate, second row for SE
    c(model3$Estimate_with_Stars[i], paste0("(", model3$Std_Error[i], ")"))
  })),
  stringsAsFactors = FALSE
)

# Print the new formatted table for model3
print(model3)

# Create a data frame with the estimates and standard errors combined into one column
# Write the model3 data frame to a CSV file
write.csv(model3, file = "model3_output.csv", row.names = FALSE)











# Merging models by term (join by Term column)
combined_table <- full_join(model1, model2, by = "Term")
combined_table <- full_join(combined_table, model3, by = "Term")

# Check for missing values
print(any(is.na(combined_table)))  # TRUE means missing values are present

# Fill missing values with empty strings or a placeholder
combined_table[is.na(combined_table)] <- ""

# Function to add significance stars
add_stars <- function(p) {
  ifelse(as.numeric(p) < 0.001, "***", ifelse(as.numeric(p) < 0.01, "**", ifelse(as.numeric(p) < 0.05, "*", "")))
}

# Adding stars to the estimates based on p-values
combined_table <- combined_table %>%
  mutate(
    Estimate_Model_1 = paste0(round(as.numeric(Estimate_Model_1), 3), add_stars(p.value_Model_1)),
    Estimate_Model_2 = paste0(round(as.numeric(Estimate_Model_2), 3), add_stars(p.value_Model_2)),
    Estimate_Model_3 = paste0(round(as.numeric(Estimate_Model_3), 3), add_stars(p.value_Model_3)),
    Std.Error_Model_1 = paste0("(", round(as.numeric(Std.Error_Model_1), 3), ")"),
    Std.Error_Model_2 = paste0("(", round(as.numeric(Std.Error_Model_2), 3), ")"),
    Std.Error_Model_3 = paste0("(", round(as.numeric(Std.Error_Model_3), 3), ")")
  )

# Select and reorder columns for better formatting
formatted_table <- combined_table %>%
  select(Term, Estimate_Model_1, Std.Error_Model_1, Estimate_Model_2, Std.Error_Model_2, Estimate_Model_3, Std.Error_Model_3)

# Debug: Check the dimensions of the table
print(dim(formatted_table))  # Make sure it has 7 columns

# Debug: Check the column names supplied to kable
col_names <- c("Term", "Model 1 Estimate (SE)", "Model 2 Estimate (SE)", "Model 3 Estimate (SE)")
print(length(col_names))  # Ensure the number of column names matches the number of columns in the table

# Create a publication-quality HTML table using kableExtra
formatted_table %>%
  kable(format = "html", escape = FALSE, align = "c", col.names = col_names) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  cat(file = "improved_regression_results.html")
