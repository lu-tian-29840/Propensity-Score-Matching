
### Author: Lu Tian
### Date: Sep 26, 2024
### Project name: The Role of Social Ties in the Cognitive Function of Informal Caregivers:
### Findings from a Population-Based Propensity-Matched Analysis


# This file contains mediation and bootstrap. mediation is based on it. 
# Step 1 After macthing
# Step 2 Before matching. Jump to line 455, this method is better for step 2!!!! 

####################################################################################################
#  After macthing
####################################################################################################
# Step 1
# Mediation as per Baron and Kenny

# Lists to store matched datasets and other model information
matched_data_list <- list()  # To store matched datasets
ols1_list <- list() 
ties_list <- list() 
ols1_f_list <- list()  # Full model
match_info <- list()  # To store matching information for each imputed dataset

# Loop through each imputed dataset
for (i in 1:10) {
  # Extract each imputed dataset
  imputed_data <- complete(psm_imputed, i)
  
  # Perform propensity score matching
  m.out <- matchit(r13g198 ~ r13cogtot + r13age + women + black + hispanic + other
                   + sepdiv + widow + nevermarr + hs + somecoll + coll
                   + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                   + r13stroke + r13cesd,
                   data = imputed_data, 
                   method = "nearest", 
                   caliper = 0.1, 
                   link = "probit", 
                   ratio = 1)
  
  # Extract matched data and store it
  mout1 <- match.data(m.out)
  matched_data_list[[i]] <- mout1  # Save the matched dataset
  
  # Count the number of treated and control units in the matched data
  num_treated <- sum(mout1$r13g198 == 1)
  num_control <- sum(mout1$r13g198 == 0)
  
  # Store matching information (treated and control counts)
  match_info[[i]] <- list(treated = num_treated, control = num_control)
  
  # OLS model for the outcome (r15cogtotp)
  ols1_psm <- lm(r15cogtotp ~ r13g198 + r13age + women + black + hispanic + other 
                 + sepdiv + widow + nevermarr + hs + somecoll + coll
                 + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
                 + r13stroke + r13cesd, 
                 data = mout1)
  
  # OLS model for the mediator (ties)
  ties_psm <- lm(ties ~ r13g198 + r13age + women + black + hispanic + other 
                 + sepdiv + widow + nevermarr + hs + somecoll + coll
                 + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
                 + r13stroke + r13cesd, 
                 data = mout1)
  
  # OLS full model
  ols1_f <- lm(r15cogtotp ~ r13g198 + ties + r13age + women + black + hispanic + other 
               + sepdiv + widow + nevermarr + hs + somecoll + coll
               + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
               + r13stroke + r13cesd, 
               data = mout1)
  
  # Store the OLS models in the respective lists
  ols1_list[[i]] <- ols1_psm
  ties_list[[i]] <-  ties_psm
  ols1_f_list[[i]] <- ols1_f 
}

# Print the number of treated and control units in each imputed dataset after matching
for (i in 1:10) {
  cat("Imputed Dataset", i, "\n")
  cat("Treated Units:", match_info[[i]]$treated, "\n")
  cat("Control Units:", match_info[[i]]$control, "\n\n")
}


# Imputed Dataset 1 - 10
# Treated Units: 1774 
# Control Units: 1774 

pooled_ols1_psm <- pool(ols1_list)

summary_ols1_psm <- summary(pooled_ols1_psm)
summary_ols1_psm$p.value <- round(summary_ols1_psm$p.value, 3)
print(summary_ols1_psm)

# term    estimate  std.error   statistic        df p.value
# 1  (Intercept) 33.00097769 1.37524994  23.9963491 1550.7852   0.000
# 2     r13g1981  0.41703857 0.14992838   2.7815853 1165.2506   0.005
# 3       r13age -0.24212935 0.01281094 -18.9002054 1585.6626   0.000
# 4        women  1.13471857 0.16274611   6.9723239 1692.8906   0.000
# 5       black1 -1.91434365 0.22398981  -8.5465657 2140.8723   0.000
# 6    hispanic1 -0.55392769 0.31019901  -1.7857172 1793.6260   0.074
# 7       other1 -0.50296951 0.48665704  -1.0335194 1760.5961   0.302
# 8       sepdiv -0.03614150 0.25622807  -0.1410521  579.3002   0.888
# 9        widow  0.19992800 0.21070168   0.9488676  806.1563   0.343
# 10   nevermarr -0.73343651 0.44647355  -1.6427323 2586.3925   0.101
# 11          hs  2.07882065 0.25715966   8.0837742 1824.9201   0.000
# 12    somecoll  3.00283497 0.26459258  11.3489009 2995.9244   0.000
# 13        coll  3.77784340 0.27638094  13.6689722 1515.1216   0.000
# 14 log_h13itot  0.45007440 0.08202128   5.4872885  967.1302   0.000
# 15    r13hibpe -0.04317617 0.17038463  -0.2534041  489.0272   0.800
# 16    r13diabe -0.51736039 0.17735929  -2.9170187  784.3379   0.004
# 17   r13cancre  0.17309545 0.19328506   0.8955449  788.2260   0.371
# 18   r13hearte  0.03440718 0.16996225   0.2024401 1265.6435   0.840
# 19   r13stroke -0.76108924 0.28292523  -2.6900720  780.1083   0.007
# 20     r13cesd -0.35200047 0.04841360  -7.2706940  566.2450   0.000


pooled_ties_psm <- pool(ties_list)
summary_ties_psm <- summary(pooled_ties_psm)
summary_ties_psm$p.value <- round(summary_ties_psm$p.value, 3)

# term     estimate  std.error   statistic       df p.value
# 1  (Intercept)  2.024756367 1.78902010  1.13176837 3525.635   0.258
# 2     r13g1981  1.393942552 0.19349564  7.20399981 3525.635   0.000
# 3       r13age  0.086185340 0.01667531  5.16844044 3525.635   0.000
# 4        women  0.549061169 0.21220825  2.58736951 3525.635   0.010
# 5       black1  1.889397949 0.29389332  6.42885639 3525.635   0.000
# 6    hispanic1  0.903261541 0.40509642  2.22974457 3525.635   0.026
# 7       other1  0.994843283 0.63522415  1.56612951 3525.635   0.117
# 8       sepdiv -0.348534713 0.32313311 -1.07861032 3525.635   0.281
# 9        widow  0.310150511 0.26886097  1.15357208 3525.635   0.249
# 10   nevermarr -1.145868686 0.58890193 -1.94577165 3525.635   0.052
# 11          hs -0.488468620 0.33598500 -1.45384058 3525.635   0.146
# 12    somecoll -1.126715227 0.35064506 -3.21326420 3525.635   0.001
# 13        coll -1.076295264 0.35931002 -2.99545015 3525.635   0.003
# 14 log_h13itot  0.016784041 0.10527021  0.15943771 3525.635   0.873
# 15    r13hibpe  0.020146742 0.21342469  0.09439743 3525.635   0.925
# 16    r13diabe -0.206385140 0.22610890 -0.91276878 3525.635   0.361
# 17   r13cancre  0.028439028 0.24645277  0.11539342 3525.635   0.908
# 18   r13hearte  0.258841086 0.21986814  1.17725598 3525.635   0.239
# 19   r13stroke -0.009035156 0.36062579 -0.02505410 3525.635   0.980
# 20     r13cesd -0.247150126 0.06100149 -4.05154220 3525.635   0.000


pooled_ols1_f <- pool(ols1_f_list)
summary_ols1_f <- summary(pooled_ols1_f)
summary_ols1_f$p.value <- round(summary_ols1_f$p.value, 3)
print(summary_ols1_f)

# term    estimate  std.error    statistic        df p.value
# 1  (Intercept) 32.93941144 1.37330751  23.98545928 1603.3668   0.000
# 2     r13g1981  0.37465332 0.15126119   2.47686354 1078.9071   0.013
# 3         ties  0.03040675 0.01305542   2.32905163 1107.1265   0.020
# 4       r13age -0.24474996 0.01288699 -18.99201495 1427.2309   0.000
# 5        women  1.11802340 0.16266341   6.87323237 1741.7297   0.000
# 6       black1 -1.97179409 0.22558533  -8.74079044 1988.1843   0.000
# 7    hispanic1 -0.58139293 0.31077017  -1.87081318 1673.8208   0.062
# 8       other1 -0.53321946 0.48629549  -1.09649271 1786.6930   0.273
# 9       sepdiv -0.02554369 0.25606937  -0.09975301  580.5995   0.921
# 10       widow  0.19049733 0.21072112   0.90402583  791.6449   0.366
# 11   nevermarr -0.69859438 0.44663336  -1.56413389 2540.3007   0.118
# 12          hs  2.09367339 0.25716483   8.14136752 1796.1373   0.000
# 13    somecoll  3.03709471 0.26448176  11.48319153 3091.5013   0.000
# 14        coll  3.81057004 0.27688082  13.76249173 1447.5780   0.000
# 15 log_h13itot  0.44956406 0.08197774   5.48397720  961.9713   0.000
# 16    r13hibpe -0.04378877 0.17028215  -0.25715418  487.9085   0.797
# 17    r13diabe -0.51108488 0.17728266  -2.88288134  780.8613   0.004
# 18   r13cancre  0.17223071 0.19316616   0.89161950  786.3010   0.373
# 19   r13hearte  0.02653667 0.16995427   0.15614004 1245.3422   0.876
# 20   r13stroke -0.76081451 0.28274096  -2.69085349  779.0089   0.007
# 21     r13cesd -0.34448544 0.04849067  -7.10415969  566.7108   0.000


# Step 2 
# Bootstrap

# List to store bootstrapped mediation results
mediation_results <- list()

# Loop through each matched dataset and run bootstrapped mediation analysis
for (i in 1:10) {
  # Extract the matched dataset
  matched_data <- matched_data_list[[i]]
  
  # Fit the mediator model (ties)
  mediator_model <- lm(ties ~ r13g198 + r13age + women + black + hispanic + other
                       + sepdiv + widow + nevermarr + hs + somecoll + coll
                       + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                       + r13stroke + r13cesd, data = matched_data)
  
  # Fit the outcome model (r15cogtotp) including the mediator (ties)
  outcome_model <- lm(r15cogtotp ~ r13g198 + r13age + women + black + hispanic + other
                      + sepdiv + widow + nevermarr + hs + somecoll + coll
                      + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                      + r13stroke + r13cesd + ties, data = matched_data)
  
  # Perform mediation analysis with bootstrapping (no need for "data" argument here)
  mediation_result <- mediate(mediator_model, outcome_model,
                              treat = "r13g198", mediator = "ties", 
                              boot = TRUE, sims = 1000)
  
  # Store the result
  mediation_results[[i]] <- mediation_result
  
  # Print the summary for each imputed dataset's mediation result
  cat("Mediation results for imputed dataset", i, ":\n")
  print(summary(mediation_result))
}


acme_results <- sapply(mediation_results, function(res) res$d0)
acme_ci_low  <- sapply(mediation_results, function(res) res$d0.ci[1])
acme_ci_high <- sapply(mediation_results, function(res) res$d0.ci[2])
acme_p_value <- sapply(mediation_results, function(res) res$d0.p)

# Pooling ACME across datasets
pooled_acme <- mean(acme_results)
within_var_acme <- mean(sapply(mediation_results, function(res) sd(res$d0.sims)^2))
between_var_acme <- var(acme_results)
total_var_acme <- within_var_acme + (1 + 1/length(acme_results)) * between_var_acme
pooled_se_acme <- sqrt(total_var_acme)

# Z-score and p-value for pooled ACME
z_acme <- pooled_acme / pooled_se_acme
p_value_acme <- 2 * (1 - pnorm(abs(z_acme)))

# Output pooled ACME and p-value
list(pooled_acme = pooled_acme, pooled_se_acme = pooled_se_acme, p_value_acme = p_value_acme)

# $pooled_acme
# [1] 0.04238526
# 
# $pooled_se_acme
# [1] 0.01927459
# 
# $p_value_acme
# [1] 0.02787635





####################################################################################################
#  Before matching
####################################################################################################


# Part 2
# mediation for ties

# a. 
# mediation test
mediation_results <- list()

for (i in 1:num_imputations) {
  # Complete each imputed dataset
  completed_data <- complete(psm_imputed, i)
  
  # Fit the mediator model for ties
  mediator_model <- lm(formula_ties, data = completed_data)
  
  # Fit the outcome model for cognitive function
  outcome_model <- lm(formula_f1, data = completed_data)
  
  # Run the mediation analysis
  mediation_results[[i]] <- mediate(mediator_model, outcome_model, treat = "r13g198", mediator = "ties", sims = 1000)
}


# b.
# Get the acme, ade and p-value

# Initialize lists to store the ACME, ADE, and their standard errors for each imputed dataset
acme_list_1 <- c()
ade_list_1 <- c()
acme_se_list_1 <- c()
ade_se_list_1 <- c()

# Extract the ACME, ADE, and their standard errors from each mediation result
for (i in 1:num_imputations) {
  acme_list_1[i] <- summary(mediation_results[[i]])$d0    # ACME (indirect effect)
  ade_list_1[i] <- summary(mediation_results[[i]])$z0    # ADE (direct effect)
  
  acme_se_list_1[i] <- summary(mediation_results[[i]])$d0.sims # Extract standard errors
  ade_se_list_1[i] <- summary(mediation_results[[i]])$z0.sims # Extract standard errors
}

# Step 1: Calculate within-imputation variance (average of squared SEs)
within_var_acme <- mean(acme_se_list_1^2)
within_var_ade <- mean(ade_se_list_1^2)

# Step 2: Calculate between-imputation variance (variance of the estimates across imputations)
between_var_acme <- var(acme_list_1)
between_var_ade <- var(ade_list_1)

# Step 3: Calculate total variance
total_var_acme <- within_var_acme + (1 + 1/num_imputations) * between_var_acme
total_var_ade <- within_var_ade + (1 + 1/num_imputations) * between_var_ade

# Step 4: Calculate the pooled standard errors
pooled_se_acme_1 <- sqrt(total_var_acme)
pooled_se_ade_1 <- sqrt(total_var_ade)


# Pool the estimates (average)
pooled_acme_1 <- mean(acme_list_1)
pooled_ade_1 <- mean(ade_list_1)

# Calculate z-scores for ACME and ADE
z_acme_1 <- pooled_acme_1 / pooled_se_acme_1
z_ade_1 <- pooled_ade_1 / pooled_se_ade_1

# Calculate p-values for ACME and ADE (two-tailed test)
p_value_acme_1 <- 2 * (1 - pnorm(abs(z_acme_1)))  # Two-tailed test
p_value_ade_1 <- 2 * (1 - pnorm(abs(z_ade_1)))   

# Display the pooled results and p-values
pooled_acme_1  # Pooled ACME (indirect effect)
pooled_ade_1   # Pooled ADE (direct effect)
p_value_acme_1 # P-value for ACME
p_value_ade_1  # P-value for ADE


var(acme_list_1)  # Variance of ACME estimates across imputations
var(ade_list_1)   # Variance of ADE estimates across imputations

# Calculate the median ACME and ADE across imputations
median_acme <- median(acme_list_1)
median_ade <- median(ade_list_1)

# Extract the p-values for ACME and ADE across imputations
acme_p_values <- c()
ade_p_values <- c()

for (i in 1:num_imputations) {
  acme_p_values[i] <- summary(mediation_results[[i]])$d0.p  # P-value for ACME
  ade_p_values[i] <- summary(mediation_results[[i]])$z0.p   # P-value for ADE
}

# Calculate the median of the p-values for ACME and ADE
median_acme_p_value <- median(acme_p_values)
median_ade_p_value <- median(ade_p_values)

# Display the median p-values
print(paste("Median-based P-value for ACME:", median_acme_p_value))
print(paste("Median-based P-value for ADE:", median_ade_p_value))


# b. 
# see the estimates and p-value in each data set. 

# Loop over each imputed dataset and run mediation analysis
mediation_results <- list()  # Store mediation results for each imputed dataset

for (i in 1:num_imputations) {
  completed_data <- complete(psm_imputed, i)  # Get each completed imputed dataset
  
  # Fit the mediator model (ties is the mediator)
  mediator_model <- lm(formula_ties, data = completed_data)
  
  # Fit the outcome model (r15cogtotp is the dependent variable)
  outcome_model <- lm(formula_f1, data = completed_data)
  
  # Run the mediation analysis for this dataset
  mediation_results[[i]] <- mediate(mediator_model, outcome_model, treat = "r13g198", mediator = "ties", sims = 1000)
  
  # Display the mediation effect (ACME) and p-value for this dataset
  print(paste("Dataset", i))
  print(paste("ACME (Indirect Effect):", summary(mediation_results[[i]])$d0))
  print(paste("ACME P-value:", summary(mediation_results[[i]])$d0.p))
  print(paste("ADE (Direct Effect):", summary(mediation_results[[i]])$z0))
  print(paste("ADE P-value:", summary(mediation_results[[i]])$z0.p))
}


# this method NOT gen significant results 


# 2. 
# bootstrap

# Number of bootstrap samples
n_bootstrap <- 1000

# Initialize vectors to store bootstrap results for ACME and ADE
acme_boot <- numeric(n_bootstrap)
ade_boot <- numeric(n_bootstrap)

# Bootstrapping loop
for (i in 1:n_bootstrap) {
  # Resample the data with replacement
  boot_data <- completed_data[sample(1:nrow(completed_data), replace = TRUE), ]
  
  # Re-run the mediator and outcome models on the bootstrapped data
  formula_ties <- as.formula(paste("ties ~ r13g198 +", paste(covariates, collapse = " + ")))
  formula_f1 <- as.formula(paste("r15cogtotp ~ r13g198 +", paste(covariates, collapse = " + "),
                                 "+ ties"))
  mediator_model <- lm(formula_ties, data = boot_data)
  outcome_model <- lm(formula_f1, data = boot_data)
  
  # Perform mediation analysis on bootstrapped data
  boot_mediation <- mediate(mediator_model, outcome_model, treat = "r13g198", mediator = "ties", sims = 1000)
  
  # Store the ACME and ADE from the bootstrapped analysis
  acme_boot[i] <- summary(boot_mediation)$d0
  ade_boot[i] <- summary(boot_mediation)$z0
}

# Calculate bootstrapped estimates
acme_mean <- mean(acme_boot)
ade_mean <- mean(ade_boot)

# Calculate confidence intervals
acme_ci <- quantile(acme_boot, probs = c(0.025, 0.975))
ade_ci <- quantile(ade_boot, probs = c(0.025, 0.975))

# Display the bootstrapped results
acme_mean  # Bootstrapped ACME (Indirect Effect)
acme_ci    # Confidence interval for ACME
ade_mean   # Bootstrapped ADE (Direct Effect)
ade_ci     # Confidence interval for ADE


# acme_mean  # Bootstrapped ACME (Indirect Effect)
# [1] 0.004736254
# > acme_ci    # Confidence interval for ACME
# 2.5%      97.5% 
#   0.00000000 0.05318475 
# > ade_mean   # Bootstrapped ADE (Direct Effect)
# [1] 0.06631775
# > ade_ci     # Confidence interval for ADE
# 2.5%    97.5% 
#   0.000000 0.659689 

# Calculate the standard deviation of bootstrapped ACME
acme_sd <- sd(acme_boot)

# Calculate the z-score for ACME (mean / standard deviation)
z_acme <- acme_mean / acme_sd

# Calculate the two-tailed p-value from the z-score
p_value_acme_z <- 2 * (1 - pnorm(abs(z_acme)))

# Display the z-score-based p-value
p_value_acme_z  # 0.7425524



# this pparoach Not gen  p < 0.05 findings. Don;t use  as.formula() or local, shoudd spell out the model. 


######################################################################################################
# Approach 2        use this!
######################################################################################################
# 2

# #Number of bootstrap simulations
n_bootstrap <- 1000

# Extract imputed datasets
imputed_data_list <- complete(psm_imputed, action = "all")  

# Prepare a list to store mediation results for each dataset
mediation_results <- list()

# Loop through each imputed dataset to run mediation with bootstrapping
for (i in seq_along(imputed_data_list)) {
  # Get the current imputed dataset
  imputed_data <- imputed_data_list[[i]]
  
  # Fit the mediator model (ties)
  mediator_model <- lm(ties ~ r13g198 + r13age + women + black + hispanic + other
                       + sepdiv + widow + nevermarr + hs + somecoll + coll
                       + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                       + r13stroke + r13cesd, data = imputed_data)
  
  # Fit the outcome model (r15cogtotp) including the mediator (ties)
  outcome_model <- lm(r15cogtotp ~ r13g198 + r13age + women + black + hispanic + other
                      + sepdiv + widow + nevermarr + hs + somecoll + coll
                      + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                      + r13stroke + r13cesd + ties, data = imputed_data)
  
  # Run mediation analysis with bootstrapping
  mediation_result <- mediate(mediator_model, outcome_model, treat = "r13g198", 
                              mediator = "ties", boot = TRUE, sims = n_bootstrap)
  
  # Store the result
  mediation_results[[i]] <- mediation_result
}

# Extract ACME estimates and their confidence intervals for each dataset
acme_results <- sapply(mediation_results, function(res) res$d0)
acme_ci_low  <- sapply(mediation_results, function(res) res$d0.ci[1])  
acme_ci_high <- sapply(mediation_results, function(res) res$d0.ci[2])  
acme_p_value <- sapply(mediation_results, function(res) res$d0.p)      

# Create a dataframe with estimates, confidence intervals, and p-values
mediation_results_df <- data.frame(
  est = acme_results,
  ci_low = acme_ci_low,
  ci_high = acme_ci_high,
  p_value = acme_p_value
)

# Extract the ACME and its simulations from each imputed dataset
acme_results <- sapply(mediation_results, function(res) res$d0)
acme_sims    <- sapply(mediation_results, function(res) res$d0.sims)  # Bootstrapped simulations for ACME

# Compute standard error as the standard deviation of the bootstrapped simulations
acme_sims <- lapply(mediation_results, function(res) res$d0.sims)
acme_se   <- sapply(acme_sims, function(sims) sd(sims))

# Now create the dataframe for the mediation results
mediation1 <- data.frame(est = acme_results, se = acme_se)

# Pooling the estimates and variances
pooled_acme <- mean(acme_results)

# Calculate within and between variances
within_var_acme  <- mean(acme_se^2)  # Variance within imputations
between_var_acme <- var(acme_results)  # Variance between imputations

# Total variance according to Rubin's rules
num_imputations <- length(acme_results)
total_var_acme <- within_var_acme + (1 + 1/num_imputations) * between_var_acme

# Pooled standard error
pooled_se_acme <- sqrt(total_var_acme)

# Z-score for pooled ACME
z_acme <- pooled_acme / pooled_se_acme

# Two-tailed p-value for pooled ACME
p_value_acme <- 2 * (1 - pnorm(abs(z_acme)))

# Display pooled ACME and p-value
pooled_acme
p_value_acme
























