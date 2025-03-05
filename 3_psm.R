# Peoject: caregiving and cognition--psm approach
# code for Jounbal of aging and health
# caregiving variables are th eones difference than preious version
# Feb 21, 2025
# Author: Lu Tian

###  Sections 3/4
#    Section 3
#             Part 1: Propensity Score Matching within each imputed dataset
#             Part 2: Love plot and balance assessments 
#             Part 3: Bootstrap
#             Part 4: Table 2 in publication


### Part  1 Propensity Score Matching within each imputed dataset

library(mice)
library(MatchIt)
library(survey)
library(mitools)
library(MatchThem)  # for love plot in the setting of multiple imputation
library(cobalt)
library(mediation) # for bootstrap 
library(dplyr)
library(tidyr)
library(purrr)
library(knitr)
library(forcats) # for fct_relevel

imputed20 <- readRDS("imputed20.rds")

####################################################################################################
#                                            Part 1 PSM
####################################################################################################

### Step  1 
#   Propensity Score Matching within each imputed dataset

matched_data_list <- list()  # To store matched datasets
ols1_list <- list() 
ties_list <- list() 
ols1_f_list <- list()  # Full model
match_info <- list()  # To store matching information for each imputed dataset

for (i in 1:20) {
  imputed_data <- complete(imputed20, i) %>%
    mutate(
      edu = case_when(
        schlyrs <= 11 ~ "Less than High School",
        schlyrs == 12 ~ "High School Graduate",
        schlyrs >= 13 & schlyrs <= 15 ~ "Some College",
        schlyrs >= 16 ~ "College Degree",
      ),
      race = case_when(
        white == 1 ~ "White",
        black == 1 ~ "Black",
        hispanic == 1 ~ "Hispanic",
        other == 1 ~ "Other"
      ),
      marr = case_when(
        married == 1 ~ "Married",
        sepdiv == 1 ~ "Separated/Divorced",
        widow == 1 ~ "Widowed",
        nevermarr == 1 ~ "Never Married"
      )
    ) %>%
    mutate(
      edu = factor(edu, levels = c("Less than High School", "High School Graduate", 
                                   "Some College", "College Degree")),
      race = as.factor(race),
      marr = as.factor(marr)
    ) %>% 
    mutate(
      race = fct_relevel(factor(race), "White"),
      marr = fct_relevel(factor(marr), "Married"),
      edu = fct_relevel(factor(edu), "Less than High School")
    )
  
  
  # Estimate the propensity score using logistic regression
  ps_model <- glm(care ~ r13cogtot + r13age + 
                    women + race + marr + edu +
                    log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte +
                    r13stroke + r13cesd,
                  family = binomial(),
                  data = imputed_data)
  
  # Add the propensity scores to the dataset
  imputed_data$propensity_score <- predict(ps_model, type = "response")
  
  # Perform propensity score matching
  m.out <- matchit(care ~ r13cogtot + r13age + 
                     women + race + marr + edu +
                     log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte +
                     r13stroke + r13cesd,
                   data = imputed_data, 
                   method = "nearest",
                   distance = "mahalanobis",  
                   exact = ~ r13age + marr,
                   ratio = 1)
  
  # Extract matched data and store it
  mout1 <- match.data(m.out)
  mout1$.imp <- i  # Corrected from matched_data$.imp <- i
  matched_data_list[[i]] <- mout1  # Save the matched dataset
  
  # Count the number of treated and control units in the matched data
  num_treated <- sum(mout1$care == 1)
  num_control <- sum(mout1$care == 0)
  
  # Store matching information (treated and control counts)
  match_info[[i]] <- list(treated = num_treated, control = num_control)
  
  # OLS model for the outcome (r15cogtotp)
  ols1_psm <- lm(r15cogtotp ~ care + r13cogtot + r13age 
                 + women + race + marr + edu
                 + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
                 + r13stroke + r13cesd, 
                 data = mout1)
  
  # OLS model for the mediator (ties)
  ties_psm <- lm(ties ~ care + r13cogtot + r13age 
                 + women + race + marr + edu
                 + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
                 + r13stroke + r13cesd, 
                 data = mout1)
  
  # OLS full model
  ols1_f <- lm(r15cogtotp ~ care + ties + r13cogtot + r13age  
               + women + race + marr + edu
               + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
               + r13stroke + r13cesd, 
               data = mout1)
  
  # Store the OLS models in the respective lists
  ols1_list[[i]] <- ols1_psm
  ties_list[[i]] <-  ties_psm
  ols1_f_list[[i]] <- ols1_f 
}

# Print the number of treated and control units in each imputed dataset after matching
for (i in 1:20) {
  cat("Imputed Dataset", i, "\n")
  cat("Treated Units:", match_info[[i]]$treated, "\n")
  cat("Control Units:", match_info[[i]]$control, "\n\n")
}

# Imputed Dataset 1 - 20
ols1_mira <- as.mira(ols1_list)  # Model 1: r15cogtotp ~ care + covariates
ties_mira <- as.mira(ties_list)  # Model 2: ties ~ care + covariates
ols1_f_mira <- as.mira(ols1_f_list)  # Model 3: Full model r15cogtotp ~ care + ties + covariates

# Pool results for each model
ols1_pooled <- pool(ols1_mira)
ties_pooled <- pool(ties_mira)
ols1_f_pooled <- pool(ols1_f_mira)


summary_ols1_pooled <- summary(ols1_pooled)
summary_ties_pooled <- summary(ties_pooled)
summary_ols1_f_pooled <- summary(ols1_f_pooled)

# Apply rounding
summary_ols1_pooled[, -1] <- round(summary_ols1_pooled[, -1], 3)
summary_ties_pooled[, -1] <- round(summary_ties_pooled[, -1], 3)
summary_ols1_f_pooled[, -1] <- round(summary_ols1_f_pooled[, -1], 3)

summary_ols1_pooled
# term estimate std.error statistic       df p.value
# 1              (Intercept)   15.729     1.459    10.782 1296.090   0.000
# 2                    care1    0.288     0.143     2.015  535.615   0.044
# 3                r13cogtot    0.463     0.020    23.455  648.676   0.000
# 4                   r13age   -0.116     0.012    -9.255  582.041   0.000
# 5                    women    0.478     0.148     3.229 1363.293   0.001
# 6                raceBlack   -0.244     0.213    -1.147 1670.076   0.252
# 7             raceHispanic    0.442     0.263     1.678 2000.065   0.093
# 8                raceOther    0.300     0.473     0.633 2137.410   0.526
# 9        marrNever Married   -0.501     0.503    -0.997  339.642   0.320
# 10  marrSeparated/Divorced   -0.526     0.249    -2.108  650.973   0.035
# 11             marrWidowed    0.125     0.213     0.588  628.232   0.557
# 12 eduHigh School Graduate    1.267     0.231     5.475 2277.666   0.000
# 13         eduSome College    1.824     0.242     7.542 3258.933   0.000
# 14       eduCollege Degree    1.566     0.262     5.978 1752.030   0.000
# 15             log_h13itot    0.235     0.085     2.750  755.971   0.006
# 16                r13hibpe    0.067     0.151     0.443 1122.502   0.658
# 17                r13diabe   -0.222     0.162    -1.370  891.352   0.171
# 18               r13cancre    0.094     0.181     0.521 1283.091   0.603
# 19               r13hearte   -0.119     0.157    -0.760 1290.406   0.447
# 20               r13stroke   -0.573     0.278    -2.058 1208.797   0.040
# 21                 r13cesd   -0.218     0.045    -4.849 1446.317   0.000

summary_ols1_f_pooled
# term estimate std.error statistic       df p.value
# 1              (Intercept)   15.359     1.422    10.803 1204.137   0.000
# 2                    care1    0.267     0.143     1.871  578.219   0.062
# 3                     ties    0.024     0.012     2.108 1039.540   0.035
# 4                r13cogtot    0.461     0.020    23.346  621.992   0.000
# 5                   r13age   -0.118     0.013    -9.410  591.500   0.000
# 6                    women    0.475     0.148     3.208 1393.626   0.001
# 7                raceBlack   -0.293     0.214    -1.368 1687.487   0.172
# 8             raceHispanic    0.421     0.263     1.600 2022.683   0.110
# 9                raceOther    0.289     0.473     0.611 2145.769   0.541
# 10       marrNever Married   -0.465     0.502    -0.926  342.173   0.355
# 11  marrSeparated/Divorced   -0.509     0.250    -2.041  638.855   0.042
# 12             marrWidowed    0.111     0.213     0.522  628.561   0.602
# 13 eduHigh School Graduate    1.286     0.231     5.559 2334.759   0.000
# 14         eduSome College    1.847     0.242     7.637 3286.909   0.000
# 15       eduCollege Degree    1.594     0.262     6.089 1811.537   0.000
# 16             log_h13itot    0.238     0.085     2.779  732.439   0.006
# 17                r13hibpe    0.072     0.151     0.473 1128.166   0.636
# 18                r13diabe   -0.215     0.162    -1.328  903.286   0.185
# 19               r13cancre    0.092     0.181     0.509 1278.405   0.611
# 20               r13hearte   -0.121     0.157    -0.768 1284.148   0.443
# 21               r13stroke   -0.574     0.278    -2.061 1204.686   0.040
# 22                 r13cesd   -0.210     0.045    -4.660 1449.913   0.000


summary_ties_pooled 
# term estimate std.error statistic       df p.value
# 1              (Intercept)    3.146     2.108     1.492 3464.838   0.136
# 2                    care1    0.874     0.200     4.373 2782.019   0.000
# 3                r13cogtot    0.046     0.028     1.686 3507.765   0.092
# 4                   r13age    0.090     0.017     5.185 3548.676   0.000
# 5                    women    0.152     0.215     0.707 3429.880   0.480
# 6                raceBlack    1.990     0.310     6.423 3628.169   0.000
# 7             raceHispanic    0.854     0.387     2.205 3419.019   0.028
# 8                raceOther    0.450     0.692     0.650 3701.450   0.516
# 9        marrNever Married   -1.454     0.669    -2.173 3671.092   0.030
# 10  marrSeparated/Divorced   -0.667     0.350    -1.908 3328.674   0.056
# 11             marrWidowed    0.575     0.296     1.944 3583.695   0.052
# 12 eduHigh School Graduate   -0.751     0.344    -2.180 3062.001   0.029
# 13         eduSome College   -0.933     0.364    -2.561 3235.898   0.010
# 14       eduCollege Degree   -1.157     0.383    -3.020 3445.379   0.003
# 15             log_h13itot   -0.112     0.121    -0.930 3340.636   0.353
# 16                r13hibpe   -0.188     0.216    -0.872 3643.757   0.383
# 17                r13diabe   -0.286     0.230    -1.248 3649.806   0.212
# 18               r13cancre    0.092     0.260     0.355 3627.579   0.723
# 19               r13hearte    0.052     0.227     0.228 3344.920   0.820
# 20               r13stroke    0.028     0.398     0.070 3686.852   0.944
# 21                 r13cesd   -0.325     0.065    -5.007 3618.225   0.000

####################################################################################################
#                      Part 2: love plot and balance assessments 
####################################################################################################


# Step 1: Propensity Score Matching within each imputed dataset

                      
 # Convert imputed data to long format while keeping the original dataset

 # Convert imputed data to long format while keeping the original dataset

 # Convert imputed data to long format 
imputed_long <- complete(imputed20, action = "long", include = TRUE) %>%
  group_by(.imp) %>%
  mutate(
    edu = case_when(
      schlyrs <= 11 ~ "Less than High School",
      schlyrs == 12 ~ "High School Graduate",
      schlyrs >= 13 & schlyrs <= 15 ~ "Some College",
      schlyrs >= 16 ~ "College Degree", 
    ),
    race = case_when(
      white == 1 ~ "White",
      black == 1 ~ "Black",
      hispanic == 1 ~ "Hispanic",
      other == 1 ~ "Other" 
    ),
    marr = case_when(
      married == 1 ~ "Married",
      sepdiv == 1 ~ "Separated/Divorced",
      widow == 1 ~ "Widowed",
      nevermarr == 1 ~ "Never Married" 
    ),
    # Convert to factors
    edu = factor(edu, levels = c("Less than High School", "High School Graduate", 
                                 "Some College", "College Degree")),
    race = factor(race, levels = c("White", "Black", "Hispanic", "Other")),
    marr = factor(marr, levels = c("Married", "Separated/Divorced", "Widowed", "Never Married"))
  )
                     
     imputed20 <- as.mids(imputed_long)
                      
     matched_data <- matchthem(care ~ r13cogtot + r13age + women + race + marr + edu +
                                 log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte +
                                 r13stroke + r13cesd,
                               datasets = imputed20,
                               method = "nearest",
                               distance = "mahalanobis",
                               exact = ~ r13age + marr,
                               ratio = 1)
                   
                      
                      
# Assess covariate balance
balance_assessment <- bal.tab(matched_data, un = TRUE, thresholds = list(m = 0.2))

 print(balance_assessment, digits = 3)
 
 # Balance summary across all imputations
 # Type Min.Diff.Un Mean.Diff.Un Max.Diff.Un Min.Diff.Adj Mean.Diff.Adj Max.Diff.Adj
 # r13cogtot                 Contin.       0.070        0.076       0.083       -0.041        -0.030       -0.022
 # r13age                    Contin.      -0.178       -0.172      -0.165        0.000         0.000        0.000
 # women                      Binary       0.032        0.035       0.040        0.066         0.071        0.074
 # race_White                 Binary      -0.047       -0.043      -0.040       -0.080        -0.077       -0.075
 # race_Black                 Binary       0.019        0.021       0.024        0.033         0.035        0.037
 # race_Hispanic              Binary       0.016        0.019       0.021        0.031         0.033        0.035
 # race_Other                 Binary       0.003        0.003       0.004        0.010         0.010        0.010
 # marr_Married               Binary       0.149        0.154       0.156        0.000         0.000        0.000
 # marr_Separated/Divorced    Binary      -0.040       -0.038      -0.036        0.000         0.000        0.000
 # marr_Widowed               Binary      -0.113       -0.109      -0.106        0.000         0.000        0.000
 # marr_Never Married         Binary      -0.008       -0.007      -0.005        0.000         0.000        0.000
 # edu_Less than High School  Binary      -0.012       -0.009      -0.006        0.015         0.017        0.019
 # edu_High School Graduate   Binary      -0.013       -0.009      -0.005       -0.019        -0.015       -0.011
 # edu_Some College           Binary       0.002        0.006       0.008       -0.003         0.002        0.005
 # edu_College Degree         Binary       0.008        0.012       0.014       -0.008        -0.005       -0.001
 # log_h13itot               Contin.       0.079        0.088       0.099       -0.113        -0.104       -0.100
 # r13hibpe                   Binary      -0.023       -0.019      -0.017       -0.007        -0.003       -0.001
 # r13diabe                   Binary      -0.014       -0.012      -0.009       -0.003        -0.000        0.002
 # r13cancre                  Binary      -0.007       -0.004      -0.002        0.022         0.025        0.029
 # r13hearte                  Binary      -0.009       -0.006      -0.003        0.010         0.014        0.019
 # r13stroke                  Binary      -0.004       -0.003      -0.001        0.027         0.029        0.030
 # r13cesd                   Contin.      -0.040       -0.033      -0.026        0.105         0.112        0.118
 # 
 # Average sample sizes across imputations
 # 0      1
 # All       2856.4 1893.6
 # Matched   1864.7 1864.7
 # Unmatched  991.7   28.9

balance_table <- balance_assessment$Balance
balance_table <- as.data.frame(lapply(balance_table, function(x) {
  if (is.numeric(x)) round(x, 3) else x
}))

kable(balance_assessment$Balance, caption = "Covariate Balance Summary Across All Imputations")

write.csv(balance_table, "balance20.csv", row.names = TRUE)

numUnbalanced <- sum(abs(balance_table$Mean.Diff.Adj) > 0.2, na.rm = TRUE)
cat("Number of covariates with SMD > 0.2:", numUnbalanced, "\n")

# Step 2
# Create a Love plot to visualize covariate balance
love.plot(balance_assessment, stat = "mean.diffs", threshold = 0.2)

# Step 3
# descrptive table for pooled matching results
# Pool descriptive statistics across imputed datasets
pooled_summary <- summary(pool(with(matched_data, 
                                    lm(r15cogtotp ~ care + r13cogtot + r13age + women + race
                                         + marr + edu +
                                         log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte +
                                         r13stroke + r13cesd))))

                  pooled_summary[, -1] <- round(pooled_summary[, -1], 3)
                  
                  # term estimate std.error statistic       df p.value
                  # 1               (Intercept)   17.802     1.687    10.553  922.613   0.000
                  # 2                     care1    0.207     0.159     1.298  471.485   0.195
                  # 3                 r13cogtot    0.464     0.023    19.842  270.729   0.000
                  # 4                    r13age   -0.109     0.014    -7.737  927.680   0.000
                  # 5                     women    0.450     0.169     2.658 1142.301   0.008
                  # 6              raceHispanic    0.732     0.344     2.129 1752.028   0.033
                  # 7                 raceOther    0.144     0.592     0.243 1628.680   0.808
                  # 8                 raceWhite    0.236     0.248     0.954 1741.345   0.340
                  # 9         marrNever Married   -1.252     0.620    -2.018 1152.745   0.044
                  # 10   marrSeparated/Divorced   -0.533     0.285    -1.868 1091.813   0.062
                  # 11              marrWidowed    0.115     0.229     0.500 1722.175   0.617
                  # 12           eduHigh School   -0.318     0.202    -1.571 1613.709   0.116
                  # 13 eduLess than High School   -1.405     0.295    -4.766 2055.804   0.000
                  # 14          eduSome College    0.122     0.211     0.580 1316.424   0.562
                  # 15               eduUnknown    1.751     1.877     0.932  900.566   0.351
                  # 16              log_h13itot    0.138     0.092     1.498 1414.627   0.134
                  # 17                 r13hibpe    0.089     0.173     0.512  568.721   0.609
                  # 18                 r13diabe   -0.218     0.177    -1.230 1427.551   0.219
                  # 19                r13cancre    0.124     0.202     0.611  907.271   0.542
                  # 20                r13hearte   -0.299     0.177    -1.690 1412.024   0.091
                  # 21                r13stroke   -0.502     0.324    -1.548 1078.022   0.122
                  # 22                  r13cesd   -0.188     0.061    -3.087 1670.088   0.002
                  
                  

####################################################################################################
#                                     Part 3  Bootstrap analysis 
####################################################################################################
                  
 n_bootstrap <- 1000 
                  
 medList_psm <- list()
  for (i in seq_along(matched_data_list)) {
     matched_data <- matched_data_list[[i]]  
        mediator_model <- lm(ties ~ care + r13cogtot + r13age + women + race
                                         + marr + edu
                                         + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                                         + r13stroke + r13cesd, data = matched_data)
                    
# Fit the outcome model (r15cogtotp) + mediator (ties)
         outcome_model <- lm(r15cogtotp ~ care + ties + r13cogtot + r13age + women + race
                                        + marr + edu
                                        + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                                        + r13stroke + r13cesd, data = matched_data)
                    
# Run mediation analysis with bootstrapping
                    
          mediation_result <- mediate(mediator_model, outcome_model, treat = "care", 
                                                mediator = "ties", boot = TRUE, sims = n_bootstrap, 
                                                boot.ci.type = "bca")  
                    medList_psm[[i]] <- mediation_result
                  }
                  
                  # Extract ACME estimates and confidence intervals for each matched dataset
                  acme_results_psm <- sapply(medList_psm, function(res) res$d0)
                  acme_ci_low_psm  <- sapply(medList_psm, function(res) res$d0.ci[1])  
                  acme_ci_high_psm <- sapply(medList_psm, function(res) res$d0.ci[2])  
                  acme_p_value_psm <- sapply(medList_psm, function(res) res$d0.p)      
                  
                  # Create a dataframe with estimates, confidence intervals, and p-values
                  medList_df_psm <- data.frame(
                    est = acme_results_psm,
                    ci_low = acme_ci_low_psm,
                    ci_high = acme_ci_high_psm,
                    p_value = acme_p_value_psm
                  )
                  
                  # Extract the ACME bootstrapped simulations for each matched dataset
                  acme_sims_psm <- lapply(medList_psm, function(res) res$d0.sims)
                  
                  acme_se_psm   <- sapply(acme_sims_psm, function(sims) sd(sims))  
                  
                  # Create dataframe for mediation results in matched data
                  mediation_psm <- data.frame(est = acme_results_psm, se = acme_se_psm)
                  
                  # Pooling estimates using Rubin's rules
                  pooled_acme_psm <- mean(acme_results_psm)
                  
                  # Calculate within and between variances
                  within_var_psm  <- mean(acme_se_psm^2)  
                  between_var_psm <- var(acme_results_psm)  
                  
                  # Total variance (Rubin's rules)
                  num_imputations <- length(acme_results_psm)
                  total_var_psm <- within_var_psm + (1 + 1/num_imputations) * between_var_psm
                  
                  # Pooled standard error
                  pooled_se_psm <- sqrt(total_var_psm)
                  
                  # Z-score for pooled ACME
                  z_psm <- pooled_acme_psm / pooled_se_psm
                  
                  # single-tailed p-value for pooled ACME
                  p_value_psm <- p_value_psm <- 1 - pnorm(abs(z_psm)) 
                  
                  # Display pooled ACME and p-value for matched datasets
                  pooled_acme_psm
                  p_value_psm
                  
                  # Create a data frame for bootstrapped mediation results
                  bootstrap_results_table <- data.frame(
                    Effect = "ACME (Indirect Effect)",  
                    Estimate = round(pooled_acme_psm, 3),  # Pooled ACME estimate
                    SE = round(pooled_se_psm, 3), 
                    CI_Low = round(quantile(acme_results_psm, probs = 0.025), 3),  
                    CI_High = round(quantile(acme_results_psm, probs = 0.975), 3),  
                    p_value = round(p_value_psm, 3)  # Pooled p-value
                  )
                  
                  # Print the table
                  print(bootstrap_results_table)
                  
                  # Effect Estimate    SE CI_Low CI_High p_value
                  # 2.5% ACME (Indirect Effect)    0.021 0.012  0.015   0.028   0.066


###################################################################################################
#  Part 4 Table 2 in manuscrio
###################################################################################################

# Variable names for the 'Term' column
variable_names <- c(
  "(Intercept)", "Caregiving", "Cognition Baseline","Age", "Women", "Black", 
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
  Estimate = as.numeric(summary_ols1_pooled$estimate),
  Std_Error = as.numeric(summary_ols1_pooled$std.error),
  p_value = as.numeric(summary_ols1_pooled$p.value)
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
model1 <- model1 %>%
  mutate(Value = ifelse(grepl("\\(", Value), paste0('"', Value, '"'), Value))

write.csv(model1, file = "model1_output.csv", row.names = FALSE)


# Variable names for model2 (assuming they are the same or different based on your context)
variable_names_model2 <- c(
  "(Intercept)", "Caregiving", "Cognition Baseline", "Age", "Women", "Black", 
  "Hispanic", "Other", "Separated/Divorced", "Widowed", "Never Married",
  "High School", "Some College", "College", "Income (log)",
  "High BP", "Diabetes", "Cancer", "Heart Disease", "Stroke", "CESD"
)

# Create the 'model2' data frame with estimates, standard errors, and p-values from summary_ties_pooled
model2 <- data.frame(
  Term = variable_names_model2,  
  Estimate = as.numeric(summary_ties_pooled$estimate),  # Ensure numeric values
  Std_Error = as.numeric(summary_ties_pooled$std.error),  # Ensure numeric values
  p_value = as.numeric(summary_ties_pooled$p.value)  # Ensure numeric values
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
model2 <- model2 %>%
  mutate(Value = ifelse(grepl("\\(", Value), paste0('"', Value, '"'), Value))
write.csv(model2, file = "model2_output.csv", row.names = FALSE)






# Variable names for model3 (assuming they are the same or based on your context)
variable_names_model3 <- c(
  "(Intercept)", "Caregiving", "Social Ties", "Cognition Baseline", "Age", "Women", "Black", 
  "Hispanic", "Other", "Separated/Divorced", "Widowed", "Never Married",
  "High School", "Some College", "College", "Income (log)",
  "High BP", "Diabetes", "Cancer", "Heart Disease", "Stroke", "CESD"
)

# Create the 'model3' data frame with estimates, standard errors, and p-values from summary_ols1_f_pooled
model3 <- data.frame(
  Term = variable_names_model3,  
  Estimate = as.numeric(summary_ols1_f_pooled$estimate),  # Ensure numeric values
  Std_Error = as.numeric(summary_ols1_f_pooled$std.error),  # Ensure numeric values
  p_value = as.numeric(summary_ols1_f_pooled$p.value)  # Ensure numeric values
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
model3 <- model3 %>%
  mutate(Value = ifelse(grepl("\\(", Value), paste0('"', Value, '"'), Value))
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

                  