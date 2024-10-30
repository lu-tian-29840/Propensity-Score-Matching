
#@@ Author: Lu Tian
#@@ Date: Sep 26, 2024
#@@ Project name: The Role of Social Ties in the Cognitive Function of Informal Caregivers:
#@@ Findings from a Population-Based Propensity-Matched Analysis

### Part 1/4
    # Step 1: Multiple imputation
    # Step 2: propensity score matching
    # Step 3: balance diagnostics

library(MatchIt)  # for PSM
library(cobalt)   #balance diagnostics
library(Matching) # for PSM
library(rgenoud)
library(stargazer)
library(broom)
library(knitr)
library(mediation)
library(tableone)
library(mice) 
library(optmatch)
library(tableone)
library(flextable)
library(officer)
library(dplyr)
library(kableExtra)

### Step  1
### Multiple imputation
#  1 
#  choose variables to be imputed
df.psm <- dplyr::select(df.mi1, rahhidpn, r13g198,
                         women, white, black, hispanic, other, married, sepdiv, widow, nevermarr, 
                         lshs, hs, somecoll, coll, raevbrn, study,
                         r13cogtot, r13age, log_h13itot, r13hibpe, r13diabe,
                         r13cancre, r13hearte, r13stroke, r13cesd, h13child,
                         r13livsib, r13momliv, r13dadliv,
                         r14mstat, r14hibpe, r14diabe, r14cancre, r14hearte, r14stroke, 
                         r14cesd, h14child, r14livsib, r14cogtotp, r14momliv, r14dadliv,
                         r15cogtotp, r15age, r15hibpe, r15diabe, r15cancre,
                         r15hearte, r15stroke, r15cesd, h15child, r15livsib,
                         lb003_a,   lb004a_a, lb004b_a, lb004c_a, lb004d_a, lb004e_a, lb004f_a, lb004g_a, 
                         lb005_a,   lb005a_a, lb005b_a, 
                         lb006_a,   lb007a_a, lb007b_a, lb007c_a,  lb007d_a, lb007e_a, lb007f_a, lb007g_a, 
                         lb008a_a,  lb008b_a, lb008c_a, lb008d_a,
                         lb009_a,   lb010_a, lb011a_a, lb011b_a, lb011c_a,
                         lb011d_a,  lb011e_a, lb011f_a, lb011g_a, 
                         lb012a_a,  lb012b_a, lb012c_a, lb012d_a, 
                         lb013_a,   lb013a_a, lb014_a,
                         lb015a_a, lb015b_a, lb015c_a, lb015d_a, lb015e_a,lb015f_a, lb015g_a, 
                         lb016a_a, lb016b_a, lb016c_a, lb016d_a,lb017_a, 
                         ties, composition, sp_sup, child_sup, fam_sup, fri_sup) 

    # Define how missing values are handled
    binary_vars <- c("r13g198", "lb003_a", "lb006_a", "lb010_a", 
                     "r13momliv", "r13dadliv", "r14momliv", "r14dadliv", "lb014_a")
    df.psm <- df.psm %>%
    mutate(across(all_of(binary_vars), as.factor))

    method_list <- sapply(df.mi2a, function(x) {
      if(is.factor(x)) "polyreg" else "pmm"
      })

#  2
# MI 
psm_imputed <- mice(df.psm, method = method_list, m = 10, seed = 123, maxit = 20)
saveRDS(psm_imputed, file = "psm_imputed.rds")

    # checking missing after  MI 
    # Loop through each imputed dataset and check for missing values
    for (i in 1:10) {
      # Extract the ith imputed dataset
     imputed_data <- complete(psm_imputed, i)
  
      # Check for missing values in the dataset
      print(paste("Missing values in Imputed Dataset", i))
  
      # Print the sum of all missing values
      print(paste("Total missing values:", sum(is.na(imputed_data))))
    }

    # Loop through each imputed dataset and check r13g198 for missing values
    for (i in 1:10) {
      # Extract the ith imputed dataset
      imputed_data <- complete(psm_imputed, i)
  
      # Check for missing values in r13g198
      cat("Imputed Dataset", i, " - Number of NAs in r13g198:", sum(is.na(imputed_data$r13g198)), "\n")}
  

# save.image("~/LT/cross/cross1/Lu2_psm.RData")


################################################################################  




### Step  2
 #   LOOP for PSM and balance diagnostics
 #   Match(): better for matching and checking the balance diagnostics
 #   MatchIt(): better for matched data sets extract

 # Number of imputations 
num_imputations <- 10

# Create a list to store matched datasets
matched_data_list <- vector("list", num_imputations)

# Loop over each imputed dataset
for (i in 1:num_imputations) {
  
  # 1: Extract the ith imputed dataset
  imputed_data_i <- complete(psm_imputed, i)
  
  imputed_data_i$r13g198 <- imputed_data_i$r13g198 == 1 
  
  # 2: Calculate Propensity Scores using logistic regression (glm)
  ps_model <- glm(r13g198 ~ r13cogtot + r13age + women + black + hispanic + other 
                  + sepdiv + widow + nevermarr + hs + somecoll + coll
                  + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
                  + r13stroke + r13cesd,
                  family = binomial(link = "probit"), data = imputed_data_i)
  
  # 3: Extract the Propensity Scores
  propensity_scores <- ps_model$fitted.values
  
  # 4: Perform pre-matching balance check
  print(paste("Pre-matching balance diagnostics", i))
  MatchBalance(r13g198 ~ r13cogtot + r13age + women + black + hispanic + other 
               + sepdiv + widow + nevermarr + hs + somecoll + coll
               + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
               + r13stroke + r13cesd,
               data = imputed_data_i)
  
  # 5: Perform matching using the Matching package
  matching_result <- Match(Y = imputed_data_i$r15cogtotp, 
                           Tr = imputed_data_i$r13g198, 
                           X = propensity_scores, 
                           M = 1, caliper = 0.1, 
                           replace = F)
  
  # 6: Extract matched data
  matched_data_i <- imputed_data_i[unique(c(matching_result$index.treated, matching_result$index.control)), ]
  
  # 7: Store the matched dataset
  matched_data_list[[i]] <- matched_data_i
  
  # 8: Check balance diagnostics
  print(paste("Balance diagnostics", i))
  MatchBalance(r13g198 ~ r13cogtot + r13age + women + black + hispanic + other 
               + sepdiv + widow + nevermarr + hs + somecoll + coll
               + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
               + r13stroke + r13cesd,
               data = imputed_data_i, 
               match.out = matching_result)
}



