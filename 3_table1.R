#@@ Author: Lu Tian
#@@ Date: Sep 26, 2024
#@@ Project name: The Role of Social Ties in the Cognitive Function of Informal Caregivers:
#@@ Findings from a Population-Based Propensity-Matched Analysis

### Part 3/4
    # I tried 1-10 in the imputed_data_1 <- complete(psm_imputed, 1),
    # Find that all the values in all outputs are exactly the same. 
    # Just pick one data set for Table 1


# Load libraries in 1_psm.R 

################################################################################################
# PSM with MatchIt package
################################################################################################

#@@ Step 1 
### Do matching in a single data set after multiple imputation 

# Extract the first imputed dataset
imputed_data_1 <- complete(psm_imputed, 1)

#   0
#   Perform propensity score estimation without matching to check initial imbalance
m.out0 <- matchit(r13g198 ~ r13cogtot + r13age + women + black + hispanic + other
                  + sepdiv + widow + nevermarr + hs + somecoll + coll
                  + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                  + r13stroke + r13cesd,
                  data = imputed_data_1,
                  method = NULL)  # No matching, just estimating propensity scores

# Checking balance prior to matching
summary(m.out0)

# Call:
#   matchit(formula = r13g198 ~ r13cogtot + r13age + women + black + 
#             hispanic + other + sepdiv + widow + nevermarr + hs + somecoll + 
#             coll + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte + 
#             r13stroke + r13cesd, data = imputed_data_1, method = NULL)
# 
# Summary of Balance for All Data:
#                Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
# distance           0.5541        0.4491          0.7152     0.8467    0.1855   0.2796      
# r13cogtot         23.7429       22.0726          0.4119     0.7696    0.0576   0.1534
# r13age            73.2032       74.9752         -0.2830     0.8951    0.0537   0.1230
# women              0.5295        0.6782         -0.2980          .    0.1487   0.1487
# black0             0.8791        0.8363          0.1311          .    0.0428   0.0428
# black1             0.1209        0.1637         -0.1311          .    0.0428   0.0428
# hispanic0          0.9474        0.8679          0.3558          .    0.0794   0.0794
# hispanic1          0.0526        0.1321         -0.3558          .    0.0794   0.0794
# other0             0.9771        0.9769          0.0011          .    0.0002   0.0002
# other1             0.0229        0.0231         -0.0011          .    0.0002   0.0002
# sepdiv             0.1082        0.1252         -0.0548          .    0.0170   0.0170
# widow              0.1854        0.2748         -0.2300          .    0.0894   0.0894
# nevermarr          0.0276        0.0299         -0.0143          .    0.0023   0.0023
# hs                 0.2949        0.3534         -0.1284          .    0.0586   0.0586
# somecoll           0.2571        0.2256          0.0720          .    0.0315   0.0315
# coll               0.3538        0.2145          0.2913          .    0.1393   0.1393
# log_h13itot       10.8392       10.4533          0.3604     0.9202    0.1172   0.1706
# r13hibpe           0.6296        0.7192         -0.1856          .    0.0896   0.0896
# r13diabe           0.2367        0.3090         -0.1699          .    0.0722   0.0722
# r13cancre          0.1939        0.1880          0.0148          .    0.0059   0.0059
# r13hearte          0.2792        0.3038         -0.0550          .    0.0247   0.0247
# r13stroke          0.0742        0.0936         -0.0738          .    0.0193   0.0193
# r13cesd            0.9101        1.3077         -0.2608     0.6838    0.0442   0.1094
# 
# Sample Sizes:
#   Control Treated
# All          2340    2357
# Matched      2340    2357
# Unmatched       0       0
# Discarded       0       0


## we want small SMD (<0.2)
# 0.2, small effect size
# 0.5, moderate
# 0.8, large

#  SMD: 0.7152 imbalance in propensity scores between the groups before matching.
# women 0.5295  0.6782 -> About 53% of the treated group are women, compared to about 68% of the control group.
# goal of matching:reduce the SMD between the treated and control groups

#   1
#  Perform Propensity Score Matching using nearest neighbor matching
m.out1 <- matchit(r13g198 ~ r13cogtot + r13age + women + black + hispanic + other
                  + sepdiv + widow + nevermarr + hs + somecoll + coll
                  + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
                  + r13stroke + r13cesd,
                  data = imputed_data_1,
                  method = "nearest",  # Nearest neighbor matching
                  caliper = 0.1,      # Caliper to restrict large PS differences
                  link = "probit", 
                  ratio = 1)           # 1:1 matching
# AS poor performance of logit, I used link = "probit" instead
# Check the balance after matching
m.out1

# A matchit object
# - method: 1:1 nearest neighbor matching without replacement
# - distance: Propensity score [caliper]
# - estimated with probit regression
# - caliper: <distance> (0.016)
# - number of obs.: 4697 (original), 3548 (matched)
# - target estimand: ATT
# - covariates: r13cogtot, r13age, women, black, hispanic, other, sepdiv, widow, nevermarr, hs, somecoll, coll, log_h13itot, r13hibpe, r13diabe, r13cancre, r13hearte, r13stroke, r13cesd

#   2 
#   Assessing the Quality of Matches
summary(m.out1, un = FALSE)

# Call:
#   matchit(formula = r13g198 ~ r13cogtot + r13age + women + black + 
#             hispanic + other + sepdiv + widow + nevermarr + hs + somecoll + 
#             coll + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte + 
#             r13stroke + r13cesd, data = imputed_data_1, method = "nearest", 
#           link = "probit", caliper = 0.1, ratio = 1)

# Summary of Balance for Matched Data:
#   Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max Std. Pair Dist.
# distance    0.5142        0.5042          0.0682     1.0855    0.0194   0.0479          0.0685
# r13cogtot   23.1669       23.0598          0.0264     0.9191    0.0059   0.0152          0.9962
# r13age      73.9746       74.1764         -0.0322     1.0666    0.0092   0.0327          1.0196
# women       0.5975        0.6234         -0.0520          .    0.0259   0.0259          0.7612
# black0      0.8591        0.8540          0.0156          .    0.0051   0.0051          0.7106
# black1      0.1409        0.1460         -0.0156          .    0.0051   0.0051          0.7106
# hispanic0   0.9324        0.9295          0.0126          .    0.0028   0.0028          0.4671
# hispanic1   0.0676        0.0705         -0.0126          .    0.0028   0.0028          0.4671
# other0      0.9752        0.9763         -0.0075          .    0.0011   0.0011          0.3089
# other1      0.0248        0.0237          0.0075          .    0.0011   0.0011          0.3089
# sepdiv      0.1201        0.1234         -0.0109          .    0.0034   0.0034          0.6896
# widow       0.2204        0.2289         -0.0218          .    0.0085   0.0085          0.8050
# nevermarr   0.0287        0.0299         -0.0069          .    0.0011   0.0011          0.3511
# hs          0.3399        0.3540         -0.0309          .    0.0141   0.0141          0.8443
# somecoll    0.2627        0.2582          0.0103          .    0.0045   0.0045          0.8693
# coll       0.2796        0.2717          0.0165          .    0.0079   0.0079          0.6673
# log_h13itot 10.6897       10.6396          0.0468     1.1380    0.0162   0.0395          0.8216
# r13hibpe   0.6674        0.6770         -0.0198          .    0.0096   0.0096          0.8416
# r13diabe    0.2632        0.2683         -0.0119          .    0.0051   0.0051          0.8765
# r13cancre   0.2001        0.1900          0.0257          .    0.0101   0.0101          0.7985
# r13hearte   0.2959        0.2943          0.0038          .    0.0017   0.0017          0.9437
# r13stroke   0.0800        0.0817         -0.0065          .    0.0017   0.0017          0.5526
# r13cesd     1.0197        1.0485         -0.0189     0.9774    0.0038   0.0186          0.9063
# 
# Sample Sizes:
#   Control Treated
# All          2340    2357
# Matched      1774    1774
# Unmatched     566     583
# Discarded       0       0

#   3 
#   Visualization

#   1) visualize the distribution of propensity scores of matched samples
plot(m.out1, type = "jitter", interactive = FALSE)

#  2) love plot for SMD -> assess the balance of covariates before and after matching 
plot(summary(m.out1))

#   3) visualize the balance on the covariates
plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~ r13cogtot + r13age + women + black + hispanic + other
     + sepdiv + widow + nevermarr + hs + somecoll + coll
     + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte
     + r13stroke + r13cesd)

#   4
#   Extract matched data from MatchIt object

matched_data1 <- match.data(m.out1)

###################################################################################
### Step 2
#   Table 1

#   1
#   Unmatched sample
vars <- c("r13cogtot", "r13age", "women", "white", "black", "hispanic", "other", 
          "married","sepdiv", "widow", "nevermarr", "lshs","hs", "somecoll", "coll", 
          "log_h13itot", "r13hibpe", "r13diabe", "r13cancre", "r13hearte", 
          "r13stroke", "r13cesd")

# Specify categorical variables (for chi-square tests)
factorVars <- c("women", "white", "black", "hispanic", "other", "married", 
                "sepdiv", "widow", "nevermarr", "lshs", "hs", "somecoll", "coll", "r13hibpe", 
                "r13diabe", "r13cancre", "r13hearte", "r13stroke")


# Create the descriptive table for the unmatched data
table1_unmatched <- CreateTableOne(vars = vars, strata = "r13g198", data = imputed_data_1, 
                                   factorVars = factorVars, test = TRUE)

# Print the unmatched table with p-values (chi-square for categorical, t-test for continuous)
print(table1_unmatched, smd = FALSE)

# Convert the table output to a data frame for exporting
df.table1_unmatched <- as.data.frame(print(table1_unmatched, smd = FALSE))

# Export the table as a CSV file
write.csv(df.table1_unmatched, file = "table1_unmatched.csv", row.names = TRUE)

# Convert the table output to a data frame for exporting
df.table1_unmatched <- as.data.frame(print(table1_unmatched, smd = FALSE))

# Create a flextable object from the data frame
flextable_obj <- flextable(df.table1_unmatched)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, flextable_obj)

# Save the Word document
print(doc, target = "table1_unmatched.docx")

# Stratified by r13g198
#                             0             1             p      test
# n                        2340          2357                    
# r13cogtot (mean (SD))   22.07 (4.62)  23.74 (4.06)  <0.001     
# r13age (mean (SD))      74.98 (6.62)  73.20 (6.26)  <0.001     
# women = 1 (%)            1587 (67.8)   1248 (52.9)  <0.001     
# white = 1 (%)            1594 (68.1)   1894 (80.4)  <0.001     
# black = 1 (%)             383 (16.4)    285 (12.1)  <0.001     
# hispanic = 1 (%)          309 (13.2)    124 ( 5.3)  <0.001     
# other = 1 (%)              54 ( 2.3)     54 ( 2.3)   1.000     
# married = 1 (%)          1334 (57.0)   1600 (67.9)  <0.001     
# sepdiv = 1 (%)            293 (12.5)    255 (10.8)   0.076     
# widow = 1 (%)             643 (27.5)    437 (18.5)  <0.001     
# nevermarr = 1 (%)          70 ( 3.0)     65 ( 2.8)   0.695     
# lshs = 1 (%)              479 (20.5)    215 ( 9.1)  <0.001     
# hs = 1 (%)                827 (35.3)    695 (29.5)  <0.001     
# somecoll = 1 (%)          528 (22.6)    606 (25.7)   0.013     
# coll = 1 (%)              502 (21.5)    834 (35.4)  <0.001     
# log_h13itot (mean (SD)) 10.45 (1.12)  10.84 (1.07)  <0.001     
# r13hibpe = 1 (%)         1683 (71.9)   1484 (63.0)  <0.001     
# r13diabe = 1 (%)          723 (30.9)    558 (23.7)  <0.001     
# r13cancre = 1 (%)         440 (18.8)    457 (19.4)   0.636     
# r13hearte = 1 (%)         711 (30.4)    658 (27.9)   0.067     
# r13stroke = 1 (%)         219 ( 9.4)    175 ( 7.4)   0.019     
# r13cesd (mean (SD))      1.31 (1.84)   0.91 (1.52)  <0.001 


#   2
#  Create the descriptive table for the matched data
table1_matched <- CreateTableOne(vars = vars, strata = "r13g198", data = matched_data1, 
                                 factorVars = factorVars, test = TRUE)

# Print the matched table with p-values (chi-square for categorical, t-test for continuous)
print(table1_matched, smd = FALSE)

df.table1_matched <- as.data.frame(print(table1_matched, smd = FALSE))
write.csv(df.table1_matched, file = "table1_matched.csv", row.names = TRUE)

# Stratified by r13g198
#                             0             1             p      test
# n                        1774          1774                    
# r13cogtot (mean (SD))   23.06 (4.24)  23.17 (4.06)   0.442     
# r13age (mean (SD))      74.18 (6.29)  73.97 (6.49)   0.347     
# women = 1 (%)            1106 (62.3)   1060 (59.8)   0.121     
# white = 1 (%)            1348 (76.0)   1360 (76.7)   0.664     
# black = 1 (%)             259 (14.6)    250 (14.1)   0.702     
# hispanic = 1 (%)          125 ( 7.0)    120 ( 6.8)   0.791     
# other = 1 (%)              42 ( 2.4)     44 ( 2.5)   0.913     
# married = 1 (%)          1096 (61.8)   1119 (63.1)   0.446     
# sepdiv = 1 (%)            219 (12.3)    213 (12.0)   0.797     
# widow = 1 (%)             406 (22.9)    391 (22.0)   0.573     
# nevermarr = 1 (%)          53 ( 3.0)     51 ( 2.9)   0.921     
# lshs = 1 (%)              203 (11.4)    205 (11.6)   0.958     
# hs = 1 (%)                628 (35.4)    603 (34.0)   0.397     
# somecoll = 1 (%)          458 (25.8)    466 (26.3)   0.789     
# coll = 1 (%)              482 (27.2)    496 (28.0)   0.625     
# log_h13itot (mean (SD)) 10.64 (1.03)  10.69 (1.10)   0.162     
# r13hibpe = 1 (%)         1201 (67.7)   1184 (66.7)   0.567     
# r13diabe = 1 (%)          476 (26.8)    467 (26.3)   0.761     
# r13cancre = 1 (%)         337 (19.0)    355 (20.0)   0.471     
# r13hearte = 1 (%)         522 (29.4)    525 (29.6)   0.941     
# r13stroke = 1 (%)         145 ( 8.2)    142 ( 8.0)   0.902     
# r13cesd (mean (SD))      1.05 (1.64)   1.02 (1.62)   0.599     


###################################################################################
### Step 3
#@@ OLS test after macthing 
ols_model <- lm(r15cogtotp ~ r13g198 + r13age + women + black + hispanic + other 
                + sepdiv + widow + nevermarr + hs + somecoll + coll
                + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
                + r13stroke + r13cesd,
                data = matched_data)
summary(ols_model)
# r13g1981     0.40714    0.14425   2.823 0.004791 ** 
ols_model_mediator <- lm(ties ~ r13g198 + r13age + women + black + hispanic + other 
                         + sepdiv + widow + nevermarr + hs + somecoll + coll
                         + log_h13itot + r13hibpe + r13diabe + r13cancre + r13hearte 
                         + r13stroke + r13cesd, data = matched_data)
summary(ols_model_mediator)
# r13g1981     1.393943   0.193496   7.204 7.11e-13 ***
