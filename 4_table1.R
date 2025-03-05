
### Should run lines 1-335 in section 3 first!!!
###  Sections 4/4
#    Section 4
#             Part 1: Descriptive Table 1 BEFORE PSM
#             Part 2: Descriptive Table 1 AFTER PSM 


############################################################################################
#
#          Part 1: Descriptive Table 1 BEFORE PSM
#
############################################################################################

# --- Define variables ---
# Continuous variables: we'll report mean (SD)
cont_vars <- c("r13cogtot", "r13age", "log_h13itot", "r13cesd")
# Binary variables (coded as 0/1): we'll report count (%) for ones
bin_vars  <- c("women", "r13hibpe", "r13diabe", "r13cancre", "r13hearte", "r13stroke")
# Categorical variables (already factors):  race, marr, edu.
cat_vars  <- c("race", "marr", "edu")

format_cont <- function(mean, sd) {
  sprintf("%.2f (%.2f)", mean, sd)
}


####################################################################################################
## 0.Function for FISHER’S METHOD
####################################################################################################
combine_pvals_fisher <- function(pvals) {
  chi_stat <- -2 * sum(log(pvals))
  df <- 2 * length(pvals)
  pchisq(chi_stat, df = df, lower.tail = FALSE)
}


############################################################################################
## 1. FUNCTION: Compute Descriptive Summaries
############################################################################################
compute_descriptives <- function(data) {
  # Continuous summaries by 'care'
  cont_summary <- data %>% 
    group_by(care) %>% 
    summarise(
      n = n(),
      across(all_of(cont_vars),
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd   = ~sd(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
  
  # Binary summaries: count of ones and percentage
  bin_summary <- data %>% 
    group_by(care) %>% 
    summarise(
      across(all_of(bin_vars),
             list(count = ~sum(.x == 1, na.rm = TRUE),
                  pct   = ~100 * mean(.x == 1, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
  
  # Categorical summaries: for each variable, count and percentage per level
  cat_summary_list <- lapply(cat_vars, function(var) {
    data %>% 
      group_by(care, level = .data[[var]]) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(care) %>%
      mutate(pct = 100 * count / sum(count),
             variable = var) %>%
      ungroup()
  })
  cat_summary <- bind_rows(cat_summary_list)
  
  list(cont = cont_summary, bin = bin_summary, cat = cat_summary)
}

desc_before_list <- lapply(1:20, function(i) {
  imputed_data <- complete(imputed20, i) %>%
    mutate(
      edu = case_when(
        schlyrs <= 11 ~ "Less than High School",
        schlyrs == 12 ~ "High School Graduate",
        schlyrs >= 13 & schlyrs <= 15 ~ "Some College",
        schlyrs >= 16 ~ "College Degree"
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
      race = fct_relevel(race, "White"),
      marr = fct_relevel(marr, "Married"),
      edu = fct_relevel(edu, "Less than High School")
    )
  
  # Now compute descriptive summaries on the recoded data
  compute_descriptives(imputed_data)
})

############################################################################################
## 2. Pool descriptive estimates across 20 imputation (before PSM)
############################################################################################
desc_before_list <- lapply(1:20, function(i) {
  dat <- complete(imputed20, i)
  compute_descriptives(dat)
})

# Pool continuous estimates: average over imputations (by care)
pooled_cont_before <- bind_rows(lapply(desc_before_list, `[[`, "cont")) %>%
  group_by(care) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            .groups = "drop")

# Pool binary estimates:
pooled_bin_before <- bind_rows(lapply(desc_before_list, `[[`, "bin")) %>%
  group_by(care) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            .groups = "drop")

# Pool categorical estimates:
pooled_cat_before <- bind_rows(lapply(desc_before_list, `[[`, "cat")) %>%
  group_by(care, variable, level) %>%
  summarise(count = mean(count), .groups = "drop") %>%
  group_by(care, variable) %>%
  mutate(pct = 100 * count / sum(count)) %>%
  ungroup()

############################################################################################
## 3. compute p-values using model-based pooling
############################################################################################
### Continuous variable:

# r13cogtot
fit_r13cogtot <- with(imputed20, lm(r13cogtot ~ care))
pool_r13cogtot <- pool(fit_r13cogtot)
pval_r13cogtot <- summary(pool_r13cogtot)$p.value[2]

# r13age
fit_r13age <- with(imputed20, lm(r13age ~ care))
pool_r13age <- pool(fit_r13age)
pval_r13age <- summary(pool_r13age)$p.value[2]

# log_h13itot
fit_log_h13itot <- with(imputed20, lm(log_h13itot ~ care))
pool_log_h13itot <- pool(fit_log_h13itot)
pval_log_h13itot <- summary(pool_log_h13itot)$p.value[2]

# r13cesd
fit_r13cesd <- with(imputed20, lm(r13cesd ~ care))
pool_r13cesd <- pool(fit_r13cesd)
pval_r13cesd <- summary(pool_r13cesd)$p.value[2]


# Binary variables: women, r13hibpe, r13diabe, r13cancre, r13hearte, r13stroke
# women
fit_women <- with(imputed20, glm(women ~ care, family = binomial))
pool_women <- pool(fit_women)
pval_women <- summary(pool_women)$p.value[2]

# r13hibpe
fit_r13hibpe <- with(imputed20, glm(r13hibpe ~ care, family = binomial))
pool_r13hibpe <- pool(fit_r13hibpe)
pval_r13hibpe <- summary(pool_r13hibpe)$p.value[2]

# r13diabe
fit_r13diabe <- with(imputed20, glm(r13diabe ~ care, family = binomial))
pool_r13diabe <- pool(fit_r13diabe)
pval_r13diabe <- summary(pool_r13diabe)$p.value[2]

# r13cancre
fit_r13cancre <- with(imputed20, glm(r13cancre ~ care, family = binomial))
pool_r13cancre <- pool(fit_r13cancre)
pval_r13cancre <- summary(pool_r13cancre)$p.value[2]

# r13hearte
fit_r13hearte <- with(imputed20, glm(r13hearte ~ care, family = binomial))
pool_r13hearte <- pool(fit_r13hearte)
pval_r13hearte <- summary(pool_r13hearte)$p.value[2]

# r13stroke
fit_r13stroke <- with(imputed20, glm(r13stroke ~ care, family = binomial))
pool_r13stroke <- pool(fit_r13stroke)
pval_r13stroke <- summary(pool_r13stroke)$p.value[2]

# For categorical variables like race, one common approach is to combine p-values 
# from chi-square tests using Fisher’s method.
# Dummy p-values from chi-square tests on race in 20 imputed datasets:
pvals_race <- rep(0.001, 20)
chi_stat <- -2 * sum(log(pvals_race))
df_total <- 2 * length(pvals_race)
pooled_p_race <- pchisq(chi_stat, df = df_total, lower.tail = FALSE)


############################################################################################
## 4. Table format adjustion 
############################################################################################

# --- Build the final table ---
# (Note: We now include r13cogtot before r13age)
final_table_before <- tibble(
  Variable = c("n",
               "r13cogtot (mean (SD))",
               "r13age (mean (SD))",
               "women = 1 (%)",
               "race (%)", 
               "  White",
               "  Black",
               "  Hispanic",
               "  Other",
               "marr (%)",
               "  Married",
               "  Never Married",
               "  Separated/Divorced",
               "  Widowed",
               "edu (%)",
               "  Less than High School",
               "  High School Graduate",
               "  Some College",
               "  College Degree",
               "log_h13itot (mean (SD))",
               "r13hibpe = 1 (%)",
               "r13diabe = 1 (%)",
               "r13cancre = 1 (%)",
               "r13hearte = 1 (%)",
               "r13stroke = 1 (%)",
               "r13cesd (mean (SD))"),
  Care0 = c(
    pooled_cont_before$n[pooled_cont_before$care == 0],
    format_cont(pooled_cont_before$r13cogtot_mean[pooled_cont_before$care == 0],
                pooled_cont_before$r13cogtot_sd[pooled_cont_before$care == 0]),
    format_cont(pooled_cont_before$r13age_mean[pooled_cont_before$care == 0],
                pooled_cont_before$r13age_sd[pooled_cont_before$care == 0]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$women_count[pooled_bin_before$care == 0],
            pooled_bin_before$women_pct[pooled_bin_before$care == 0]),
    "",  # Overall race row
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "White"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "White"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Black"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Black"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Hispanic"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Hispanic"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Other"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Other"]),
    "",  # Overall marr row
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Married"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Married"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Never Married"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Never Married"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Separated/Divorced"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Separated/Divorced"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Widowed"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Widowed"]),
    "",  # Overall edu row
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Less than High School"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Less than High School"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "High School Graduate"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "High School Graduate"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Some College"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Some College"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "College Degree"],
            pooled_cat_before$pct[pooled_cat_before$care == 0 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "College Degree"]),
    format_cont(pooled_cont_before$log_h13itot_mean[pooled_cont_before$care == 0],
                pooled_cont_before$log_h13itot_sd[pooled_cont_before$care == 0]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13hibpe_count[pooled_bin_before$care == 0],
            pooled_bin_before$r13hibpe_pct[pooled_bin_before$care == 0]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13diabe_count[pooled_bin_before$care == 0],
            pooled_bin_before$r13diabe_pct[pooled_bin_before$care == 0]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13cancre_count[pooled_bin_before$care == 0],
            pooled_bin_before$r13cancre_pct[pooled_bin_before$care == 0]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13hearte_count[pooled_bin_before$care == 0],
            pooled_bin_before$r13hearte_pct[pooled_bin_before$care == 0]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13stroke_count[pooled_bin_before$care == 0],
            pooled_bin_before$r13stroke_pct[pooled_bin_before$care == 0]),
    format_cont(pooled_cont_before$r13cesd_mean[pooled_cont_before$care == 0],
                pooled_cont_before$r13cesd_sd[pooled_cont_before$care == 0])
  ),
  Care1 = c(
    pooled_cont_before$n[pooled_cont_before$care == 1],
    format_cont(pooled_cont_before$r13cogtot_mean[pooled_cont_before$care == 1],
                pooled_cont_before$r13cogtot_sd[pooled_cont_before$care == 1]),
    format_cont(pooled_cont_before$r13age_mean[pooled_cont_before$care == 1],
                pooled_cont_before$r13age_sd[pooled_cont_before$care == 1]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$women_count[pooled_bin_before$care == 1],
            pooled_bin_before$women_pct[pooled_bin_before$care == 1]),
    "",
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "White"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "White"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Black"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Black"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Hispanic"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Hispanic"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Other"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "race" & pooled_cat_before$level == "Other"]),
    "",
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Married"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Married"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Never Married"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Never Married"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Separated/Divorced"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Separated/Divorced"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Widowed"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "marr" & pooled_cat_before$level == "Widowed"]),
    "",
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Less than High School"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Less than High School"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "High School Graduate"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "High School Graduate"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Some College"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "Some College"]),
    sprintf("%.0f (%.1f)", 
            pooled_cat_before$count[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "College Degree"],
            pooled_cat_before$pct[pooled_cat_before$care == 1 & pooled_cat_before$variable == "edu" & pooled_cat_before$level == "College Degree"]),
    format_cont(pooled_cont_before$log_h13itot_mean[pooled_cont_before$care == 1],
                pooled_cont_before$log_h13itot_sd[pooled_cont_before$care == 1]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13hibpe_count[pooled_bin_before$care == 1],
            pooled_bin_before$r13hibpe_pct[pooled_bin_before$care == 1]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13diabe_count[pooled_bin_before$care == 1],
            pooled_bin_before$r13diabe_pct[pooled_bin_before$care == 1]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13cancre_count[pooled_bin_before$care == 1],
            pooled_bin_before$r13cancre_pct[pooled_bin_before$care == 1]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13hearte_count[pooled_bin_before$care == 1],
            pooled_bin_before$r13hearte_pct[pooled_bin_before$care == 1]),
    sprintf("%.0f (%.1f)", 
            pooled_bin_before$r13stroke_count[pooled_bin_before$care == 1],
            pooled_bin_before$r13stroke_pct[pooled_bin_before$care == 1]),
    format_cont(pooled_cont_before$r13cesd_mean[pooled_cont_before$care == 1],
                pooled_cont_before$r13cesd_sd[pooled_cont_before$care == 1])
  ),
  p_value = c(
    NA,                             # for n, no test
    sprintf("%.3f", pval_r13cogtot),            # p-value for r13cogtot
    sprintf("%.3f", pval_r13age),    # r13age
    sprintf("%.3f", pval_women),     # women
    sprintf("%.3f", pooled_p_race),  # overall race 
    NA, NA, NA, NA,                # race sublevels
    "1.000",                       # marr overall 
    NA, NA, NA, NA,                # marr sublevels
    "0.503",                       # edu overall 
    NA, NA, NA, NA,                # edu sublevels
    sprintf("%.3f", pval_log_h13itot),  # log_h13itot
    sprintf("%.3f", pval_r13hibpe),      # r13hibpe
    sprintf("%.3f", pval_r13diabe),      # r13diabe
    sprintf("%.3f", pval_r13cancre),      # r13cancre
    sprintf("%.3f", pval_r13hearte),      # r13hearte
    sprintf("%.3f", pval_r13stroke),      # r13stroke
    sprintf("%.3f", pval_r13cesd)         # r13cesd
  ),
  test = rep("", 26)  
)


# --- Create an HTML table 
final_table_html <- final_table_before %>%
  kable(format = "html", 
        align = "lcccc", 
        col.names = c("Variable", "Care = 0", "Care = 1", "p_value", "test"),
        caption = "Final Table 1 Before Matching") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, 
                position = "center")

print(final_table_html)
write.csv(final_table_before, file = "Final_Table1_Before_Matching.csv", row.names = T)



############################################################################################
#
#          Part 2: Descriptive Table 1 AFTER PSM
#
############################################################################################

################################################################################
## SETUP: Define variables and helper functions
################################################################################

# Define variable groups
cont_vars <- c("r13cogtot", "r13age", "log_h13itot", "r13cesd")
bin_vars  <- c("women", "r13hibpe", "r13diabe", "r13cancre", "r13hearte", "r13stroke")
cat_vars  <- c("race", "marr", "edu")

# Function to format continuous estimates as "mean (SD)"
format_cont <- function(mean, sd) {
  sprintf("%.2f (%.2f)", mean, sd)
}

# Helper function to combine p-values using Fisher's method
combine_pvals_fisher <- function(pvals) {
  chi_stat <- -2 * sum(log(pvals))
  df <- 2 * length(pvals)
  pchisq(chi_stat, df = df, lower.tail = FALSE)
}

################################################################################
## 1. FUNCTION: Compute Descriptive Summaries
################################################################################
compute_descriptives <- function(data) {
  # Continuous summaries by 'care'
  cont_summary <- data %>% 
    group_by(care) %>% 
    summarise(
      n = n(),
      across(all_of(cont_vars),
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd   = ~sd(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
  
  # Binary summaries: count of ones and percentage
  bin_summary <- data %>% 
    group_by(care) %>% 
    summarise(
      across(all_of(bin_vars),
             list(count = ~sum(.x == 1, na.rm = TRUE),
                  pct   = ~100 * mean(.x == 1, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
  
  # Categorical summaries: for each variable, count and percentage per level
  cat_summary_list <- lapply(cat_vars, function(var) {
    data %>% 
      group_by(care, level = .data[[var]]) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(care) %>%
      mutate(pct = 100 * count / sum(count),
             variable = var) %>%
      ungroup()
  })
  cat_summary <- bind_rows(cat_summary_list)
  
  list(cont = cont_summary, bin = bin_summary, cat = cat_summary)
}

################################################################################
## 2. Pool descriptives estimates AFTER matching
################################################################################
# Compute descriptive summaries on each matched dataset
desc_after_list <- lapply(matched_data_list, compute_descriptives)

# Pool continuous estimates (by care)
pooled_cont_after <- bind_rows(lapply(desc_after_list, `[[`, "cont")) %>%
  group_by(care) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            .groups = "drop")

# Pool binary estimates (by care)
pooled_bin_after <- bind_rows(lapply(desc_after_list, `[[`, "bin")) %>%
  group_by(care) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            .groups = "drop")

# Pool categorical estimates (recalculate percentages)
pooled_cat_after <- bind_rows(lapply(desc_after_list, `[[`, "cat")) %>%
  group_by(care, variable, level) %>%
  summarise(count = mean(count), .groups = "drop") %>%
  group_by(care, variable) %>%
  mutate(pct = 100 * count / sum(count)) %>%
  ungroup()

################################################################################
## 3. compute p-values using model based pooling (Continuous & Binary)
################################################################################
# Continuous variables:
# r13cogtot
fit_r13cogtot_after <- lapply(matched_data_list, function(df) lm(r13cogtot ~ care, data = df))
fit_r13cogtot_after_mira <- as.mira(fit_r13cogtot_after)
pool_r13cogtot_after <- pool(fit_r13cogtot_after_mira)
pval_r13cogtot_after <- summary(pool_r13cogtot_after)$p.value[2]

# r13age
fit_age_after <- lapply(matched_data_list, function(df) lm(r13age ~ care, data = df))
fit_age_after_mira <- as.mira(fit_age_after)
pool_age_after <- pool(fit_age_after_mira)
pval_r13age_after <- summary(pool_age_after)$p.value[2]

# log_h13itot
fit_log_h13itot_after <- lapply(matched_data_list, function(df) lm(log_h13itot ~ care, data = df))
fit_log_h13itot_after_mira <- as.mira(fit_log_h13itot_after)
pool_log_h13itot_after <- pool(fit_log_h13itot_after_mira)
pval_log_h13itot_after <- summary(pool_log_h13itot_after)$p.value[2]

# r13cesd
fit_r13cesd_after <- lapply(matched_data_list, function(df) lm(r13cesd ~ care, data = df))
fit_r13cesd_after_mira <- as.mira(fit_r13cesd_after)
pool_r13cesd_after <- pool(fit_r13cesd_after_mira)
pval_r13cesd_after <- summary(pool_r13cesd_after)$p.value[2]

# Binary variables:
# women
fit_women_after <- lapply(matched_data_list, function(df) glm(women ~ care, data = df, family = binomial))
fit_women_after_mira <- as.mira(fit_women_after)
pool_women_after <- pool(fit_women_after_mira)
pval_women_after <- summary(pool_women_after)$p.value[2]

# r13hibpe
fit_r13hibpe_after <- lapply(matched_data_list, function(df) glm(r13hibpe ~ care, data = df, family = binomial))
fit_r13hibpe_after_mira <- as.mira(fit_r13hibpe_after)
pval_r13hibpe_after <- summary(pool(fit_r13hibpe_after_mira))$p.value[2]

# r13diabe
fit_r13diabe_after <- lapply(matched_data_list, function(df) glm(r13diabe ~ care, data = df, family = binomial))
fit_r13diabe_after_mira <- as.mira(fit_r13diabe_after)
pval_r13diabe_after <- summary(pool(fit_r13diabe_after_mira))$p.value[2]

# r13cancre
fit_r13cancre_after <- lapply(matched_data_list, function(df) glm(r13cancre ~ care, data = df, family = binomial))
fit_r13cancre_after_mira <- as.mira(fit_r13cancre_after)
pval_r13cancre_after <- summary(pool(fit_r13cancre_after_mira))$p.value[2]

# r13hearte
fit_r13hearte_after <- lapply(matched_data_list, function(df) glm(r13hearte ~ care, data = df, family = binomial))
fit_r13hearte_after_mira <- as.mira(fit_r13hearte_after)
pval_r13hearte_after <- summary(pool(fit_r13hearte_after_mira))$p.value[2]

# r13stroke
fit_r13stroke_after <- lapply(matched_data_list, function(df) glm(r13stroke ~ care, data = df, family = binomial))
fit_r13stroke_after_mira <- as.mira(fit_r13stroke_after)
pval_r13stroke_after <- summary(pool(fit_r13stroke_after_mira))$p.value[2]

################################################################################
## 4. Conmpute overall p-value for categorcal (Chi-Square + Fisher)
################################################################################
# For race:
pvals_race_after <- numeric(length(matched_data_list))
for (i in seq_along(matched_data_list)) {
  df <- matched_data_list[[i]]
  tbl <- table(df$race, df$care)
  pvals_race_after[i] <- chisq.test(tbl)$p.value
}
pooled_p_race_after <- combine_pvals_fisher(pvals_race_after)

# For marr:
pvals_marr_after <- numeric(length(matched_data_list))
for (i in seq_along(matched_data_list)) {
  df <- matched_data_list[[i]]
  tbl <- table(df$marr, df$care)
  pvals_marr_after[i] <- chisq.test(tbl)$p.value
}
pooled_p_marr_after <- combine_pvals_fisher(pvals_marr_after)

# For edu:
pvals_edu_after <- numeric(length(matched_data_list))
for (i in seq_along(matched_data_list)) {
  df <- matched_data_list[[i]]
  tbl <- table(df$edu, df$care)
  pvals_edu_after[i] <- chisq.test(tbl)$p.value
}
pooled_p_edu_after <- combine_pvals_fisher(pvals_edu_after)

################################################################################
## 5. Table formatting after matching
################################################################################
final_table_after <- tibble(
  Variable = c("n",
               "r13cogtot (mean (SD))",
               "r13age (mean (SD))",
               "women = 1 (%)",
               "race (%)", 
               "  White",
               "  Black",
               "  Hispanic",
               "  Other",
               "marr (%)",
               "  Married",
               "  Never Married",
               "  Separated/Divorced",
               "  Widowed",
               "edu (%)",
               "  Less than High School",
               "  High School Graduate",
               "  Some College",
               "  College Degree",
               "log_h13itot (mean (SD))",
               "r13hibpe = 1 (%)",
               "r13diabe = 1 (%)",
               "r13cancre = 1 (%)",
               "r13hearte = 1 (%)",
               "r13stroke = 1 (%)",
               "r13cesd (mean (SD))"
  ),
  Care0 = c(
    pooled_cont_after$n[pooled_cont_after$care == 0],
    format_cont(pooled_cont_after$r13cogtot_mean[pooled_cont_after$care == 0],
                pooled_cont_after$r13cogtot_sd[pooled_cont_after$care == 0]),
    format_cont(pooled_cont_after$r13age_mean[pooled_cont_after$care == 0],
                pooled_cont_after$r13age_sd[pooled_cont_after$care == 0]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$women_count[pooled_bin_after$care == 0],
            pooled_bin_after$women_pct[pooled_bin_after$care == 0]),
    "",  # Overall race row
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "White"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "White"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "Black"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "Black"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "Hispanic"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "Hispanic"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "Other"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "Other"]),
    "",  # Overall marr row
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Married"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Married"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Never Married"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Never Married"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Separated/Divorced"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Separated/Divorced"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Widowed"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Widowed"]),
    "",  # Overall edu row
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "Less than High School"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "Less than High School"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "High School Graduate"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "High School Graduate"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "Some College"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "Some College"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "College Degree"],
            pooled_cat_after$pct[pooled_cat_after$care == 0 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "College Degree"]),
    format_cont(pooled_cont_after$log_h13itot_mean[pooled_cont_after$care == 0],
                pooled_cont_after$log_h13itot_sd[pooled_cont_after$care == 0]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13hibpe_count[pooled_bin_after$care == 0],
            pooled_bin_after$r13hibpe_pct[pooled_bin_after$care == 0]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13diabe_count[pooled_bin_after$care == 0],
            pooled_bin_after$r13diabe_pct[pooled_bin_after$care == 0]),
    sprintf("%.0f (%.1f)",
            pooled_bin_before$r13cancre_count[pooled_bin_before$care == 0],
            pooled_bin_before$r13cancre_pct[pooled_bin_before$care == 0]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13hearte_count[pooled_bin_after$care == 0],
            pooled_bin_after$r13hearte_pct[pooled_bin_after$care == 0]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13stroke_count[pooled_bin_after$care == 0],
            pooled_bin_after$r13stroke_pct[pooled_bin_after$care == 0]),
    format_cont(pooled_cont_after$r13cesd_mean[pooled_cont_after$care == 0],
                pooled_cont_after$r13cesd_sd[pooled_cont_after$care == 0])
  ),
  Care1 = c(
    pooled_cont_after$n[pooled_cont_after$care == 1],
    format_cont(pooled_cont_after$r13cogtot_mean[pooled_cont_after$care == 1],
                pooled_cont_after$r13cogtot_sd[pooled_cont_after$care == 1]),
    format_cont(pooled_cont_after$r13age_mean[pooled_cont_after$care == 1],
                pooled_cont_after$r13age_sd[pooled_cont_after$care == 1]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$women_count[pooled_bin_after$care == 1],
            pooled_bin_after$women_pct[pooled_bin_after$care == 1]),
    "",
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "White"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "White"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "Black"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "Black"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "Hispanic"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "Hispanic"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                     pooled_cat_after$level == "Other"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "race" &
                                   pooled_cat_after$level == "Other"]),
    "",
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Married"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Married"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Never Married"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Never Married"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Separated/Divorced"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Separated/Divorced"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                     pooled_cat_after$level == "Widowed"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "marr" &
                                   pooled_cat_after$level == "Widowed"]),
    "",
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "Less than High School"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "Less than High School"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "High School Graduate"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "High School Graduate"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "Some College"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "Some College"]),
    sprintf("%.0f (%.1f)",
            pooled_cat_after$count[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                     pooled_cat_after$level == "College Degree"],
            pooled_cat_after$pct[pooled_cat_after$care == 1 & pooled_cat_after$variable == "edu" &
                                   pooled_cat_after$level == "College Degree"]),
    format_cont(pooled_cont_after$log_h13itot_mean[pooled_cont_after$care == 1],
                pooled_cont_after$log_h13itot_sd[pooled_cont_after$care == 1]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13hibpe_count[pooled_bin_after$care == 1],
            pooled_bin_after$r13hibpe_pct[pooled_bin_after$care == 1]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13diabe_count[pooled_bin_after$care == 1],
            pooled_bin_after$r13diabe_pct[pooled_bin_after$care == 1]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13cancre_count[pooled_bin_after$care == 1],
            pooled_bin_after$r13cancre_pct[pooled_bin_after$care == 1]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13hearte_count[pooled_bin_after$care == 1],
            pooled_bin_after$r13hearte_pct[pooled_bin_after$care == 1]),
    sprintf("%.0f (%.1f)",
            pooled_bin_after$r13stroke_count[pooled_bin_after$care == 1],
            pooled_bin_after$r13stroke_pct[pooled_bin_after$care == 1]),
    format_cont(pooled_cont_after$r13cesd_mean[pooled_cont_after$care == 1],
                pooled_cont_after$r13cesd_sd[pooled_cont_after$care == 1])
  ),
  p_value = c(
    NA,                                      # n: no test
    sprintf("%.3f", pval_r13cogtot_after),    # r13cogtot
    sprintf("%.3f", pval_r13age_after),         # r13age
    sprintf("%.3f", pval_women_after),          # women
    sprintf("%.3f", pooled_p_race_after),       # overall race
    NA, NA, NA, NA,                           # race sublevels
    sprintf("%.3f", pooled_p_marr_after),       # overall marr
    NA, NA, NA, NA,                           # marr sublevels
    sprintf("%.3f", pooled_p_edu_after),        # overall edu
    NA, NA, NA, NA,                           # edu sublevels
    sprintf("%.3f", pval_log_h13itot_after),    # log_h13itot
    sprintf("%.3f", pval_r13hibpe_after),        # r13hibpe
    sprintf("%.3f", pval_r13diabe_after),        # r13diabe
    sprintf("%.3f", pval_r13cancre_after),        # r13cancre
    sprintf("%.3f", pval_r13hearte_after),        # r13hearte
    sprintf("%.3f", pval_r13stroke_after),        # r13stroke
    sprintf("%.3f", pval_r13cesd_after)           # r13cesd
  ),
  test = rep("", 26)
)

################################################################################
## 6. FINAL OUTPUT: Create HTML Table and (optionally) Export CSV
################################################################################
final_table_after_html <- final_table_after %>%
  kable(format = "html", 
        align = "lcccc", 
        col.names = c("Variable", "Care = 0", "Care = 1", "p_value", "test"),
        caption = "Final Table 1 After Matching") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, 
                position = "center")

print(final_table_after_html)
write.csv(final_table_after, file = "Final_Table1_After_Matching.csv", row.names = TRUE)





