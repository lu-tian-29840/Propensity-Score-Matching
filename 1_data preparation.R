# Peoject: caregiving and cognition--PSM approach
# code for Jounbal of aging and research
# caregiving variables are th eones difference than prebious version
# Feb 21, 2025
# Author: Lu Tian

### Sections 1/4
#   Section 1: Prepare data and create variables 






###################################################################################################
#                              <Section One >  Prepare data and create variables
###################################################################################################

# Part 1
######################  Merge data: 2016 half sample creation ######################     

# 1. Load data
hrs2020  <- readRDS("hrs2020.rds")
lbq2016 <- readRDS("lbq2016.rds")
rand2016 <- readRDS("rand2016.rds")
df.trk2016_clean <- readRDS("df.trk2016_clean.rds")
cognition2016 <- readRDS("cognition2016.rds")
care2016 <- readRDS("care2016.rds") 


# 1) merge lbq2016 and df.trk2016_clean
merged_data1 <- merge(lbq2016, df.trk2016_clean, by = "rahhidpn") # lbq2016: r13001A
merged_data1$hhid.y <- NULL 
merged_data1 <- merged_data1 %>% rename(hhid = hhid.x) %>%  arrange(rahhidpn) 

# 2) merge merged_data1 and rand2016
merged_data2 <- merge(merged_data1, rand2016, by = "rahhidpn") %>% arrange(rahhidpn)

# 3) Merge merged_data2 and cognition2016
merged_data3 <- merge(merged_data2, cognition2016, by = "rahhidpn") %>% arrange(rahhidpn)

# 4) Merge merged_data3 and h16f_r
hrs2016 <- merge(merged_data3, care2016, by = "rahhidpn") %>% arrange(rahhidpn)

hrs2016$samp2016 <- 1

# 5) Retain one value for variables that have subffix .y and .x
# Get the column names
col_names <- colnames(hrs2016)
# Identify and remove the .y variables
overlap_vars_y <- grep("\\.y$", col_names, value = T)
hrs2016 <- hrs2016[, !names(hrs2016) %in% overlap_vars_y]
# Rename the .x variables to remove the .x suffix
new_col_names <- gsub("\\.x$", "", colnames(hrs2016))
colnames(hrs2016) <- new_col_names

saveRDS(hrs2016,"hrs2016.rds")

# Part 2
######################  Merge data: 2018 half sample creation ######################  
lbq2018 <- readRDS("lbq2018.rds")
df.trk2018_clean <- readRDS("df.trk2018_clean.rds")
rand2018 <- readRDS("rand2018.rds")
cognition2018 <- readRDS("cognition2018.rds")

# 1) Merge lbq2018 and df.trk2018_clean
merged_data1a <- merge(lbq2018, df.trk2018_clean, by = "rahhidpn")
merged_data1a$hhid.y <- NULL 
merged_data1a <- merged_data1a %>% rename(hhid = hhid.x) %>%  arrange(rahhidpn) 

# 2) merge merged_data1a and rand2018
merged_data2a <- merge(merged_data1a, rand2018, by = "rahhidpn") %>% arrange(rahhidpn)


# 3) Merge merged_data2a and cognition2018
hrs2018 <- merge(merged_data2a, cognition2018, by = "rahhidpn") %>% arrange(rahhidpn)

# flag 2018 sample  
hrs2018$samp2018 <- 1


# 4) Retain one value for variables that have subffix .y and .x
# Get the column names
col_names <- colnames(hrs2018)
# Identify and remove the .y variables
overlap_vars_y <- grep("\\.y$", col_names, value = T)
hrs2018 <- hrs2018[, !names(hrs2018) %in% overlap_vars_y]
# Identify and remove the .y variables
new_col_names <- gsub("\\.x$", "", colnames(hrs2018))

colnames(hrs2018) <- new_col_names

# 6) save hrs2018
saveRDS(hrs2018,"hrs2018.rds")


# Part 3
######    Bind rows of hrs2016 and hrs2018 for full sample of social support     #####

# 1) Remove the "r13" prefix and add "_a" suffix to each column name
# Make a copy of hrs2016
hrs2016_a <- hrs2016
# Get the existing column names
old_colnames <- colnames(hrs2016)
# Initialize new_colnames as empty to be populated inside the loop
new_colnames <- character(length(old_colnames))

# Loop through each column name
for (i in 1:length(old_colnames)) {
  # If the column name starts with "r13", remove it and add "_a"
  if (grepl("^r13", old_colnames[i])) {
    new_colnames[i] <- paste0(sub("^r13", "", old_colnames[i]), "_a")
  } else {
    new_colnames[i] <- old_colnames[i]
  }
}

# Set the new column names to the data frame
colnames(hrs2016_a) <- new_colnames


# 2) Remove the "r14" prefix and add "_a" suffix to each column name
# Get the existing column names
hrs2018_a <- hrs2018
old_colnames <- colnames(hrs2018)
# Initialize new_colnames as empty to be populated inside the loop
new_colnames <- character(length(old_colnames))

# Loop through each column name
for (i in 1:length(old_colnames)) {
  # If the column name starts with "r14", remove it and add "_a"
  if (grepl("^r14", old_colnames[i])) {
    new_colnames[i] <- paste0(sub("^r14", "", old_colnames[i]), "_a")
  } else {
    new_colnames[i] <- old_colnames[i]
  }
}
# Set the new column names to the data frame
colnames(hrs2018_a) <- new_colnames

# 3) Bind rows pf hrs2016 and hrs2018

# Selecting socual support relevant columns
hrs2016_ss <- dplyr::select(hrs2016_a, rahhidpn, lb001a_a, lb003_a:lb005b_a, lb006_a:lb017_a, lbcomp_a, lbelig_a, rescode_a)

hrs2016_ss$year <- 2016
hrs2016_ss$plqcomp <- ifelse(hrs2016_ss$lbelig_a == 1 & hrs2016_ss$lbcomp_a %in% c(1, 2), 1, 0) # binary indicators for PLQ completion 

hrs2018_ss <- dplyr::select(hrs2018_a, rahhidpn, lb001a_a, lb003_a:lb005b_a, lb006_a:lb017_a, lbcomp_a, lbelig_a, rescode_a)
hrs2018_ss$year <- 2018

hrs2018_ss$plqcomp <- ifelse(hrs2018_ss$lbelig_a == 1 & hrs2018_ss$lbcomp_a %in% c(1, 2), 1, 0)

hrs_2016_2018_ss <- rbind(hrs2016_ss, hrs2018_ss) %>% 
  arrange(rahhidpn) %>%      # n=37864 
  filter(rescode_a == 1001, (lbcomp_a == 1 | lbcomp_a == 2), lbelig_a == 1) # 12011; unique ID# 

# 4)  merge
df2016a <- merge(care2016, cognition2016, by = "rahhidpn") %>% arrange(rahhidpn)
df2016b <- merge(df2016a, rand2016, by = "rahhidpn") %>% arrange(rahhidpn)
df2016c <- merge(df2016b, df.trk2016_clean, by = "rahhidpn") %>% arrange(rahhidpn)
df2016_2018a <- merge(df2016c, hrs_2016_2018_ss, by = "rahhidpn") %>% arrange(rahhidpn)
df2016_2018b <- merge(df2016_2018a, rand2018, by = "rahhidpn") %>% arrange(rahhidpn)
df2016_2018 <- merge(df2016_2018b, cognition2018, by = "rahhidpn") %>% arrange(rahhidpn)


# 5) Retain one value for variables that have subffix .y and .x
# Get the column names
col_names <- colnames(df2016_2018)
# Identify and remove the .y variables
overlap_vars_y <- grep("\\.y$", col_names, value = T)
df2016_2018 <- df2016_2018[, !names(df2016_2018) %in% overlap_vars_y]
# Identify and remove the .y variables
new_col_names <- gsub("\\.x$", "", colnames(df2016_2018))
colnames(df2016_2018) <- new_col_names

# Part 4
######################   merge df2016_2018 + 2020 ###################### 
hrs2020_a <- hrs2020 %>% 
  dplyr::select(rahhidpn, r15age:samp2020, -version, -raceth, -women, -study_cog)

df_full <- merge(df2016_2018, hrs2020_a, by = "rahhidpn") %>% arrange(rahhidpn)
names(df_full)
# Part 5
###################### Variable selection  ###################### 
fullsamp2016_2020 <-  df_full %>% 
  dplyr::select(rahhidpn, hhid, pn, 
                r13f001:r13status, study_cog, r13mstat:birthmo, birthyr, degree, gender, race, schlyrs, secu, stratum, study,
                r13age, r13alive, raceth, women, lb001a_a, lb003_a:plqcomp, 
                r14mstat:r14status,
                r15age, r15alive, r15proxy, r15rescode, r15cogtotp, r15status, r15hibpe:samp2020) %>%    # 11915
  filter(r15rescode == 1001)  %>%  # 10350
  filter(r13status == 1)  %>%  # 10329
  filter(r15status == 1)  %>% # 9645 
  filter(r14status == 1) %>%  # 9472
  arrange(rahhidpn)

saveRDS(fullsamp2016_2020,"fullsamp2016_2020.rds")


