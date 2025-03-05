# Peoject: caregiving and cognition--psm approach
# code for Jounbal of aging and health
# caregiving variables: a composite binary
# Feb 21, 2025
# Author: Lu Tian


### Sections 
#   Section 2: Skip patterns for network features and multiple imputation

library(tidyverse)    # loads dplyr, ggplot2, tidyr, tibble, etc. see tidyverse.org/packages
library(Hmisc)     # adds label to variables, missing data coorelation
library(haven)     # read .dta file
library(kableExtra) # Make nice table
library(psych)    # to describe variables, Cronbach's Alpha 
library(mediation) # for bootrap
library(stargazer)
library(car)
library(visdat) # visualize missing
library(naniar) # visualize missing
library(Amelia)
library(mice)
library(lattice)
library(gridExtra)
library(survey)




fullsamp2016_2020 <- readRDS("fullsamp2016_2020.rds")

names(fullsamp2016_2020)

kidcare2016 <- read_dta("kidcare2016.dta")
care2018 <- readRDS("care2018.rds")

df.mi <- fullsamp2016_2020 %>% filter(r13age >= 65)  # n= 4707
df.mi <- df.mi[complete.cases(df.mi[, c("r13mstat", "r14mstat")]), ] # 4697
df.mi <- merge(df.mi, care2018, by = "rahhidpn", all.x = TRUE) # 4697
df.mi <- merge(df.mi, kidcare2016, by = "rahhidpn", all.x = TRUE) # 4697

saveRDS(df.mi, file = "df.mi.rds")



# r13f119, r13f139, lb001a_a
# Transform PLB001A into lb001a_a
df.mi$care <- ifelse(df.mi$lb001a_a == 7, 0, 
                     ifelse(df.mi$lb001a_a %in% 1:6, 1, NA))

table(df.mi$care, useNA = "ifany")
# 0    1   <NA> 
# 3159 1446  102 


##############################################################################
df.mi$r13f119[df.mi$r13f119 == 8] <- NA; df.mi$r13f119[df.mi$r13f139 == 8] <- NA
df.mi$r13f119[df.mi$r13f119 == 5] <- 0; df.mi$r13f119[df.mi$r13f139 == 5] <- 0

df.mi$r14f119[df.mi$r13f119 == 8] <- NA; df.mi$r14f119[df.mi$r13f139 == 8] <- NA
df.mi$r14f119[df.mi$r13f119 == 5] <- 0; df.mi$r14f119[df.mi$r13f139 == 5] <- 0

df.mi <- df.mi %>%
  mutate(care = case_when(
    r13f119 == 1 & (care == 0 | is.na(care)) ~ 1,  
    TRUE ~ care                                   
  ))
table(df.mi$care, useNA = "ifany")
                          # 0    1 <NA> 
                          #   3158 1492  100
df.mi <- df.mi %>%
  mutate(care = case_when(
    r13f139 == 1 & (care == 0 | is.na(care)) ~ 1,  
    TRUE ~ care                                   
  ))
                            # 0    1 <NA> 
                            #   3107 1547   96 

table(df.mi$s13iadl5h, useNA = "ifany")

##### help personal acitvities for spouse
df.mi <- df.mi %>%
  mutate(care = case_when(
    s13iadl5h %in% 1:5 & (care == 0 | is.na(care)) ~ 1,
    TRUE ~ care
  ))

table(df.mi$care, useNA = "ifany")
# 0    1 <NA> 
#   2984 1677   89 

df.mi <- df.mi %>%
  mutate(care = case_when(
    s13iadl5a %in% 1:5 & (care == 0 | is.na(care)) ~ 1,
    TRUE ~ care
  ))

table(df.mi$care, useNA = "ifany")

# 0    1 <NA> 
#   2953 1708   89 

df.mi <- df.mi %>%
  mutate(care = case_when(
    s14iadl5h %in% 1:5 & (care == 0 | is.na(care)) ~ 1,
    TRUE ~ care
  ))

table(df.mi$care, useNA = "ifany")

# 0    1 <NA> 
#   2873 1790   87 


df.mi <- df.mi %>%
  mutate(care = case_when(
    s14iadl5a %in% 1:5 & (care == 0 | is.na(care)) ~ 1,
    TRUE ~ care
  ))

table(df.mi$care, useNA = "ifany")

# 0    1 <NA> 
#   2848 1816   86 


df.mi <- df.mi %>%
  mutate(care = case_when(
    r14f119 == 1 & (care == 0 | is.na(care)) ~ 1,  
    TRUE ~ care                                   
  ))

# 0    1 <NA> 
#   2816 1805   86 

df.mi <- df.mi %>%
  mutate(care = case_when(
    r14f139 == 1 & (care == 0 | is.na(care)) ~ 1,  
    TRUE ~ care                                   
  ))

# 0    1 <NA> 
#   2834 1830   86 



# Does father need help with basic personal needs like dressing, eating, or bathing?
df.mi <- df.mi %>%
  mutate(care = case_when(
    r13f013 == 1 & (care == 0 | is.na(care)) ~ 1,  
    TRUE ~ care                                   
  ))
# 0    1 <NA> 
#   2826 1838   86 

# r13f104: $ -> parents
# Not counting any shared housing or shared food, did you [or your] [late]
# [husband/wife/partner] give financial help to your [(deceased)
#                                                     parents/mother/father/mother (and/or her husband)/father (and/or his wife)]
# amounting to $500 or more [since [Prev Wave Family R IW Month], [Prev Wave
#                                                                  Family R IW Year]/in the last two years]?


df.mi <- df.mi %>%
  mutate(care = case_when(
    r13f104 == 1 & (care == 0 | is.na(care)) ~ 1,  
    TRUE ~ care                                   
  ))
table(df.mi$care, useNA = "ifany")

# 0    1 <NA> 
#   2804 1862   84 

table(df.mi$r13e056, useNA = "ifany")




##############################################################################

pMiss <- sapply(df.mi, function(x) sum(is.na(x)) / length(x) * 100)


df.mi1<- dplyr::select(df.mi, -(r13f001:r13vdate))
names(df.mi1)

###################################################################################################
#                              <Section Two >  Skip patterns for network features
###################################################################################################

### I.                                     <Spouse>



## 1. lb003_a: Do you have a husband, wife, or partner with whom you live? Check marital status <------Network composition 1 OLS (0/1)

# lb003_a: 5 = no, 5 -> 0
df.mi1$lb003_a[df.mi1$lb003_a == 5] <- 0
df.mi1 <- df.mi1 %>% mutate(lb003_a = ifelse(is.na(lb003_a) & r13mstat %in% c(5, 7, 8), 0, lb003_a))

df.mi1 <- df.mi1 %>% mutate(lb003_a = ifelse(is.na(lb003_a) & r14mstat %in% c(5, 7, 8), 0, lb003_a))

table(df.mi1$r13mstat, df.mi1$lb003_a, useNA = "ifany"); table(df.mi1$r14mstat, df.mi1$lb003_a, useNA = "ifany")

## 2. Spousal soc support                                                                      <------ positive soc support 1 OLS (1-4)
# lb004a_a: How much do they really understand the way you feel about things?                
# lb004b_a: How much can you rely on them if you have a serious problem?
# lb004c_a: How much can you open up to them if you need to talk about your worries?

# Replace NA values under specified conditions
df.mi1 <- df.mi1 %>% mutate(across(c(lb004a_a, lb004b_a, lb004c_a), ~ifelse(r13mstat %in% c(5, 7, 8) & is.na(.), 0, .)))



df.mi1 <- df.mi1 %>% mutate(across(c(lb004a_a, lb004b_a, lb004c_a), ~ifelse(r14mstat %in% c(5, 7, 8) & is.na(.), 0, .)))


## 3, Spousal strain      scale 1-4                                                             <------ strain 1 MI (1-4)
# lb004d_a:  How often do they make too many demands on you?
# lb004e_a: How much do they criticize you?
# lb004f_a: How much do they let you down when you are counting on them?
# lb004g_a: How much do they get on your nerves?

# Replace NA values under specified conditions
df.mi1 <- df.mi1 %>%
  mutate(across(c(lb004d_a, lb004e_a, lb004f_a, lb004g_a),
                ~if_else((r13mstat %in% c(5, 7, 8) | r14mstat %in% c(5, 7, 8)) & is.na(.), 0, .)))

## 4. 
# lb005_a: How close is your relationship with your partner or spouse?                          <------ close ties 1 OLS (1 - 4)
# lb005a_a: Overall, how enjoyable is the time you spend together with your spouse/partner?
# lb005b_a: In your free time, do you and your spouse mostly do things together or separately?

# Replace NA values under specified conditions
df.mi1 <- df.mi1 %>%
  mutate(across(c(lb005_a, lb005a_a, lb005b_a),
                ~ifelse((r13mstat %in% c(5, 7, 8) | r14mstat %in% c(5, 7, 8) & is.na(.)), 0, .)))

table(df.mi1$r13mstat, df.mi1$lb005_a, useNA = "ifany")                                         #<------ close ties 1 MI (1 - 4)

table(df.mi1$r13mstat, df.mi1$lb005a_a, useNA = "ifany")                                        #<------ quality 1 MI (1-4)
table(df.mi1$r13mstat, df.mi1$lb005b_a, useNA = "ifany")                                        #<------ quality 1 MI (1-4)

### II.                                     <Children>

# lb006_a: 5 = no, 5 -> 0
df.mi1$lb006_a[df.mi1$lb006_a == 5] <- 0                                                      # <------Network composition 2 OLS (0/1)

## 1. lb006_a: Do you have any living children? Check marital status   
df.mi1 <- df.mi1 %>% 
  mutate(lb006_a = if_else(is.na(lb006_a) & (raevbrn == 0 | h13child == 0 | h14child == 0), 0, lb006_a))

## 2. Children soc support                                                                      
# lb007a_a: How much do they really understand the way you feel about things?                
# lb007b_a: How much can you rely on them if you have a serious problem?
# lb007c_a: How much can you open up to them if you need to talk about your worries?

## 3, Children strain      scale 1-4                                                            
# lb007d_a:  How often do they make too many demands on you?
# lb007e_a: How much do they criticize you?
# lb007f_a: How much do they let you down when you are counting on them?
# lb007g_a: How much do they get on your nerves?

df.mi1 <- df.mi1 %>%
  mutate(across(c(lb007a_a, lb007b_a, lb007c_a, lb007d_a, lb007e_a, lb007f_a, lb007g_a),
                ~if_else(lb006_a == 0 & is.na(.), 0, .)))
## 4. 
# lb008a_a: how often do you do each of the following with any of your children, not counting any who live 
# with you     
# lb008b_a: how often do you speak on the phone
# lb008c_a: Write or email
# lb008d_a: Communicate by Skype, Facebook, or other social media

df.mi1 <- df.mi1 %>%
  mutate(across(c(lb008a_a, lb008b_a, lb008c_a, lb008d_a, lb009_a),
                ~if_else(lb006_a == 0 & is.na(.), 0, .)))
# lb009_a: How many of your children would you say you have a close relationship with??                         
table(df.mi1$lb006_a, df.mi1$lb009_a, useNA = "ifany")                                        # <------ close ties 2 OLS (continuous)

### IIT.                                     <Immediate family>

# lb010_a: Do you have any OTHER IMMEDIATE FAMILY, for example, any brothers or sisters, parents, 
# cousins or grandchildren or other family member?


df.mi1$lb010_a[df.mi1$lb010_a == 5] <- 0                                                      # <------Network composition 3 OLS (0/1)

df.mi1 <- df.mi1 %>%
  mutate(lb010_a = if_else(r13momliv == 0 & r13dadliv == 0 & r13livsib == 0 & r14livsib == 0 & lb006_a == 0 & is.na(lb010_a), 0, lb010_a))


## 2. Other family member soc support                                                                      
# lb011a_a: How much do they really understand the way you feel about things?                
# lb011b_a: How much can you rely on them if you have a serious problem?
# lb011c_a: How much can you open up to them if you need to talk about your worries?

## 3, Other family member strain      scale 1-4                                                            
# lb011d_a:  How often do they make too many demands on you?
# lb011e_a: How much do they criticize you?
# lb011f_a: How much do they let you down when you are counting on them?
# lb011g_a: How much do they get on your nerves?

df.mi1 <- df.mi1 %>%
  mutate(across(c(lb011a_a, lb011b_a, lb011c_a, lb011d_a, lb011e_a, lb011f_a, lb011g_a),
                ~if_else(lb010_a == 0 & is.na(.), 0, .)))


## 4. 
# lb012a_a: how often do you do each of the following with any of your Other family member, not counting any who live 
# with you     
# lb012b_a: how often do you speak on the phone
# lb012c_a: Write or email
# lb012d_a: Communicate by Skype, Facebook, or other social media
table(df.mi1$lb010_a, df.mi1$lb012a_a, useNA = "ifany")                                        # <------ Meet up 2             MI (1-6)                 
table(df.mi1$lb010_a, df.mi1$lb012b_a, useNA = "ifany")                                        # <------ speak on the phone 2  MI  (1-6)
table(df.mi1$lb010_a, df.mi1$lb012c_a, useNA = "ifany")                                        # <------ Write or email 2      MI (1-6)
table(df.mi1$lb010_a, df.mi1$lb012d_a, useNA = "ifany")                                        # <------ Communicate  2        NI (1-6)

# lb013_a: How many of your Other family member would you say you have a close relationship with??                         
table(df.mi1$lb010_a, df.mi1$lb013_a, useNA = "ifany")                                        # <------ close ties 2 OLS (continuous)

table(df.mi1$lb010_a, df.mi1$lb013a_a, useNA = "ifany")                                      # relatives living in your neighborhood MI (0/1)
df.mi1$lb013a_a[df.mi1$lb013a_a == 5] <- 0  

df.mi1 <- df.mi1 %>%
  mutate(across(c(lb012a_a, lb012b_a, lb012c_a, lb012d_a, lb013_a, lb013a_a),
                ~if_else(lb010_a == 0 & is.na(.), 0, .)))

### IV.                                     <Friend>

# lb014_a: DDo you have any friends?
df.mi1$lb014_a[df.mi1$lb014_a == 5] <- 0                                                      # <------Network composition 4 OLS (0/1)


## 2. Friends soc support                                                                      
# lb015a_a: How much do they really understand the way you feel about things?                
# lb015b_a: How much can you rely on them if you have a serious problem?
# lb015c_a: How much can you open up to them if you need to talk about your worries?

## 3, Friends strain      scale 1-4                                                            
# lb015d_a:  How often do they make too many demands on you?
# lb015e_a: How much do they criticize you?
# lb015f_a: How much do they let you down when you are counting on them?
# lb015g_a: How much do they get on your nerves?

df.mi1 <- df.mi1 %>%
  mutate(across(c(lb015a_a, lb015b_a, lb015c_a, lb015d_a, lb015e_a, lb015f_a, lb015g_a),
                ~if_else(lb014_a == 0 & is.na(.), 0, .)))

## 4. 
# lb014_a: DDo you have any friends?
# lb016a_a: how often do you do each of the following with any of your Friends, not counting any who live 
# with you     
# lb016b_a: how often do you speak on the phone
# lb016c_a: Write or email
# lb016d_a: Communicate by Skype, Facebook, or other social media
table(df.mi1$lb014_a, df.mi1$lb016a_a, useNA = "ifany")                                        # <------ Meet up 3             MI (1-6)                 
table(df.mi1$lb014_a, df.mi1$lb016b_a, useNA = "ifany")                                        # <------ speak on the phone 3  MI  (1-6)
table(df.mi1$lb014_a, df.mi1$lb016c_a, useNA = "ifany")                                        # <------ Write or email 3     MI (1-6)
table(df.mi1$lb014_a, df.mi1$lb016d_a, useNA = "ifany")                                        # <------ Communicate  3        NI (1-6)

# lb017_a: How many of your Friends would you say you have a close relationship with??                         
df.mi1$lb017_a[df.mi1$lb017_a == -2] <- 0  
table(df.mi1$lb014_a, df.mi1$lb017_a, useNA = "ifany")                                        # <------ close ties 3 OLS (continuous)

df.mi1 <- df.mi1 %>%
  mutate(across(c(lb016a_a, lb016b_a, lb016c_a, lb016d_a, lb017_a),
                ~if_else(lb014_a == 0 & is.na(.), 0, .)))
### V. Reverse coding 
lb1 <- c("lb004a_a", "lb004b_a", "lb004c_a", "lb004d_a", "lb004e_a", "lb004f_a", "lb004g_a",
         "lb005a_a", 
         "lb007a_a", "lb007b_a", "lb007c_a", "lb007d_a", "lb007e_a", "lb007f_a", "lb007g_a",
         "lb011a_a", "lb011b_a", "lb011c_a", "lb011d_a", "lb011e_a", "lb011f_a", "lb011g_a",
         "lb015a_a", "lb015b_a", "lb015c_a", "lb015d_a", "lb015e_a", "lb015f_a", "lb015g_a")
df.mi1 <- df.mi1 %>%
  mutate(across(all_of(lb1), ~ case_when(
    . == 1 ~ 3,
    . == 2 ~ 2,
    . == 3 ~ 1,
    . == 4 ~ 0,
    TRUE ~ as.numeric(.)
  )))

lb2 <- c("lb008a_a", "lb008b_a", "lb008c_a", "lb008d_a",                                   
         "lb012a_a", "lb012b_a", "lb012c_a", "lb012d_a",                                   
         "lb016a_a", "lb016b_a", "lb016c_a", "lb016d_a")
df.mi1 <- df.mi1 %>%
  mutate(across(all_of(lb2), ~ case_when(
    . == 1 ~ 5,
    . == 2 ~ 4,
    . == 3 ~ 3,
    . == 4 ~ 2,
    . == 5 ~ 1,
    . == 6 ~ 0,
    TRUE ~ as.numeric(.)
  )))


df.mi1 <- df.mi1 %>%
  mutate(lb005b_a = case_when(
    lb005b_a == 3 ~ 0,
    lb005b_a == 2 ~ 1,
    lb005b_a == 1 ~ 2,
    TRUE ~ as.numeric(NA) 
  ))


df.mi1 <- df.mi1 %>%
  mutate(across(c(lb005_a, lb005a_a, lb005b_a),
                ~ifelse((r13mstat %in% c(5, 7, 8) | r14mstat %in% c(5, 7, 8) & is.na(.)), 0, .)))

pMiss <- sapply(df.mi1, function(x) sum(is.na(x)) / length(x) * 100)
sum(is.na(df.mi1$lb005b_a))
sapply(df.mi1, function(x) sum(is.na(x)))





####
###                              Step 1 Variable recoding
df.mi1$care <- factor(df.mi1$care)
table(df.mi1$care, useNA = "ifany")




# marriage
df.mi1 <- df.mi1 %>%
  mutate(r13marcat = case_when(
    r13mstat %in% c(1, 2, 3) ~ "Mar/Par",
    r13mstat %in% c(4, 5, 6) ~ "Div/Sep",
    r13mstat == 7 ~ "Widow",
    r13mstat == 8 ~ "NevMar"
  )) %>% 
  mutate(married = ifelse(r13marcat == "Mar/Par", 1, 0),
         sepdiv = ifelse(r13marcat == "Div/Sep", 1, 0),
         widow = ifelse(r13marcat == "Widow", 1, 0),
         nevermarr = ifelse(r13marcat == "NevMar", 1, 0)) 

# race
levels(df.mi1$raceth) <-  c("NH White", "NH Black", "Hispanic", "NH Other")

df.mi1$white <- factor(ifelse(df.mi1$raceth == 'NH White', 1, 0), levels = c(0, 1))  
df.mi1$black <- factor(ifelse(df.mi1$raceth == 'NH Black', 1, 0), levels = c(0, 1))  
df.mi1$hispanic <- factor(ifelse(df.mi1$raceth == 'Hispanic', 1, 0), levels = c(0, 1)) 
df.mi1$other    <- factor(ifelse(df.mi1$raceth == 'NH Other', 1, 0), levels = c(0, 1)) 

# gender
df.mi1$women <- ifelse(df.mi1$women == "Women", 1, 0) # 2835

# income
df.mi1 <- df.mi1 %>%
  mutate(log_h13itot  = asinh(h13itot))


allCog2020 <- read_dta("allCog2020.dta")

df.mi1 <- merge(df.mi1, allCog2020, by = "rahhidpn", all.x = TRUE) # 4750




###
###                         Step 2: cognition


# Visualize missingness pattern
 vis_miss(df.mi1)
# gg_miss_upset(df.mi1)
# 
# \\table(df.mi1$lb005b_a, df.mi1$r15cogtotp,  useNA = "ifany") 
# 
# 
# table(df.mi1$women, df.mi1$r15cogtotp, useNA = "ifany")
# table(df.mi1$white, df.mi1$r15cogtotp,  useNA = "ifany")
# 
# d
# table(df.mi1$white, df.mi1$r15cogtotp,  useNA = "ifany")
# 
# 
# table(df.mi1$r15cesd, df.mi1$r15cogtotp,  useNA = "ifany")
#     # lower cesd more NA
# table(df.mi1$r15stroke, df.mi1$r15cogtotp,  useNA = "ifany")  
#     #  646 missing entries for participants who did not report a stroke 
# table(df.mi1$r15cancre, df.mi1$r15cogtotp,  useNA = "ifany")  
# table(df.mi1$r15diabe, df.mi1$r15cogtotp,  useNA = "ifany")  
# table(df.mi1$r14cogtotp, df.mi1$r15cogtotp,  useNA = "ifany")  
# table(df.mi1$married, df.mi1$r15cogtotp,  useNA = "ifany") 
# 
# 
# table(df.mi1$lb004a_a, df.mi1$r15cogtotp,  useNA = "ifany") 
# table(df.mi1$lb004b_a, df.mi1$r15cogtotp,  useNA = "ifany") 


# First, create a new variable indicating missingness in the cognition variable
df.mi1$missing_cognition <- ifelse(is.na(df.mi1$r15cogtotp), 1, 0)
model <- glm(missing_cognition ~ women, data = df.mi1, family = "binomial")
summary(model)
#  women       -0.23616    0.08175  -2.889  0.00387 ** 

model <- glm(missing_cognition ~ care, data = df.mi1, family = "binomial")
# care1       -0.16899    0.08431  -2.004    0.045 *

model <- glm(missing_cognition ~ r15cesd, data = df.mi1, family = "binomial")
# r15cesd     -0.08114    0.02381  -3.408 0.000655 ***

model <- glm(missing_cognition ~ r15stroke, data = df.mi1, family = "binomial")
# r15stroke   -0.46664    0.14742  -3.165  0.00155 ** 

model <- glm(missing_cognition ~ r15cancre, data = df.mi1, family = "binomial")
# p > 0.05

model <- glm(missing_cognition ~ r15diabe, data = df.mi1, family = "binomial")
#  diabetes is associated with a lower likelihood of missing 

model <- glm(missing_cognition ~ r14cogtotp, data = df.mi1, family = "binomial")
# higher cognitive scores in wave 14, more missing cognitive sc ores in wave 15

model <- glm(missing_cognition ~ married, data = df.mi1, family = "binomial")
# p > 0.05


model <- glm(missing_cognition ~ schlyrs, data = df.mi1, family = "binomial")
# higher edu more 

model <- glm(missing_cognition ~ race,data = df.mi1, family = "binomial")

model <- glm(missing_cognition ~ r13marcat,
             data = df.mi1, family = "binomial")
summary(model)
# widowed individuals more likely to have missing 
model <- glm(missing_cognition ~ lb004a_a,data = df.mi1, family = "binomial")
model <- glm(missing_cognition ~ lb004b_a,data = df.mi1, family = "binomial")



########################################################################### Section three: MI

### Step 0
#   social support variables creation
df.mi1 <- df.mi1 %>%  arrange(rahhidpn)
pMiss <- sapply(df.mi1, function(x) sum(is.na(x)) / length(x) * 100)

vars <- c("lb004a_a", "lb004b_a", "lb004c_a", 
          "lb007a_a", "lb007b_a", "lb007c_a", 
          "lb011a_a", "lb011b_a", "lb011c_a", 
          "lb015a_a", "lb015b_a", "lb015c_a")


# Create new variables
df.mi1$sp_sup    <- rowMeans(df.mi1[, c("lb004a_a", "lb004b_a", "lb004c_a")], na.rm = TRUE)
df.mi1$child_sup <- rowMeans(df.mi1[, c("lb007a_a", "lb007b_a", "lb007c_a")], na.rm = TRUE)
df.mi1$fam_sup <- rowMeans(df.mi1[, c("lb011a_a", "lb011b_a", "lb011c_a")], na.rm = TRUE)
df.mi1$fri_sup <- rowMeans(df.mi1[, c("lb015a_a", "lb015b_a", "lb015c_a")], na.rm = TRUE)

# composition
composition <- sapply(df.mi1[, c("lb003_a", "lb006_a", "lb010_a", "lb014_a")], as.numeric)

df.mi1$composition <- rowSums(composition, na.rm = TRUE)

# ties
df.mi1$ties <- rowSums(df.mi1[, c("lb009_a", "lb013_a", "lb017_a")], na.rm = TRUE)
            # top-code the close tie at 26
            df.mi1$ties[df.mi1$ties > 26] <- 26
            

            saveRDS(df.mi1,"df.mi1.rds")
            
 df.mi1 <- readRDS("df.mi1.rds")
            
           names(df.mi1) 
           # education
           df.mi1$schlyrs[df.mi1$schlyrs == 99] <- NA
           
           
           df.mi1 <- df.mi1 %>%
             mutate(
               edu = case_when(
                 schlyrs <= 11 ~ "Less than High School",
                 schlyrs == 12 ~ "High School Graduate",
                 schlyrs >= 13 & schlyrs <= 15 ~ "Some College",
                 schlyrs >= 16 ~ "College Degree",
                 TRUE ~ NA_character_  
               ),
               edu = factor(edu, levels = c("Less than High School", "High School Graduate", 
                                            "Some College", "College Degree"))
             ) %>%
             # Create dummy variables for each education category
             mutate(
               lshs = if_else(edu == "Less than High School", 1, 0),
               hs = if_else(edu == "High School Graduate", 1, 0),
               somecoll = if_else(edu == "Some College", 1, 0),
               coll = if_else(edu == "College Degree", 1, 0)
             )
           
     
            

### Step 1. 
#   Select variable for imputation
           df.mi2a <- dplyr::select(df.mi1, rahhidpn, care, raestrat, raehsamp, r13wtresp, 
                                    women,  white, black, hispanic, other, married, sepdiv, widow, nevermarr, 
                                    schlyrs, raevbrn, study, raestrat,
                                    r13cogtot, r13age, birthyr, log_h13itot, r13hibpe, r13diabe,
                                    r13cancre, r13hearte, r13stroke, r13cesd, h13child,
                                    r13livsib, r13momliv, r13dadliv,
                                    r14hibpe, r14diabe, r14cancre, r14hearte, r14stroke, 
                                    r14cesd, h14child, r14livsib, r14cogtotp, r14momliv, r14dadliv,
                                    r15cogtotp, r15age, r15hibpe, r15diabe, r15cancre,
                                    r15hearte, r15stroke, r15cesd, h15child, r15livsib,
                                    lb004d_a, lb004e_a, lb004f_a, lb004g_a, 
                                    lb005_a, lb005a_a, lb005b_a, 
                                    lb007d_a, lb007e_a, lb007f_a, lb007g_a, 
                                    lb008a_a, lb008b_a, lb008c_a, lb008d_a,
                                    lb011d_a, lb011e_a, lb011f_a, lb011g_a, 
                                    lb012a_a, lb012b_a, lb012c_a, lb012d_a, 
                                    lb013a_a, 
                                    lb015d_a, lb015e_a, lb015f_a, lb015g_a, 
                                    lb016a_a, lb016b_a, lb016c_a, lb016d_a, 
                                    ties, composition, sp_sup, child_sup, fam_sup, fri_sup,
                                    r15imrcp, r15dlrcp, r15tr20p, 
                                    r15ser7p,  r15bwc20p, r15mop,
                                    r15scisp, r15cactp, r15presp, r15mstotp,
                                    r15cog27, r15slfmem, r15pstmem)
            

            df.mi2a <- df.mi2a %>%
              mutate(across(where(is.numeric), as.numeric))
     
            numMiss <- sum(rowSums(is.na(df.mi2a)) > 0)
            pMiss <- (numMiss / nrow(df.mi2a)) * 100 #55.6% of the rows contain at least one missing value.
       
            sapply(df.mi2a, function(x) sum(is.na(x)))
      
            
### Step 2: Create a Missing Data Indicator Matrix
missFlag <- is.na(df.mi2a)

### Step 3: Flag Variables for Imputation
# Flag variables with any missing data
varImp <- names(df.mi2a)[colSums(missFlag) > 0]

### Step 4: Imputation with Survey Weight Consideration -
wt <- svydesign(
   id = ~raehsamp,
   weights = ~r13wtresp,
   strata = ~raestrat,
   data = df.mi2a,
   nest = TRUE
 )

 pred <- quickpred(df.mi2a, exclude = c("rahhidpn"))
 pred["r13wtresp", ] <- 0


### Step 5: Run the imputation
binary_vars <- c("r13momliv", "r13dadliv", "r14momliv", "r14dadliv")
df.mi2a <- df.mi2a %>%
  mutate(across(all_of(binary_vars), as.factor))

method_list <- sapply(df.mi2a, function(x) {
  if(is.factor(x)) "polyreg" else "pmm"
})

imputed10 <- mice(df.mi2a, method = method_list, m = 10, seed = 123, maxit = 10, predictorMatrix = pred)
imputed20 <- mice(df.mi2a, method = method_list, m = 20, seed = 123, maxit = 20, predictorMatrix = pred)


saveRDS(imputed10,"imputed10.rds")
saveRDS(imputed20,"imputed20.rds")

imputed_data_1 <- complete(imputed10, 9)
names(imputed_data_1)


imputed10 <- readRDS("imputed10.rds")







