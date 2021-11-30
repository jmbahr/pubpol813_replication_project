# set working directory
setwd("/Users/joebahr/sanford/pubpol813/pubpol813_replication_project")

# load packages
library(haven)
library(tidyverse)
library(config)
library(purrr)
library(magrittr)

# import config
config <- config::get()
policy_year <- config$policy_year

# read in data
acs_over24_df <- haven::read_stata("./ACS_PPS813_F2021_revised.dta")
acs_under24_df <- haven::read_stata("./ACS_PPS813_F2021_15to24.dta")

# append data
acs_df <- rbind.data.frame(acs_over24_df, acs_under24_df)

## DACA ELIGIBILITY
# 1) no lawful status as of June 15, 2012
# 2) applicants came to US before age 16
# 3) under the age of 31 as of June 2012
# 4) continuously resided in US since June 2007
# 5) high school diploma or GED
# 6) no felony

elig_df = acs_df %>% 
  filter(educ >= 6) %>% # filter to only high school grads and up / believe this is already done
  mutate(qob_age_adjustment = case_when(birthqtr <= 2 ~ 0,
                                        birthqtr > 2 ~ 1,
                                        TRUE ~ -9),
         age_june_2012 = policy_year - birthyr - qob_age_adjustment,  # subtract 1 if born after June
         age_june_2012_elig = ifelse(age_june_2012 < 31, 1, 0),
         age_of_entry = age - yrsusa1,
         age_of_entry_elig = ifelse(age_of_entry < 16,1,0),
         cont_residence_elig = ifelse(yrimmig < 2007, 1, 0),
         eligible = age_june_2012_elig * age_of_entry_elig * cont_residence_elig,
         eligibility_groups = case_when(citizen != 3 ~ 'citizen',
                                        citizen == 3 & eligible == 1 ~ 'noncitizen_daca',
                                        citizen == 3 & eligible == 0 ~ 'noncitizen_nondaca')
         )

# Outcomes
# 1) worked in last week (binary) - wrklstwk == 2
# 2) worked in last year (binary) - workedyr == 3
# 3) usual number of hours worked each week - uhrswork
# 4) labor force participation (binary) - labforce == 2
# 5) unemployed or not (binary) - empstat == 2
# 6) self-employed or not (binary) - classwkr == 1
# 7) income - all sources in past twelve months - inctot
# 8) individual is attending school (binary) - school == 2
# 9) individual has obtained a GED (binary) - educd == 64

# Demographics
# 1) Years in US (yrsusa1)
# 2) Age Entered US (age_of_entry)
# 3) Male (sex == 1)
# 4) White (race == 1)
# 5) Black (race == 2)
# 6) Asian (race %in% c(4,5,6))
# 7) Hispanic ethnicity (hispan %in% c(1,2,3,4))
# 8) Home language of Spanish (language == 12)
# 9) Born in Latin America (bpl between 200 and 300)
# 10) Age (age)
# 11) Married (marst %in% c(1,2))
# 12) Live in a metro area (metro %in% c(2,3,4))
# 13) High School Degree (educ >= 6)
# 14) Some college (educ >= 7)
# 15) College degree (educd >= 101)
# 16) Count

outcome_df <- elig_df %>% 
  mutate(outcome_worked_last_week = ifelse(wrklstwk == 2, 1, 0),
         outcome_worked_last_year = ifelse(workedyr == 3, 1, 0),
         outcome_hours_worked = uhrswork,
         outcome_labor_force = ifelse(labforce == 2, 1, 0),
         outcome_unemployed = ifelse(empstat == 2, 1, 0),
         outcome_self_employed = ifelse(classwkr == 1, 1, 0),
         outcome_income = inctot,
         outcome_school_attendance = ifelse(school == 2, 1, 0),
         outcome_ged = ifelse(educd == 64, 1, 0),
         demo_yrsusa = yrsusa1,
         demo_age_of_entry = age_of_entry,
         demo_male = ifelse(sex == 1, 1, 0),
         demo_white = ifelse(race == 1, 1, 0),
         demo_black = ifelse(race == 2, 1, 0),
         demo_asian = ifelse(race %in% c(4,5,6), 1, 0),
         demo_home_spanish = ifelse(language == 12, 1, 0),
         demo_latin_america = ifelse(between(bpl, 200, 300), 1, 0),
         demo_age = age,
         demo_married = ifelse(marst %in% c(1,2), 1, 0),
         demo_metro = ifelse(metro %in% c(2,3,4), 1, 0),
         demo_some_college = ifelse(educ >= 7, 1, 0),
         demo_college = ifelse(educd >= 101, 1, 0)
         ) 


# create function to calculate means and estimate a t-statistic
# for null hypothesis that difference in means is equal to 0
calc_table1 <- function(df, .x, control = "noncitizen_nondaca"){
  
  daca <- df %>% filter(eligibility_groups == "noncitizen_daca") %>% select({{.x}})
  daca_outcome <- daca[[.x]]
  
  control <- df %>% filter(eligibility_groups == control) %>% select({{.x}})
  control_outcome <- control[[.x]]
  
  daca_mean <- mean(daca_outcome)
  control_mean <- mean(control_outcome)
  difference <- daca_mean - control_mean
  t_stat <- t.test(daca_outcome, control_outcome, mu = 0, paired = FALSE)$statistic
  
  return(data.frame(outcome_variable = .x, daca_mean = daca_mean, control_mean, 
                    difference, t_stat))
}

# create vector of variables used for table 1 using prefixes
outcome_vars <- outcome_df %>% select(starts_with("outcome_")) %>% names
demo_vars <- outcome_df %>% select(starts_with("demo_")) %>% names
table1_vars <- c(outcome_vars, demo_vars)

# need to filter on ages before calcing table 1

# t-test compared to daca ineligible
table1_nondaca <- map_dfr(.x = table1_vars, .f = calc_table1, 
                          df = outcome_df, control = "noncitizen_nondaca")
  
# t-test compared to citizens
table1_citizen <- map_dfr(.x = table1_vars, .f = calc_table1, 
                          df = outcome_df, control = "citizen") %>% 
  select(-daca_mean)

# join the two together for table 1
combined_table1 <- table1_nondaca %>% inner_join(table1_citizen, by = "outcome_variable",
                                                 suffix = c(".nondaca", ".citizen"))
