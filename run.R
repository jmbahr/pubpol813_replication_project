setwd("/Users/joebahr/sanford/pubpol813/replication_project")

library(haven)
library(tidyverse)
library(config)
library(purrr)
library(magrittr)

config <- config::get()
policy_year <- config$policy_year
acs_df <- haven::read_stata("./ACS_PPS813_F2021_revised.dta")

## DACA ELIGIBILITY
# 1) no lawful status as of June 15, 2012 xxx
# 2) applicants came to US before age 16   xxx
# 3) under the age of 31 as of June 2012  xxx
# 4) continuously resided in US since June 2007  xxx ?? just at lease five years?
# 5) high school diploma or GED   xxx
# 6) no felony xxx

# account for year prior eligibility by adding 1 to different criteria
# set to zero if we don't do this
yrprior = 1

elig_df = acs_df %>% 
  filter(educ >= 6) %>% # filter to only high school grads and up / believe this is already done
  mutate(qob_age_adjustment = case_when(birthqtr <= 2 ~ 0,
                                        birthqtr > 2 ~ 1,
                                        TRUE ~ -9),
         age_june_2012 = policy_year - birthyr - qob_age_adjustment,  # subtract 1 if born after June
         age_june_2012_elig = ifelse(age_june_2012 < 31, 1, 0),
         age_of_entry = age - yrsusa1,
         age_of_entry_elig = ifelse(age_of_entry < 16,1,0),
         years_in_us_elig = ifelse(yrsusa1 >= 5 + yrprior, 1, 0),
         cont_residence_elig = ifelse(yrimmig <= 2007, 1, 0),
         eligible = age_june_2012_elig * age_of_entry_elig * years_in_us_elig,
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

table1 <- elig_df %>% 
  mutate(outcome_worked_last_week = ifelse(wrklstwk == 2, 1, 0),
         outcome_worked_last_year = ifelse(workedyr == 3, 1, 0),
         outcome_hours_worked = uhrswork,
         outcome_labor_force = ifelse(labforce == 2, 1, 0),
         outcome_unemployed = ifelse(empstat == 2, 1, 0),
         outcome_self_employed = ifelse(classwkr == 1, 1, 0),
         outcome_income = inctot,
         outcome_school_attendance = ifelse(school == 2, 1, 0),
         outcome_ged = ifelse(educd == 64, 1, 0)
         ) %>% 
  group_by(eligibility_groups) %>% 
  summarise_at(.vars = vars(starts_with("outcome_")), .funs = mean)



elig_df %>% 
  group_by(years_in_us_elig) %>% count

acs_df %>% 
  group_by(citizen) %>% count
