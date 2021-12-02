######### Setup ##########

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

######### Read ##########

# read in data
acs_over24_df <- haven::read_stata("./ACS_PPS813_F2021_revised.dta")
acs_under24_df <- haven::read_stata("./ACS_PPS813_F2021_15to24.dta")

# append data
acs_df <- rbind.data.frame(acs_over24_df, acs_under24_df)


######### Prep ##########

## DACA ELIGIBILITY
# 1) no lawful status as of June 15, 2012
# 2) applicants came to US before age 16
# 3) under the age of 31 as of June 2012
# 4) continuously resided in US since June 2007
# 5) high school diploma or GED
# 6) no felony
elig_df = acs_df %>% 
  filter(educ >= 6 &       # filter to only high school grads and up / believe this is already done
         between(age, 18, 35)) %>% 
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
  mutate(outcome_worked_last_week = case_when(wrklstwk == 2 ~ 1,
                                              wrklstwk == 3 ~ -9,
                                              TRUE ~ 0), # omit if Not Reported
         outcome_worked_last_year = ifelse(workedyr == 3, 1, 0),
         outcome_hours_worked = uhrswork,
         outcome_labor_force = ifelse(labforce == 2, 1, 0), # good
         outcome_unemployed = ifelse(empstat == 2, 1, 0),   # good
         outcome_self_employed = ifelse(classwkr == 1, 1, 0), # good
         outcome_income = inctot,
         outcome_school_attendance = ifelse(school == 2, 1, 0), # good
         outcome_ged = ifelse(educd %in% c(62,64), 1, 0), # assuming that code 62 is GED
         income_zeroed = ifelse(inctot < 0, 0, inctot),
         outcome_log_income = log(income_zeroed + 1),
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
         demo_highschool = ifelse(between(educd, 62, 64), 1, 0),
         demo_some_college = ifelse(between(educd, 65, 100), 1, 0), # assuming less than bachelors is only some college
         demo_college = ifelse(educd >= 101, 1, 0),
         after_daca = ifelse(year > 2012, 1, 0),
         did_term = after_daca * eligible
         )


######### Table 1 ##########

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

# need to filter on ages and years before creating table 1 figures

# t-test compared to daca ineligible
table1_nondaca <- purrr::map_dfr(.x = table1_vars, .f = calc_table1, 
                                 df = outcome_df %>% filter(between(age, 18, 35)), 
                                 control = "noncitizen_nondaca")
  
# t-test compared to citizens
table1_citizen <- purrr::map_dfr(.x = table1_vars, .f = calc_table1, 
                          df = outcome_df %>% filter(between(age, 18, 35)), 
                          control = "citizen") %>% 
  select(-daca_mean)

# join the two together for table 1
combined_table1 <- table1_nondaca %>% inner_join(table1_citizen, by = "outcome_variable",
                                                 suffix = c(".nondaca", ".citizen"))


######### Figures 2-5 ##########

### NEED TO ADD 90th percentile graph

# create function to calculate yearly differences in outcomes
calc_fig25 <- function(df, .x, control = "noncitizen_nondaca"){

  output_df <- data.frame(matrix(ncol = 6, nrow = 0))
  
  #provide column names
  colnames(output_df) <- c('year', 'outcome_variable', 'daca_mean', 'control_mean',
                           'difference', 'ci')
  
  for (i in years[["year"]]){
    
    year_df <- df %>% filter(year == i)
    
    daca_df <- year_df %>% filter(eligibility_groups == "noncitizen_daca") %>% select({{.x}})
    daca_outcome <- daca_df[[.x]]
    
    control_df <- year_df %>% filter(eligibility_groups == control) %>% select({{.x}})
    control_outcome <- control_df[[.x]]
    
    daca_mean <- mean(daca_outcome)
    control_mean <- mean(control_outcome)
    difference <- daca_mean - control_mean
    t_test <- t.test(daca_outcome, control_outcome, mu = 0, paired = FALSE)
    
    stderr <- t_test$stderr
    ci <- 1.96*stderr
    
    year_output <- data.frame(year = i, outcome_variable = .x, daca_mean = daca_mean, control_mean, 
                              difference, ci)
    
    output_df <- rbind.data.frame(output_df, year_output)
    
  }
  
  return(output_df)
    
}

# run calc_fig25 across all outcome variables
fig_25 <- map_dfr(.x = outcome_vars, .f = calc_fig25, 
                  df = outcome_df %>% filter(between(age, 18, 35)), 
                  control = "noncitizen_nondaca")


# create df for estimating d-in-d
did_df = outcome_df %>% filter(between(age, 18, 35) & citizen == 3)


# create function to estimate DiD w/o controls
calc_did_nocontrols <- function(df, outcome){
  
  formula <- sprintf("%s ~ after_daca + eligible + did_term", outcome) %>% as.formula()
  
  model <- lm(formula = formula,
              data = df)  
  
  model_summary <- summary(model)
  
  coefficient <- model_summary$coefficients[[4,1]]
  stderr <- model_summary$coefficients[[4,2]]
  
  did_caption <- sprintf("DID: %.3f (%.3f)", coefficient, stderr)
  
  return(did_caption)
}

# create function to dynamically plot output of fig25 calcs
plot_fig25 <- function(fig_25_df, did_df, outcome){
  
  did_caption <- calc_did_nocontrols(df = did_df, outcome = outcome)
  
  fig_df <- fig_25_df %>% filter(outcome_variable == outcome)
  
  title_name <- outcome %>% str_replace("outcome_", "") %>% 
    str_replace_all("_", " ") %>% str_to_title()
  
  label_height <- max(fig_df[["difference"]])
  
  ggplot(fig_df %>% filter(outcome_variable == outcome), aes(x=year, y=difference)) + 
    geom_errorbar(aes(ymin=difference-stderr, ymax=difference+stderr), width=.1) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(title = title_name,
         x = "Year",
         y = "Difference") +
    geom_vline(xintercept = 2012) +
    annotate(geom="text", x=2009, y=label_height * .9, label=did_caption)

}

# execute all plots
map(.x = outcome_vars, .f = plot_fig25, fig_25_df = fig_25, did_df = did_df)


# model from page 103

# years of education, sex, race, ethnicity, marital status, state-level unemployment rates
# Wit - fixed effects for age and age when arrive in US
# Time fixed effects
# State fixed effects

