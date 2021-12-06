######### Setup ##########
options(scipen = 0)

# set working directory
setwd("/Users/joebahr/sanford/pubpol813/pubpol813_replication_project")

# load packages
library(haven)
library(tidyverse)
library(config)
library(purrr)
library(magrittr)
library(scales)
library(fixest)

# set year of policy
policy_year <- 2012

#########################
######### READ ##########
#########################

# read in data
acs_over24_df <- haven::read_stata("./ACS_PPS813_F2021_revised.dta")
acs_under24_df <- haven::read_stata("./ACS_PPS813_F2021_15to24.dta")

# append data
acs_df <- rbind.data.frame(acs_over24_df, acs_under24_df)

#######################################
############### PREP ##################
#######################################

## DACA ELIGIBILITY
# 1) no lawful status as of June 15, 2012
# 2) applicants came to US before age 16
# 3) under the age of 31 as of June 2012
# 4) continuously resided in US since June 2007
# 5) high school diploma or GED
# 6) no felony
elig_df <- acs_df %>% 
  filter(educ >= 6) %>% # filter to only high school grads and up )
  mutate(qob_age_adjustment = case_when(birthqtr <= 2 ~ 0,
                                        birthqtr > 2 ~ 1,
                                        TRUE ~ -9),
         age_june_2012 = policy_year - birthyr - qob_age_adjustment,  # subtract 1 if born after June
         age_june_2012_elig = ifelse(age_june_2012 < 31, 1, 0),
         age_of_entry = age - yrsusa1,
         age_of_entry_elig = ifelse(age_of_entry < 16,1,0),
         cont_residence_elig = ifelse(yrimmig <= 2007, 1, 0),  # write in paper how the numbers changed insignificantly
         non_citizen = ifelse(citizen == 3, 1, 0),
         eligible = age_june_2012_elig * age_of_entry_elig * cont_residence_elig * non_citizen,
         eligibility_groups = case_when(citizen != 3 ~ 'citizen',
                                        citizen == 3 & eligible == 1 ~ 'noncitizen_daca',
                                        citizen == 3 & eligible == 0 ~ 'noncitizen_nondaca')
  )

rm(acs_df, acs_over24_df, acs_under24_df)

# Outcomes
# 1) worked in last week (binary) - wrklstwk == 2, omit Nulls when calculating means
# 2) worked in last year (binary) - workedyr == 3
# 3) usual number of hours worked each week - uhrswork
# 4) labor force participation (binary) - labforce == 2
# 5) unemployed or not (binary) - empstat == 2
# 6) self-employed or not (binary) - classwkr == 1
# 7) income - all sources in past twelve months - inctot
# 8) individual is attending school (binary) - school == 2
# 9) individual has obtained a GED (binary) - educd %in% c(62,64)

# Demographics
# 1) Years in US (yrsusa1)
# 2) Age Entered US (age_of_entry)
# 3) Male (sex == 1)
# 4) White (race == 1)
# 5) Black (race == 2)
# 6) Asian (race %in% c(4,5,6))
# 7) Hispanic ethnicity (hispan %in% c(1,2,3,4)) - omit since all hispanic
# 8) Home language of Spanish (language == 12)
# 9) Born in Latin America (bpl between 200 and 300)
# 10) Age (age)
# 11) Married (marst %in% c(1,2))
# 12) Live in a metro area (metro %in% c(2,3,4))
# 13) High School Degree (educd between 62 and 64)
# 14) Some college (educd between 65 and 100)
# 15) College degree (educd >= 101)
# 16) Count

outcome_df <- elig_df %>% 
  group_by(year) %>% 
  mutate(inc_perc = percent_rank(x = inctot)) %>% 
  ungroup() %>% 
  mutate(outcome_worked_last_week = case_when(wrklstwk == 2 ~ 1,
                                              wrklstwk == 3 ~ -9,
                                              TRUE ~ 0), # omit if Not Reported
         outcome_worked_last_year = ifelse(workedyr == 3, 1, 0),
         outcome_hours_worked = uhrswork,
         outcome_labor_force = ifelse(labforce == 2, 1, 0), # good
         outcome_unemployed = ifelse(empstat == 2, 1, 0),   # good
         outcome_self_employed = ifelse(classwkr == 1, 1, 0), # good
         outcome_income = ifelse(inctot < 0, 0, inctot),
         outcome_income_sub90 = ifelse(inc_perc < .9, outcome_income, -9),
         outcome_school_attendance = ifelse(school == 2, 1, 0), # good
         outcome_ged = case_when(educd == 64 ~ 1,
                                 educd == 62 ~ -9,
                                 TRUE ~ 0),
         outcome_log_income = log(outcome_income + 1),
         outcome_poverty = case_when(poverty == 0 ~ -9,
                                     poverty < 100 ~ 1,
                                     TRUE ~ 0),
         outcome_home_ownership = case_when(ownershp == 1 ~ 1,
                                            ownershp == 0 ~ -9,
                                            TRUE ~ 0),
         outcome_healthcare = case_when(hcovany == 2 ~ 1,
                                        is.na(hcovany) ~ -9,
                                        TRUE ~ 0),
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

rm(elig_df)

############################
######### Table 1 ##########
############################

# create function to calculate means and estimate a t-statistic
# for null hypothesis that difference in means is equal to 0
calc_table1 <- function(df, .x, control = "noncitizen_nondaca"){
  
  # filter to daca eligible and only select outcome of interest
  daca <- df %>% filter(eligibility_groups == "noncitizen_daca" & # filter to daca eligible
                          !!rlang::sym(.x) != -9 &                  # filter out null observations
                          year <= 2014) %>% select({{.x}})          # reduce sample to 2014 and less
  
  # create vector of outcome variable for daca eligible
  daca_outcome <- daca[[.x]]
  
  # filter on daca not elibile or citizens and select outcome of interest
  control <- df %>% filter(eligibility_groups == control &
                             !!rlang::sym(.x) != -9 &
                             year <= 2014) %>% select({{.x}})
  
  # create vector of outcome variable for control (not daca eligible or citizen)
  control_outcome <- control[[.x]]
  
  # calculate means, diff in means, and t-statistic on diff of means
  daca_mean <- mean(daca_outcome)
  control_mean <- mean(control_outcome)
  difference <- daca_mean - control_mean
  t_stat <- t.test(daca_outcome, control_outcome, mu = 0, paired = FALSE)$statistic
  
  # return data frame with key variables
  return(data.frame(outcome_variable = .x, daca_mean = daca_mean, control_mean, 
                    difference, t_stat))
}

# create vector of variables used for table 1 using prefixes
outcome_vars <- outcome_df %>% select(starts_with("outcome_")) %>% names
demo_vars <- outcome_df %>% select(starts_with("demo_")) %>% names

# combine the two into a single vector to populate table 1
table1_vars <- c(outcome_vars, demo_vars)


# compare to non-citizen, daca ineligible
table1_nondaca <- purrr::map_dfr(.x = table1_vars, .f = calc_table1, 
                                 df = outcome_df %>% filter(between(age, 18, 35)), 
                                 control = "noncitizen_nondaca")

# compare to citizens
table1_citizen <- purrr::map_dfr(.x = table1_vars, .f = calc_table1, 
                                 df = outcome_df %>% filter(between(age, 18, 35)), 
                                 control = "citizen") %>% 
  select(-daca_mean)

# join the two together for table 1
combined_table1 <- table1_nondaca %>% inner_join(table1_citizen, by = "outcome_variable",
                                                 suffix = c(".nondaca", ".citizen"))

##############################################
################ FIGURES 2-5 #################
##############################################

### NEED TO ADD 90th percentile graph

# create distinct list of years up to 2014
fig_25_years <- outcome_df %>% filter(year <= 2014) %>% 
  select(year) %>% distinct()

# create function to calculate yearly differences in outcomes
calc_fig25 <- function(df, fig_25_years, .x, control = "noncitizen_nondaca"){
  
  # print name of outcome variable for debugging purposes
  print(.x)
  
  # create if statement for years since healthcare question only arrived in 2008
  years <- if (.x == "outcome_healthcare") {
    fig_25_years %>% filter(between(year, 2008, 2014))
  } else {
    fig_25_years 
  } 
  
  # create empty dataframe that will be appended to in a loop
  output_df <- data.frame(matrix(ncol = 6, nrow = 0))
  
  #provide column names for empty dataframe
  colnames(output_df) <- c('year', 'outcome_variable', 'daca_mean', 'control_mean',
                           'difference', 'ci')
  
  # iterate over every year value
  for (i in years[["year"]]){
    
    # filter to year of current iteration
    year_df <- df %>% filter(year == i)
    
    # filter to daca eligible individuals and filter out Null observations (for that variable)
    daca_df <- year_df %>% filter(eligibility_groups == "noncitizen_daca" & 
                                    !!rlang::sym(.x) != -9) %>% 
      select({{.x}})
    
    # create vector of outcome for daca eligible    
    daca_outcome <- daca_df[[.x]]
    
    # filter to comparison group and filter out Nulls
    control_df <- year_df %>% filter(eligibility_groups == control &
                                       !!rlang::sym(.x) != -9) %>% 
      select({{.x}})
    
    # create vector of comparison group outcome
    control_outcome <- control_df[[.x]]
    
    # estimate means, difference in means, and t_test statistics
    daca_mean <- mean(daca_outcome)
    control_mean <- mean(control_outcome)
    difference <- daca_mean - control_mean
    t_test <- t.test(daca_outcome, control_outcome, mu = 0, paired = FALSE)
    
    # extract standard error and create confidence interval bounds
    stderr <- t_test$stderr
    ci <- 1.96*stderr
    
    # create dataframe of relevant output
    year_output <- data.frame(year = i, outcome_variable = .x, daca_mean = daca_mean, control_mean, 
                              difference, ci)
    
    # append to existing output
    output_df <- rbind.data.frame(output_df, year_output)
    
  }
  
  return(output_df)
  
}

# run calc_fig25 across all outcome variables
fig_25_df <- map_dfr(.x = outcome_vars, .f = calc_fig25, 
                     df = outcome_df %>% filter(between(age, 18, 35)), 
                     fig_25_years = fig_25_years,
                     control = "noncitizen_nondaca")


# create df for estimating d-in-d
# filter to only non-citizens and pre-2014
did_df = outcome_df %>% filter(citizen == 3 & 
                               year <= 2014 &
                               between(age, 18, 35)) %>% 
  select(starts_with("outcome_"), after_daca, eligible, did_term)



# create function to estimate DiD w/o controls
calc_did_nocontrols <- function(df, outcome){
  
  df <- df %>% filter(!!rlang::sym(outcome) != -9)
  
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
  
  print(outcome)
  
  did_caption <- calc_did_nocontrols(df = did_df, outcome = outcome)
  
  fig_df <- fig_25_df %>% filter(outcome_variable == outcome)
  
  title_name <- outcome %>% str_replace("outcome_", "") %>% 
    str_replace_all("_", " ") %>% str_to_title()
  
  label_height <- max(fig_df[["difference"]])
  
  ggplot(fig_df %>% filter(outcome_variable == outcome), aes(x=year, y=difference)) + 
    geom_rect(aes(xmin=2012, xmax=2013, ymin=-Inf, ymax=Inf), alpha = .08) +
    geom_errorbar(aes(ymin=difference-ci, ymax=difference+ci), width=.1) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(title = title_name,
         x = "Year",
         y = "Difference") +
    annotate(geom="text", x=2009, y=label_height, label=did_caption) +
    scale_x_continuous(breaks= pretty_breaks()) 
  
  
}

# execute all plots
map(.x = outcome_vars, .f = plot_fig25, fig_25_df = fig_25_df, did_df = did_df)


####################################
############## MODEL ###############
####################################

## NOTE: NEED TO ADD CLUSTERED STANDARD ERRORS ##

# create state/year unemployment level variable
state_unemployed <- outcome_df %>% 
  filter(outcome_unemployed != -9) %>% 
  group_by(year, statefip) %>% 
  summarise(state_unemployment = mean(outcome_unemployed))

# join on state/year unemployment and select columns of interest
# filter to only non-citizens
model_df <- outcome_df %>% 
  left_join(state_unemployed, by = c("year", "statefip")) %>% 
  select(starts_with("outcome_"), starts_with("demo_"), eligible, after_daca, 
         did_term, state_unemployment, age, age_of_entry, year, statefip,
         age_june_2012, citizen)

rm(outcome_df)


# create generic fucntion for estimating model given:
# 1) A sample, filtering to relevant panel of individuals
# 2) Outcome variable, ensuring we are filtering out Nulls for that particular outcome
# 3) Final year - which year to end the analysis in (2014 or 2019 for this particular paper)
get_coefs <- function(model_df, outcome, final_year){
  
  print(outcome)
  start <- Sys.time()
  
  # dynamically create formula given outcome of interest
  formula <- sprintf("%s ~ did_term + eligible + after_daca + demo_some_college + 
                      demo_college + demo_male  + demo_black + demo_asian + demo_married + 
                      state_unemployment | demo_age + age_of_entry + year +
                      statefip + year^statefip", outcome) %>% as.formula()
  
  # estimate fixed effects model, clustering standard errors by statefip^year
  model <- fixest::feols(data = model_df %>% 
                           filter(year <= final_year & 
                                  !!rlang::sym(outcome) != -9),
              cluster = c("statefip^year"),
              collin.tol = 0.0000000000000000000000000000000000000000000000001,
              fml = formula)
  
  model_summary <- model %>% summary()
  
  # extract relevant coefficients and values
  did_coef <- model_summary$coefficients[["did_term"]]
  did_se <- model_summary$se[["did_term"]]
  did_t <- model_summary$coeftable[1,3]
  did_p <- model_summary$coeftable[1,4]
  
  elig_coef <- model_summary$coefficients[["eligible"]]
  elig_se <- model_summary$se[["eligible"]]
  elig_t <- model_summary$coeftable[2,3]
  elig_p <- model_summary$coeftable[2,4]
  
  # extract number of observations and rsquared
  n <- model_summary$nobs
  rsq <- fixest::r2(x = model, type = "r2", full_names = FALSE)
  
  # still need to round this out to include eligible coefficient
  output_df <- data.frame(outcome_variable = outcome, did_coef = did_coef,
                          did_se = did_se, did_t = did_t, did_p = did_p,
                          elig_coef = elig_coef, elig_se = elig_se,
                          elig_t = elig_t, elig_p = elig_p,
                          n = n, rsquared = rsq)
  
  end <- Sys.time()
  print(end-start)
  return(output_df)
}


##############################################
# PANEL A - ENTERED US BETWEEN AGES 12 AND 19
##############################################

panel_a_df <- model_df %>% 
  filter(citizen == 3 &
           between(age_of_entry, 12, 19))

# run model against panel a sample up to 2014
panel_a_2014_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_a_df, final_year = 2014)

# run model against panel a sample up to 2019
panel_a_2019_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_a_df, final_year = 2019)

rm(panel_a_df)

#####################################################################
# PANEL B - AGES 27 TO 34 IN JUNE 2012 AND ENTERED US BEFORE AGE 16
#####################################################################

# filter to non-citizens, age 27-34 as of June 2012, entered US before age 16
## NEED TO FIX: believe the 18-35 filter is bad for this cut (i.e. if 41 year old in 2019 is excluded)
panel_b_df <- model_df %>% 
  filter(citizen == 3 &
           between(age_june_2012, 27, 34) &
           age_of_entry < 16)

# run model on Panel B sample up to 2014
panel_b_2014_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_b_df, final_year = 2014)

# run model on Panel B sample up to 2019
panel_b_2019_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_b_df, final_year = 2019)

rm(panel_b_df)

########################################
# PANEL C - ALL NON-CITIZENS AGES 18-35
########################################

panel_c_df <- model_df %>% 
  filter(citizen == 3 & 
         between(age, 18, 35))

# run model on Panel C sample up to 2014
panel_c_2014_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_c_df, final_year = 2014)

# run model on Panel C sample up to 2019
panel_c_2019_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_c_df, final_year = 2019)

rm(panel_c_df)

######################################################
# PANEL D -  ALL CITIZENS AND NON-CITIZENS AGES 18-35
######################################################

panel_d_df <- model_df %>% filter(between(age, 18, 35))

# run model on Panel D up to 2014
panel_d_2014_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_d_df, final_year = 2014)

# run model on Panel D up to 2019
panel_d_2019_coefs <- map_dfr(.x = outcome_vars, .f = get_coefs, 
                              model_df = panel_d_df, final_year = 2019)

rm(panel_d_df)

# bind 2014 results into a single dataframe
results_2014 <- panel_a_2014_coefs %>% mutate(panel = "panel_a") %>% 
  rbind.data.frame(panel_b_2014_coefs %>% mutate(panel = "panel_b")) %>% 
  rbind.data.frame(panel_c_2014_coefs %>% mutate(panel = "panel_c")) %>%
  rbind.data.frame(panel_d_2014_coefs %>% mutate(panel = "panel_d"))

# bind 2019 results into a single dataframe
results_2019 <- panel_a_2019_coefs %>% mutate(panel = "panel_a") %>% 
  rbind.data.frame(panel_b_2019_coefs %>% mutate(panel = "panel_b")) %>% 
  rbind.data.frame(panel_c_2019_coefs %>% mutate(panel = "panel_c")) %>%
  rbind.data.frame(panel_d_2019_coefs %>% mutate(panel = "panel_d"))

# write results
results_2014 %>% write_csv("./output/regression_results_2014.csv")
results_2019 %>% write_csv("./output/regression_results_2019.csv")

# write table 1
combined_table1 %>% write_csv("./output/table1.csv")

# write fig25 data
fig_25_df %>% write_csv("./output/figs2to5.csv")

rm(list = ls())
