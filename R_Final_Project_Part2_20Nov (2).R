install.packages("tidyverse")
library("tidyverse")

school_dm_final <- read.csv("C:/Users/skadav/Downloads/school_dm_final.csv")

# 27. Create nation-year descriptive variables to check overall trends
national_schools1 <- school_dm_final %>%
  group_by(year) %>%
  summarize(county_enroll_dm=mean(county_enroll_DM, na.rm=TRUE),
            population_dm=mean(population_DM, na.rm=TRUE),
            prop_nonwhite_dm=mean(prop_nonwhite_DM, na.rm=TRUE),
            prop_age1524_dm=mean(prop_age1524_DM, na.rm=TRUE),
            prop_male_dm=mean(prop_male_DM, na.rm=TRUE),
            total_crime_county_dm=mean(total_crime_county_DM, na.rm=TRUE),
            total_crime_rate_old_dm=mean(total_crime_rate_old_DM, na.rm=TRUE),
            violent_crime_county_dm=mean(violent_crime_county_DM, na.rm=TRUE),
            violent_rate_old_dm=mean(violent_rate_old_DM, na.rm=TRUE),
            employment_dm=mean(employment_rate_DM, na.rm=TRUE), 
            percap_income_dm=mean(percap_income_DM, na.rm=TRUE),
            new_tot_crime_rate_dm=mean(new_tot_crime_rate_DM, na.rm=TRUE),
            new_violent_rate_dm=mean(new_violent_rate_DM, na.rm=TRUE),
            employment_rate_dm=mean(employment_rate_DM, na.rm=TRUE))

# Plot the graphs
plot(national_schools1$year, national_schools1$county_enroll_dm, type = "l")
plot(national_schools1$year, national_schools1$population_dm, type = "l")
plot(national_schools1$year, national_schools1$prop_nonwhite_dm, type = "l")
plot(national_schools1$year, national_schools1$prop_age1524_dm, type = "l")
plot(national_schools1$year, national_schools1$prop_male_dm, type = "l")
plot(national_schools1$year, national_schools1$total_crime_county_dm, type = "l")
plot(national_schools1$year, national_schools1$total_crime_rate_old_dm, type = "l")
plot(national_schools1$year, national_schools1$violent_crime_county_dm, type = "l")
plot(national_schools1$year, national_schools1$violent_rate_old_dm, type = "l")
plot(national_schools1$year, national_schools1$employment_dm, type = "l")
plot(national_schools1$year, national_schools1$percap_income_dm, type = "l")
plot(national_schools1$year, national_schools1$new_tot_crime_rate_dm, type = "l")
plot(national_schools1$year, national_schools1$new_violent_rate_dm, type = "l")
plot(national_schools1$year, national_schools1$employment_rate_dm, type = "l")


# 28. Check inter-variable correlations
install.packages("corrr")
library(corrr)

#To check the first half of the variables 
corr1 <- school_dm_final %>%   #Creates a nice, clean correlation dataframe.
  correlate() %>%    # Create correlation data frame (cor_df) 
  focus(county_enroll_DM,population_DM,prop_nonwhite_DM,prop_male_DM,prop_age1524_DM, mirror = TRUE) %>%   
  rearrange() %>%  # rearrange by correlations 
  shave() # Shave off the upper triangle for a clean result

#To check the second half of the variables 
corr2 <- school_dm_final %>%  #Creates a nice, clean correlation dataframe.
  correlate() %>%    # Create correlation data frame (cor_df) 
  focus(county_enroll_DM, violent_rate_old_DM, new_violent_rate_DM, percap_income_DM, employment_rate_DM, mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs' 
  rearrange() %>%  # rearrange by correlations 
  shave() # Shave off the upper triangle for a clean result 


# 29. Regressions for each variable and overall model
model_year <- lm(county_enroll_DM ~ year, data = school_dm_final)
summary(model_year)

model_population <- lm(county_enroll_DM ~ population_DM, data = school_dm_final)
summary(model_population)

model_prop_age1524 <- lm(county_enroll_DM ~ prop_age1524_DM, data = school_dm_final)
summary(model_prop_age1524)

model_prop_nonwhite <- lm(county_enroll_DM ~ prop_nonwhite_DM, data = school_dm_final)
summary(model_prop_nonwhite)

model_prop_male <- lm(county_enroll_DM ~ prop_male_DM, data = school_dm_final)
summary(model_prop_male)

model_total_crime_rate_old <- lm(county_enroll_DM ~ total_crime_rate_old_DM, data = school_dm_final)
summary(model_total_crime_rate_old)

model_new_tot_crime_rate <- lm(county_enroll_DM ~ new_tot_crime_rate_DM, data = school_dm_final)
summary(model_new_tot_crime_rate)

model_violent_rate_old <- lm(county_enroll_DM ~ violent_rate_old_DM, data = school_dm_final)
summary(model_violent_rate_old)

model_new_violent_rate <- lm(county_enroll_DM ~ new_violent_rate_DM, data = school_dm_final)
summary(model_new_violent_rate)

model_percap_income <- lm(county_enroll_DM ~ percap_income_DM, data = school_dm_final)
summary(model_percap_income)

model_employment_rate <- lm(county_enroll_DM ~ employment_DM, data = school_dm_final)
summary(model_employment_rate)

# Include all the predictor variables
overallmodel1 <- lm(county_enroll_DM ~ population_DM + prop_age1524_DM + prop_nonwhite_DM + prop_male_DM +
                     total_crime_rate_old_DM + new_tot_crime_rate_DM +
                     violent_rate_old_DM + new_violent_rate_DM +
                     percap_income_DM + employment_rate_DM + year, data = school_dm_final)
summary(overallmodel1)

overallmodel2 <- lm(county_enroll_DM ~ population_DM + prop_age1524_DM + prop_nonwhite_DM + prop_male_DM +
                      total_crime_rate_old_DM + new_tot_crime_rate_DM + new_violent_rate_DM +
                      year, data = school_dm_final)
summary(overallmodel2)


# Parsimonious or best model
bestmodel <- lm(county_enroll_DM ~ prop_age1524_DM + prop_nonwhite_DM + prop_male_DM +
                  total_crime_rate_old_DM + new_violent_rate_DM +
                  percap_income_DM + employment_rate_DM + year, data = school_dm_final)
summary(bestmodel)


# 30. Explanation of the regression results in the work document

