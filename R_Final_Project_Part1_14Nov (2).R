## R Final Project Part 1 Code:


# 1. Install all the packages and read-in the libraries
install.packages("tidyverse")
install.packages("panelr")
install.packages("readxl")
install.packages("haven")

library("tidyverse")
library("panelr")
library("readxl")
library("haven")


# 2. Read the School enrollment data
school_clean1 <- read_xlsx("C:/Users/Dell/Downloads/ELSI_School_Enroll_1990_2001_Revised.xlsx")


# 3. Clean the data by removing unwanted symbols
school_clean1[school_clean1 == "†" | school_clean1 == "–"] <- NA


# 4. Change all variable names to lowercase
names(school_clean1) <- tolower(names(school_clean1))


# 5. Convert the file to long panel format (Transposing)
school_long <- long_panel(school_clean1, id="school_id", prefix="_", begin = 1990, end = 2001, label_location = "end")


# 6. Rename the wave variable and convert the variables to numeric types
school_long2 <- school_long %>% 
  rename(year = wave)

school_long$white <- as.integer(school_long$white)
school_long$black <- as.integer(school_long$black)
school_long$hispanic <- as.integer(school_long$hispanic)
school_long$ai_native <- as.integer(school_long$ai_native)
school_long$asian_pacif <- as.integer(school_long$asian_pacif)
school_long$total_enroll <- as.integer(school_long$total_enroll)


# 7. Create total_enroll2, sort by school_id and drop total_enroll
school_long2 <- school_long %>% 
  rowwise() %>% 
  mutate(total_enroll2 = white + black + hispanic + ai_native + asian_pacif) %>%  # Create total_enroll2
  rename(year = wave) %>% 
  arrange(school_id) %>%  # Sort by school_id
  select(-total_enroll) # Drop the total_enroll variable


# 8. Convert the school id into numeric format
school_long2$school_id <- as.numeric(as.character(school_long2$school_id))
typeof(school_long2$school_id)


# 9. Read the other files
school_counties <- read_xlsx("C:/Users/Dell/Downloads/ELSI_School_ID_County_FIPS_2001.xlsx")
school_coord <- read_xlsx("C:/Users/Dell/Downloads/All_School_Coordinates_2016_ELSI.xlsx")


# 10. Cleaning school_counties and school_coord data files
# Convert all column names to lowercase
names(school_counties) <- tolower(names(school_counties))
names(school_coord) <- tolower(names(school_coord))

school_counties$school_id <- as.numeric(as.character(school_counties$school_id))
school_coord$school_id <- as.numeric(as.character(school_coord$`school id nces`))

school_coord <- school_coord %>%
  select(school_name, state_name, county_number, school_id, state_abbr)

# Rename columns in school_counties to fix improper naming
school_counties <- school_counties %>%
  rename(school_name2 = `school name`)


# 11. Merge process using dplyr
# First, perform a left join of the school enrollment data (school_long2) with school_counties
combo_school_counties <- left_join(school_long2, school_counties, by = "school_id")

# Identify non-matching records using an anti_join
non_matching_records <- anti_join(school_long2, school_counties, by = "school_id")

# Merge non-matching records with school_coord using an inner join
non_matching_merged <- inner_join(non_matching_records, school_coord, by = "school_id")

# Combine the matching and non-matching records using a full join
final_combo <- bind_rows(combo_school_counties, non_matching_merged)


# 12. Clean the final data and use coalesce to ensure `fips_county` is complete
final_combo <- final_combo %>%
  # mutate(fips_county = coalesce(fips_state.x, county_number)) %>% # Did not need coalesce because county_number was already there
  select(c(school_id, year, school_name, state_name.x, fips_state.x, state_abbr.x, nces_id_short, nces_id_unique,
           white, black, hispanic, ai_native, asian_pacif, total_enroll2, agency_id, county_name, county_number))  # Remove unnecessary column


# 13. Aggregate data to county-year level
county_year_enrollments <- final_combo %>%
  group_by(county_number, year) %>%
  summarize(county_enroll = sum(total_enroll2, na.rm = TRUE))

# Convert county_number to numeric in county_year_enrollments
county_year_enrollments$county_number <- as.integer(county_year_enrollments$county_number)


# 14. Export the final aggregated data and clear the R environment
write_csv(county_year_enrollments, "C:/Users/Dell/Downloads/county_year_enrollments.csv")


# Clear all objects from the environment
rm(list = ls())

# Start fresh by reading in the new CSV file
county_year_enrollments <- read_csv("C:/Users/Dell/Downloads/county_year_enrollments.csv")


# 15. Read the census data and clean the dataset
# Read the SAS dataset "pop_est9001.sas7bdat"
census_orig <- read_sas("C:/Users/Dell/Downloads/pop_est9001.sas7bdat")

# Clean and prepare the census data
census_clean <- census_orig %>%
  mutate(county_number = as.integer(fips),  # Convert to numeric format
         population = pop,                      # Rename for clarity
         county_name2 = County,
         state_name = state) %>%
  select(county_number, year, population, county_name2, state_name, prop_age1524, prop_nonwhite, prop_male)  # Keep only needed variables


# 16. Merge the census data with the aggregated county-year school enrollment data
# Ensure both data frames have matching columns for merging
combo_school_enrollments <- left_join(county_year_enrollments, census_clean, by = c("county_number", "year"))


# 17. Read-in the crime data using haven
crime_data <- read_sas("C:/Users/Dell/Downloads/ucr_1990_2001_ori_level.sas7bdat")


# 18. Create a unique concatenated fips_state and fips_county variable
crime_data <- crime_data %>%
  mutate(fips_county = formatC(fips_county, digits = 0, width = 3, format = "d", flag = "0"),
         fips_state = formatC(fips_state, digits = 0, width = 2, format = "d", flag = "0"),
         fips_combined = str_c(fips_state, fips_county)) %>% # Concatenate state and county codes
  rename(year = yearucr)

# Convert fips_combined to numeric format for merging
crime_data$fips_combined <- as.numeric(crime_data$fips_combined)


# 19. Aggregate the crime data to the county-year level
crime_aggregated <- crime_data %>%
  group_by(fips_combined, year) %>%
  summarize(total_crime_county = mean(total_sum, na.rm = TRUE),
            total_crime_rate_old = mean(total_rate, na.rm = TRUE),
            violent_crime_county = mean(violent_sum, na.rm = TRUE),
            violent_rate_old = mean(violent_rate, na.rm = TRUE))


# 20. Merge the aggregated crime data with the combined school enrollments data
# First, create the same fips_combined variable in the school enrollments data by renaming county_number
combo_school_enrollments <- combo_school_enrollments %>%
  rename(fips_combined = county_number)

# Convert fips_combined to numeric format for consistency
combo_school_enrollments$fips_combined <- as.numeric(combo_school_enrollments$fips_combined)

# Perform the left join to merge with crime data
final_combo_school_crime <- left_join(combo_school_enrollments, crime_aggregated, by = c("fips_combined", "year"))


# 21. Read-in and prepare the economic data files
# List all SAS files in the specified directory
files <- list.files(path = "C:/Users/Dell/Downloads/BEA_Economic/", pattern = "*.sas7bdat", full.names = TRUE)

# Loop through each file, read it, rename columns, and add the year
j <- 1989
for (i in files) {
  j <- j + 1
  assign(paste0("reis_", j), read_sas(i))
  temp <- get(paste0("reis_", j))
  names(temp) <- c("fips_county", "area_name", "state", "percap_income", "employment")
  temp$year <- j
  assign(paste0("reis_", j), temp)
}

# Combine all economic files into a single data frame
econ_vect <- mget(ls(pattern = "reis_"))
econ_orig <- bind_rows(econ_vect)


# 22. Convert `fips_county` to numeric and merge with the most updated enrollment file
econ_orig$fips_county <- as.integer(econ_orig$fips_county)

final_combo_school_crime$fips_county <- final_combo_school_crime$fips_combined

# Merge the economic data with the combined school enrollments and crime data
final_combo <- left_join(final_combo_school_crime, econ_orig, by = c("fips_county", "year"))


# 23. Create new county-wide crime rate and employment rate variables
final_combo <- final_combo %>%
  mutate(new_tot_crime_rate = (total_crime_county / population) * 100000,
         new_violent_rate = (violent_crime_county / population) * 100000,
         employment_rate = (employment / population) * 1000) %>% 
  select(-fips_combined)


# 24. Finish cleaning and relocate variables as needed
final_combo <- final_combo %>%
  relocate(county_name2, state_name, area_name, state, .after = employment_rate)


# 25. Demean the independent and dependent variables for panel data regressions
school_dm_final <- final_combo %>%
  group_by(fips_county) %>%
  mutate(across(c(county_enroll:employment_rate), ~ .x - mean(.x, na.rm = TRUE), .names = "{col}_DM"))


# 26. Write the final file with demeaned variables to a CSV and clear the environment
write_csv(school_dm_final, "C:/Users/Dell/Downloads/school_dm_final.csv")

# Clear all objects from the R environment
rm(list = ls())

# Read-in the permanent CSV file to start fresh
school_dm_final <- read_csv("C:/Users/Dell/Downloads/school_dm_final.csv")

