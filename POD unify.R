# COVID-19 Pulse Survey - data cleaning and combining ---------------------
#
# Author:   Alexander Saeri
# Date:     7 Nov 2023


# # Libraries -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(gtools)
library(haven)

# # Constants -------------------------------------------------------------

file_path <- "Formatted data/"

# Get a list of all Excel file names in the formatted data folder
file_list <- list.files(path = file_path, pattern = "\\.xlsx$", full.names = TRUE)

wave_levels <- c(
  "March '21", "April '21", "May '21", "June '21", "July '21",
  "August '21", "September '21", "October '21", "November '21",
  "December '21", "January '22", "February '22", "March '22"
)

# # Functions -------------------------------------------------------------

# Function to read an Excel file and add a column with the file name
read_excel_with_filename <- function(file_path) {
  data_frame <- read_excel(file_path)
  data_frame$filename <- basename(file_path) # Add a new column with the file name
  return(data_frame)
}

# Function to sort a vector so variables are in order meta-data, demographics, Qs.
sort_variables <- function(variables) {
  # Categorize the variables
  ids <- grep("^(wave*|respondent_serial)", variables, value = TRUE)
  demographics <- grep("^d[0-9]", variables, value = TRUE)
  questions <- grep("^q[0-9]", variables, value = TRUE)
  metadata <- setdiff(variables, c(ids, demographics, questions))
  
  # Sort within each category
  ids_sorted <- mixedsort(ids)
  demographics_sorted <- mixedsort(demographics)
  questions_sorted <- mixedsort(questions)
  metadata_sorted <- mixedsort(metadata)
  
  # Combine the categories
  combined_sorted <- c(ids_sorted, demographics_sorted, questions_sorted, metadata_sorted)
  
  # Return the sorted vector
  return(combined_sorted)
}


# Function to detect variables with all values matching a given vector
detect_match <- function(data, match_values) {
  # Helper function to check for matching unique values
  has_only_values <- function(x, values) {
    unique_vals <- unique(na.omit(x))
    setequal(unique_vals, values)
  }
  
  # Apply the helper function across all columns and return names that match
  names(which(map_lgl(data, has_only_values, match_values)))
}


# # Read in data ----------------------------------------------------------

# Read in all files that are in the 'Formatted data/' folder into a list
df_list <- purrr::map(file_list, read_excel_with_filename) %>% 
  set_names(basename(file_list))

# Clean list elements (i.e., survey waves) before joining

## First wave is missing Wave and WaveRaw columns (March 2021), add them
df_list[[1]] <- df_list[[1]] %>%
  mutate(
    Wave = 'March \'21',
    WaveRaw = 202103
  )

## the END_DATE variable is not consistent across waves and isn't used; remove it
df_list <- df_list %>% 
  map(~ if ("END_DATE" %in% names(.x)) select(.x, -END_DATE) else .x)

## The elements are out of order, e.g., F1, F10, F2, F3... Fix this
df_list <- append(df_list[-2], df_list[2], after = 9)

## Add a variable tracking the wave number WaveNum, based on the list order
# df_list <- imap(df_list, ~mutate(.x, WaveNum = .y))

# Bind the waves into a single dataframe
d <- bind_rows(df_list)

# Basic data cleaning ---------------------------------------------------

# transform variable names into lowercase and add underscores for ease of reading
d <- d %>% 
  clean_names()

# rearrange the order of variables: metadata, demographics, survey questions
varnames_sorted <- sort_variables(names(d))

d <- d %>% 
  select(all_of(varnames_sorted), everything()) %>% 
    mutate(wave = factor(wave, levels = wave_levels))

# some variables stored NA as '.'; recode these as NA
d <- d %>%
  mutate(across(matches("^d|^q"), ~if (is.character(.)) na_if(., ".") else .))

# # Write combined formatted data ---------------------------------------------------

write_xlsx(d, "COVID-19 Pulse Survey - Combined formatted data.xlsx")
write_rds(d, "COVID-19 Pulse Survey - Combined formatted data.rds")
haven::write_dta(d, "COVID-19 Pulse Survey - Combined formatted data.dta")



# # Data cleaning and coding -------------------------------------------------------

# d <- read_rds("COVID-19 Pulse Survey - Combined formatted data.rds")

# naming convention for 'noyes' 'freq4' etc:
# description of scale (e.g., 'frequency') or anchors (e.g., no, yes)
# followed by number of response options
# if applicable, followed by options such as not applicable (na) or don't know (dk)

values_noyes2 <- c("No","Yes")
varlist_noyes2 <- detect_match(d, values_noyes2)

values_freq5_na <- c("Never", "Rarely", "Sometimes", "Often", "Always", "Not applicable")
varlist_freq5_na <- detect_match(d, values_freq5_na)

values_freqinfo4 <- c("Not at all", "Very little", "Somewhat", "A lot")
varlist_freqinfo4 <- detect_match(d, values_freqinfo4)

values_freq4 <- c("Never", "Sometimes", "Often", "Always")
varlist_freq4 <- detect_match(d, values_freq4)

values_freqcov6_na <- c("Never", "Only if required", "Only if COVID cases increase", "Sometimes", "Often", "Always", "Not applicable")
varlist_freqcov6_na <- detect_match(d, values_freqcov6_na)

values_worry3 <- c("Not at all worried", "A little worried", "Extremely worried")
varlist_worry3 <- detect_match(d, values_worry3)

values_ease4 <- c("Very difficult", "Somewhat difficult", "Somewhat easy", "Very easy")
varlist_ease4 <- detect_match(d, values_ease4)

values_likely4 <- c("Not at all likely", "Not very likely", "Somewhat likely", "Very likely")
varlist_likely4 <- detect_match(d, values_likely4)

values_lessmore5_na <- c("Much less", "Slightly less", "No change", "Slightly more", "Much more", "Not applicable")
varlist_lessmore5_na <- detect_match(d, values_lessmore5_na)

values_worsebetter3_dk <- c("Worse off", "About the same", "Better off", "Don't know")
varlist_worsebetter_3_dk <- detect_match(d, values_worsebetter3_dk)

values_lifesat7 <- c(
  "Completely dissatisfied",
  "Dissatisfied",
  "Somewhat dissatisfied",
  "Neither satisfied nor dissatisfied",
  "Somewhat satisfied",
  "Satisfied",
  "Completely satisfied"
)
varlist_lifesat7 <- detect_match(d, values_lifesat7)




d <- d %>%
  mutate(
    across(all_of(varlist_noyes2), ~ factor(.x, levels = values_noyes2)),
    across(all_of(varlist_freq5_na), ~ factor(.x, levels = values_freq5_na)),
    across(all_of(varlist_freqinfo4), ~ factor(.x, levels = values_freqinfo4)),
    across(all_of(varlist_freq4), ~ factor(.x, levels = values_freq4)),
    across(all_of(varlist_freqcov6_na), ~ factor(.x, levels = values_freqcov6_na)),
    across(all_of(varlist_worry3), ~ factor(.x, levels = values_worry3)),
    across(all_of(varlist_ease4), ~ factor(.x, levels = values_ease4)),
    across(all_of(varlist_likely4), ~ factor(.x, levels = values_likely4)),
    across(all_of(varlist_lessmore5_na), ~ factor(.x, levels = values_lessmore5_na)),
    across(all_of(varlist_lifesat7), ~ factor(.x, levels = values_lifesat7))
  )

d <- d %>% 
  mutate(
    d3_coded = fct_relevel(
      fct_collapse(factor(d3), 
                   "Male" = c("Male", "Man or male"), 
                   "Female" = c("Female", "Woman or female"), 
                   "Non-binary / other term" = c("Non-binary", "I use a different term (please specify)")
      ),
      "Female", "Male"),
    q2_coded = fct_relevel(
      fct_collapse(factor(q2),
                   "ASAP / within two months" = c("As soon as possible", "Within 1-2 months"),
                   "Within one year" = c("Within 3-5 months","Within 3-6 months", "Within 6-12 months"),
                   "More than one year" = c("In more than a year", "Within 1-2 years", "In more than 2 years"),
                   "Never" = "Never",
                   "Not sure" = "Not sure"),
      "ASAP / within two months", "Within one year", "More than one year", "Never", "Not sure"),
    q15_coded = fct_relevel(
      fct_collapse(factor(q15),
                   "0 doses" = "No",
                   "1 dose" = "Yes, first dose",
                   other_level = "2 or more doses"),
      "0 doses", "1 dose", "2 or more doses")
  )



# Allows for identiication of any variables that haven't been calssified yet.
# d %>%
#   select(-any_of(c(varlist_noyes,
#                    varlist_freq5_na,
#                    varlist_freqinfo4,
#                    varlist_freq4,
#                    varlist_freqcov6_na,
#                    varlist_worry3,
#                    varlist_ease4,
#                    varlist_likely4,
#                    varlist_lessmore5_na,
#                    varlist_lifesat7)),
#          -matches("text$")) %>%
#   sjPlot::view_df(., show.string.values = TRUE)


# 
# sjPlot::view_df(d)
# 
# foo <- d %>% 
#   select(matches("^q")) %>% 
#   select(!matches("text"))


# # Weight experimentation ------------------------------------------------

foo <- d %>% 
  select(wave, age = d2, sex = d3_coded, state = d4) %>% 
  mutate(uid = row_number()) %>% 
  mutate() %>% 
  filter(sex %in% c("Male","Female")) %>% 
  filter(state != "Other") %>% 
  mutate(state = fct_relevel(state, "NSW","VIC","QLD","WA","SA","TAS","ACT","NT"),
         age_group = factor(case_when(
           age >= 18 & age <= 19 ~ "18-19",
           age >= 20 & age <= 29 ~ "20-29",
           age >= 30 & age <= 39 ~ "30-39",
           age >= 40 & age <= 49 ~ "40-49",
           age >= 50 & age <= 59 ~ "50-59",
           age >= 60 & age <= 69 ~ "60-69",
           age >= 70 ~ "70+"
         )),
         sex = fct_drop(sex),
         state = fct_drop(state))
  

# at this point we assume we bring in ABS data - see POD reprsentativeness


raking_targets <- list(
  abs_sex %>% 
    select(-source, Freq = percent) %>% 
    mutate(sex = factor(sex),
           Freq = Freq * 100) %>% 
    as_tibble(),
  abs_state %>% 
    select(-source, Freq = percent) %>% 
    mutate(Freq = Freq * 100) %>% 
    as_tibble(),
  abs_age %>% 
    select(-source, Freq = percent) %>% 
    mutate(age_group = factor(age_group),
      Freq = Freq * 100) %>% 
    as_tibble()
)



# Custom function to weight individual waves:
rake_wave <- function(data_wave) {
  # Apply raking
  new_weights <- rake_survey(
    data_wave,
    # survey_margins = c("age_group", "sex", "state"),
    pop_margins = raking_targets,
    base_weight = weight
  )
  
  # Update the weights in the dataset
  data_wave$weight <- new_weights
  
  return(data_wave)
}


list_of_waves <- split(foo, foo$wave)

# Apply raking to each wave and store the results
list_of_waves <- lapply(list_of_waves, function(wave_data) {
  wave_data$wave_weight <- rake_survey(
    wave_data,
    pop_margins = raking_targets
  )
  return(wave_data)
})

# Recombine the data frames into one
foo_weighted <- bind_rows(list_of_waves)

foo$weight <- rake_survey(
  foo,
  pop_margins = raking_targets)

wave_weights <- numeric(nrow(foo))

# Loop through each wave
for (wave in unique(foo$wave)) {
  # Subset the data for the current wave
  wave_data <- filter(foo, wave == wave)
  
  # Apply raking to the wave-specific data
  wave_specific_weights <- rake_survey(
    wave_data,
    pop_margins = raking_targets
  )
  
  # Store the calculated weights in the wave_weights vector
  wave_weights[foo$wave == wave] <- wave_specific_weights
}

# Add the calculated wave-specific weights to your dataset
foo$wave_weight <- wave_weights

# Write data files --------------------------------------------------------

write_xlsx(d, "COVID-19 Pulse Survey - Cleaned combined formatted data.xlsx")
write_rds(d, "COVID-19 Pulse Survey - Cleaned combined formatted data.rds")
haven::write_dta(d, "COVID-19 Pulse Survey - Cleaned combined formatted data.dta")


# Data measurement table --------------------------------------------------

# This identifies in the waves in which each variable was measured. It is more
# reliable than the data dictionary

# d_measured <- d %>% 
#   mutate(across(everything(), as.character)) %>% 
#   pivot_longer(!wave) %>% 
#   group_by(wave, name) %>%
#   summarise(has_response = as.integer(any(!is.na(value)))) %>%
#   ungroup() %>%
#   pivot_wider(names_from = wave, values_from = has_response) %>% 
#   select(variable = name, any_of(wave_levels))
# 
# select(variable, variable_label, everything())
# d_measured <- d_measured[match(varnames_sorted, d_measured$variable), ]
# 
# d_dict_labels <- read_csv("Dictionary for each measured wave.csv") %>% 
#   select(variable, variable_label) %>% 
#   mutate(variable = make_clean_names(variable))
# asdf
# d_measured <- d_measured %>% 
#   left_join(d_dict_labels) %>% 
#   
#   write_csv(d_measured, "Validated dictionary for each measured wave.csv")

