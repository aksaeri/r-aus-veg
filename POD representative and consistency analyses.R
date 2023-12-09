# COVID-19 Pulse Survey - checking representativeness  ---------------------
#
# Author:   Alexander Saeri
# Date:     22 Nov 2023


# # Libraries -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(gtools)
library(haven)


# # Functions -------------------------------------------------------------


abs_linefunc <- function(data, category_col, category_value) {
  filtered_data <- data %>% filter(.[[category_col]] == category_value)
  if (nrow(filtered_data) > 0) {
    return(filtered_data$percent)
  } else {
    return(NA)
  }
}

# # Constants -------------------------------------------------------------

abs_sex <- readxl::read_excel("abs_census2021_counts.xlsx", sheet = "sex") %>% 
  adorn_percentages("col") %>% 
  rename(percent = n) %>% 
  mutate(source = "abs")

abs_age <- readxl::read_excel("abs_census2021_counts.xlsx", sheet = "age") %>% 
  filter(age >= 18) %>% 
  mutate(age_group = case_when(
    age >= 18 & age <= 19 ~ "18-19",
    age >= 20 & age <= 29 ~ "20-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 ~ "70+"
  )) %>% 
  group_by(age_group) %>%
  summarise(n = sum(n)) %>% 
  adorn_percentages("col") %>% 
  rename(percent = n) %>% 
  mutate(source = "abs")

abs_state <- readxl::read_excel("abs_census2021_counts.xlsx", sheet = "state") %>% 
  mutate(state = case_when(state == "New South Wales" ~ "NSW",
                           state == "Victoria" ~ "VIC",
                           state == "Queensland" ~ "QLD",
                           state == "Western Australia" ~ "WA",
                           state == "South Australia" ~ "SA",
                           state == "Tasmania" ~ "TAS",
                           state == "Australian Capital Territory" ~ "ACT",
                           state == "Northern Territory" ~ "NT")) %>% 
  mutate(state = fct_relevel(state, "NSW","VIC","QLD","WA","SA","TAS","ACT","NT")) %>% 
  adorn_percentages("col") %>% 
  rename(percent = n) %>% 
  filter(!is.na(state)) %>% 
  mutate(source = "abs")

d <- read_rds("COVID-19 Pulse Survey - Formatted combined data.rds")

  

# # Sex -------------------------------------------------------------------


# # # Whole sample --------------------------------------------------------


d_sex <- d %>%
  filter(d3_coded %in% c("Male", "Female")) %>%
  rename(sex = d3_coded) %>%
  tabyl(sex) %>%
  filter(!n == 0) %>% 
  mutate(source = "pulse") %>% 
  bind_rows(abs_sex)

ggplot(d_sex, aes(x = sex, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "COVID-19 pulse survey: Sex - overall sample vs. 2021 Census",
       x = "Gender",
       y = "Percentage",
       fill = "Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/Pulse survey - sex - overall sample vs ABS comparison.png", width = 10, height = 6, bg = "white")

# # # By wave -------------------------------------------------------------



d_sex_bywave <- d %>% 
  filter(d3_coded %in% c("Male", "Female")) %>% 
  rename(sex = d3_coded) %>%
  summarise(n = n(), .by = c(sex, wave)) %>% 
  mutate(percent = n/sum(n), .by = wave)


d_sex_bywave <- d_sex_bywave %>%
  mutate(abs_benchmark = map_dbl(sex, ~ abs_linefunc(abs_sex, "sex", .x)))

ggplot(d_sex_bywave, aes(x = wave, y = percent, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)),
            position = position_dodge(width = 0.9), hjust = 2, size = 3, angle = 90) +
  facet_wrap(~ sex) +  # This creates a separate plot for each gender
  geom_text(data = abs_sex,
            aes(y = 0.6, x = 0.2, label = paste0("Census: ",scales::percent(percent, accuracy = 0.1))),
            hjust = -0.1, vjust = 0, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_hline(aes(yintercept = abs_benchmark), color = "black", linetype = "dashed") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "COVID-19 pulse survey: sex - wave by wave vs. 2021 Census",
       x = "Wave",
       y = "Percentage",
       fill = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/Pulse survey - sex - wave by wave vs ABS comparison.png", width = 10, height = 6, bg = "white")


# # State / territory -----------------------------------------------------

# # # Whole sample --------------------------------------------------------


d_state <- d %>%
  rename(state = d4) %>%
  mutate(state = fct_relevel(state, "NSW","VIC","QLD","WA","SA","TAS","ACT","NT")) %>% 
  tabyl(state)%>% 
  mutate(source = "pulse") %>% 
  bind_rows(abs_state)

ggplot(d_state, aes(x = state, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "COVID-19 pulse survey: location - overall sample vs. 2021 Census",
       x = "Wave",
       y = "Percentage",
       fill = "Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("plots/Pulse survey - location - overall sample vs ABS comparison.png", width = 10, height = 6, bg = "white")


# # # By wave -------------------------------------------------------------

d_state_bywave <- d %>% 
  filter(d4 != "Other") %>%
  rename(state = d4) %>%
  mutate(state = fct_relevel(state, "NSW","VIC","QLD","WA","SA","TAS","ACT","NT")) %>% 
  summarise(n = n(), .by = c(state, wave)) %>% 
  mutate(percent = n/sum(n), .by = wave)

d_state_bywave <- d_state_bywave %>%
  mutate(abs_benchmark = map_dbl(state, ~ abs_linefunc(abs_state, "state", .x)))

ggplot(d_state_bywave, aes(x = wave, y = percent, fill = state)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)),
            position = position_dodge(width = 0.9), hjust = -.2, size = 3, angle = 90) +
  geom_text(data = abs_state,
            aes(y = 0.35, x = 0.2, label = paste0("Census: ",scales::percent(percent, accuracy = 0.1))),
            hjust = -0.1, vjust = 1, color = "black") +
  facet_wrap(~ state) +  # This creates a separate plot for each state
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = "Set1") +
  geom_hline(aes(yintercept = abs_benchmark), color = "black", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "COVID-19 pulse survey: location - wave by wave vs. 2021 Census",
       x = "Wave",
       y = "Percentage",
       fill = "Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("plots/Pulse survey - location - wave by wave vs ABS comparison.png", width = 10, height = 8, bg = "white")



# # Age -------------------------------------------------------------------

# # # Whole sample --------------------------------------------------------


d_age <- d %>%
  rename(age = d2) %>%
  mutate(age_group = factor(case_when(
    age >= 18 & age <= 19 ~ "18-19",
    age >= 20 & age <= 29 ~ "20-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 ~ "70+"
  ))) %>% 
  tabyl(age_group) %>% 
  mutate(source = "pulse") %>% 
  bind_rows(abs_age)



ggplot(d_age, aes(x = age_group, y = percent, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "COVID-19 pulse survey: age - overall sample vs. 2021 Census",
       x = "Age",
       y = "Percentage",
       fill = "Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("plots/Pulse survey - age - overall sample vs ABS comparison.png", width = 10, height = 6, bg = "white")


# # # By wave -------------------------------------------------------------


d_age_bywave <- d %>% 
  # filter(d4 != "Other") %>%
  rename(age = d2) %>%
  mutate(age_group = factor(case_when(
    age >= 18 & age <= 19 ~ "18-19",
    age >= 20 & age <= 29 ~ "20-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 ~ "70+"
  ))) %>% 
  summarise(n = n(), .by = c(age_group, wave)) %>% 
  mutate(percent = n/sum(n), .by = wave)


d_age_bywave <- d_age_bywave %>%
  mutate(abs_benchmark = map_dbl(age_group, ~ abs_linefunc(abs_age, "age_group", .x)))

ggplot(d_age_bywave, aes(x = wave, y = percent, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)),
            position = position_dodge(width = 0.9), hjust = -.2, size = 3, angle = 90) +
  geom_text(data = abs_age,
            aes(y = 0.3, x = .2, label = paste0("Census: ",scales::percent(percent, accuracy = 0.1))),
            hjust = -0.1, vjust = 1, color = "black") +
  facet_wrap(~ age_group) +  # This creates a separate plot for each state
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(aes(yintercept = abs_benchmark), color = "black", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "COVID-19 pulse survey: age - wave by wave vs. 2021 Census",
       x = "Wave",
       y = "Percentage",
       fill = "Age group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("plots/Pulse survey - age - wave by wave vs ABS comparison.png", width = 10, height = 8, bg = "white")

d_geo <- d %>% 
  summarise(n = n(), .by = c(d5, wave)) %>% 
  filter(!is.na(d5)) %>% 
  mutate(percent = n / sum(n), .by = wave)

d_geo %>% 
  ggplot(aes(x = wave, y = percent, fill = d5)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  geom_text(aes(label = scales::percent_format(accuracy = 0.1)(percent)),    
            position = position_stack(vjust = 0.5),
            color = "white", size = 3) +  # Adjust color and size as needed
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+ 
  labs(title = "COVID-19 pulse survey: Metro / regional by wave",
       x = "Wave",
       y = "Percentage",
       fill = "Live in metro area")
  
ggsave("plots/Pulse survey - metro-regional - wave by wave.png", width = 10, height = 6, bg = "white")



  
# d_efa <- d %>% 
#   # select(matches("^q3_[0-9]+$")) %>% 
#   select(q3_2,
#          q3_3,
#          q3_5,
#          q3_6,
#          q3_7,
#          q3_8,
#          q3_11,
#          q3_12,
#          q3_13,
#          q3_15) %>% 
#   mutate(across(everything(), ~ case_when(.x == "No" ~ 0,
#                                           .x == "Yes" ~ 1))) %>% 
#   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% # impute means 
#   drop_na()
# 
# fa_stats <- psych::fa(d_efa, nfactors = 2, rotate = "oblimin")
# psych::scree(fa_stats)
