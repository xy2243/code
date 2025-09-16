# 加载需要的包

library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(magrittr)   # 有时候需要这个
install.packages("readxl")
library(readxl)


# ────────────────────────────────
# Part A: Age distribution (San Diego)
# ────────────────────────────────
casand <- read_excel("~/Desktop/Research Data/ca_san_diego_2020_04_01.xlsx", col_types = "text")
glimpse(casand)

casand_clean <- casand %>% filter(!is.na(subject_age))

pmf <- casand_clean %>%
  count(subject_age) %>%
  mutate(probability = n / sum(n))

cdf <- pmf %>%
  arrange(subject_age) %>%
  mutate(cumulative_probability = cumsum(probability))

ggplot(pmf, aes(x = subject_age, y = probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "PMF: Age Distribution of Stopped Drivers - San Diego",
       x = "Age", y = "Probability")

ggplot(cdf, aes(x = subject_age, y = cumulative_probability, group = 1)) +
  geom_line(color = "darkgreen") +
  labs(title = "CDF: Age Distribution of Stopped Drivers - San Diego",
       x = "Age", y = "Cumulative Probability")

max_age <- pmf %>% filter(probability == max(probability))
print(max_age)

# ────────────────────────────────
# Part A: Age distribution (San Francisco)
# ────────────────────────────────
casanf <- read_excel("~/Desktop/Research Data/ca_san_francisco_2020_04_01.xlsx", col_types = "text")
glimpse(casanf)

casanf_clean <- casanf %>% filter(!is.na(subject_age), subject_age != "NA")

pmf <- casanf_clean %>%
  count(subject_age) %>%
  mutate(probability = n / sum(n))

cdf <- pmf %>%
  arrange(subject_age) %>%
  mutate(cumulative_probability = cumsum(probability))

ggplot(pmf, aes(x = subject_age, y = probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "PMF: Age Distribution of Stopped Drivers - San Francisco",
       x = "Age", y = "Probability")

ggplot(cdf, aes(x = subject_age, y = cumulative_probability, group = 1)) +
  geom_line(color = "darkgreen") +
  labs(title = "CDF: Age Distribution of Stopped Drivers - San Francisco",
       x = "Age", y = "Cumulative Probability")

max_age <- pmf %>% filter(probability == max(probability))
print(max_age)

# ────────────────────────────────
# Part B: Relative probability by race (San Diego & San Francisco)
# ────────────────────────────────
# San Diego
race_counts <- casand_clean %>%
  count(subject_race) %>%
  mutate(proportion_stopped = n / sum(n))
print(race_counts)

# Extract probabilities
p_black <- race_counts %>% filter(subject_race == "black") %>% pull(proportion_stopped)
p_white <- race_counts %>% filter(subject_race == "white") %>% pull(proportion_stopped)
p_hispanic <- race_counts %>% filter(subject_race == "hispanic") %>% pull(proportion_stopped)
p_asian <- race_counts %>% filter(subject_race == "asian") %>% pull(proportion_stopped)

# San Francisco
race_counts <- casanf_clean %>%
  count(subject_race) %>%
  mutate(proportion_stopped = n / sum(n))
print(race_counts)

p_black <- race_counts %>% filter(subject_race == "black") %>% pull(proportion_stopped)
p_white <- race_counts %>% filter(subject_race == "white") %>% pull(proportion_stopped)
p_hispanic <- race_counts %>% filter(subject_race == "hispanic") %>% pull(proportion_stopped)
p_asian <- race_counts %>% filter(subject_race == "asian/pacific islander") %>% pull(proportion_stopped)

# ────────────────────────────────
# Part C: Conditioning on race & gender
# ────────────────────────────────
race_gender_counts <- casanf_clean %>%
  count(subject_race, subject_sex) %>%
  mutate(proportion_stopped = n / sum(n))
print(race_gender_counts)

# Extract examples
p_black_female <- race_gender_counts %>% filter(subject_race == "black", subject_sex == "female") %>% pull(proportion_stopped)
p_black_male <- race_gender_counts %>% filter(subject_race == "black", subject_sex == "male") %>% pull(proportion_stopped)

# ────────────────────────────────
# Part D: Search probability & contraband rates
# ────────────────────────────────
search_counts <- casanf_clean %>%
  group_by(subject_race) %>%
  summarize(
    total_stops = n(),
    searches_conducted = sum(search_conducted == TRUE)
  ) %>%
  mutate(proportion_searched = searches_conducted / total_stops)
print(search_counts)

contraband_rate_by_race <- casanf_clean %>%
  filter(search_conducted == TRUE) %>%
  group_by(subject_race) %>%
  summarize(
    total_searches = n(),
    contraband_found = sum(contraband_found == TRUE),
    p_contraband_given_search = contraband_found / total_searches
  )
print(contraband_rate_by_race)

# ────────────────────────────────
# Part E: PMF & CDF by gender (San Diego)
# ────────────────────────────────
casand_clean <- casand %>% filter(!is.na(subject_age), !is.na(subject_sex))

pmf_female <- casand_clean %>%
  filter(subject_sex == "female") %>%
  count(subject_age) %>%
  mutate(probability = n / sum(n))

pmf_male <- casand_clean %>%
  filter(subject_sex == "male") %>%
  count(subject_age) %>%
  mutate(probability = n / sum(n))

cdf_female <- pmf_female %>%
  arrange(subject_age) %>%
  mutate(cumulative_probability = cumsum(probability))

cdf_male <- pmf_male %>%
  arrange(subject_age) %>%
  mutate(cumulative_probability = cumsum(probability))

ggplot() +
  geom_bar(data = pmf_female, aes(x = subject_age, y = probability), stat = "identity",
           fill = "purple", alpha = 0.6) +
  geom_bar(data = pmf_male, aes(x = subject_age, y = probability), stat = "identity",
           fill = "blue", alpha = 0.6) +
  labs(title = "PMF: Age Distribution of Stopped Drivers by Gender (San Diego)",
       x = "Age", y = "Probability") +
  theme_minimal()

ggplot() +
  geom_line(data = cdf_female, aes(x = subject_age, y = cumulative_probability, group = 1, color = "Female")) +
  geom_line(data = cdf_male, aes(x = subject_age, y = cumulative_probability, group = 1, color = "Male")) +
  labs(title = "CDF: Age Distribution of Stopped Drivers by Gender (San Diego)",
       x = "Age", y = "Cumulative Probability") +
  scale_color_manual(values = c("Female" = "purple", "Male" = "blue")) +
  theme_minimal()
