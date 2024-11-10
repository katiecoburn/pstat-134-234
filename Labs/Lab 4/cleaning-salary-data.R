library(tidyverse)
library(tidymodels)
library(janitor)
# install.packages("ggstatsplot")
salary <- read_csv("Labs/Lab 4/data/salarydata.csv") %>% 
  clean_names()
# rename the columns: 
names(salary) <- c("timestamp", "age", "industry", "title",
                   "title_b", "salary", "bonuses", "currency",
                   "currency_b", "salary_b", "country",
                   "us_state", "city", "exp", "spec_exp",
                   "educ","gender", "race")
salary
salary %>% 
  ggplot(aes(x = salary)) + geom_histogram()
salary %>% arrange(salary)
salary %>% arrange(desc(salary))
salary$salary %>% max()
# drops the 113 rows with salary > 750000

salary %>% 
  filter(currency == "USD") %>% 
  filter(salary > 750000) %>% select(salary_b) %>% print(n = 103)
table(salary$currency)
salary <- salary %>% 
  mutate(salary_in_usd = case_when(
    currency == "USD" ~ salary,
    currency == "AUD/NZD" ~ salary*0.67, # not quite the same, NZD is 0.61
    currency == "CAD" ~ salary*0.72,
    currency == "CHF" ~ salary*1.16,
    currency == "EUR" ~ salary*1.09,
    currency == "GBP" ~ salary*1.31,
    currency == "HKD" ~ salary*0.13,
    currency == "JPY" ~ salary*0.0067,
    currency == "SEK" ~ salary*0.095,
    currency == "ZAR" ~ salary*0.057
  ))

salary %>% 
  ggplot(aes(x = salary_in_usd)) + geom_histogram()
salary %>% arrange(desc(salary_in_usd)) %>% 
  select(salary_in_usd, salary, currency, bonuses)
salary <- salary %>% 
  filter(salary_in_usd <= 750000)
salary %>% head() %>% View()

salary %>% 
  ggplot(aes(y = industry)) +
  geom_bar()
salary %>% 
  ggplot(aes(y = title)) +
  geom_bar()
salary %>% 
  ggplot(aes(y = country)) +
  geom_bar()
salary %>% group_by(country) %>% summarise(n()) %>% 
  arrange(desc(`n()`)) %>% print(n = 302)
# United States, USA, US, U.S., Usa, and United States of America
# are the same
# United Kingdom and UK are the same
# Let's fix that and see how it's looking:
us <- c("United States", "USA", "US", "U.S.", "Usa",
        "United States of America")
uk <- c("United Kingdom", "UK")
salary %>% 
  mutate(country_new = case_when(
    country %in% us ~ "USA",
    country %in% uk ~ "UK",
    .default = country
  )) %>% 
  select(country, country_new) %>% 
  group_by(country_new) %>% 
  summarise(n()) %>% 
  arrange(desc(`n()`))
# so we need to also include United states, united states,
# and usa
us <- c("United States", "USA", "US", "U.S.", "Usa",
        "United States of America", "United states",
        "united states", "usa")
# let's just look at it:
salary %>% 
  mutate(country_new = case_when(
    country %in% us ~ "USA",
    country %in% uk ~ "UK",
    country %in% canada ~ "Canada",
    country %in% netherlands ~ "Netherlands",
    .default = country
  )) %>% 
  select(country, country_new) %>% 
  group_by(country_new) %>% 
  summarise(n()) %>% 
  arrange(desc(`n()`)) %>% 
  print(n = 293)
# this is good and messy -- there are a lot of misspellings
# and 4 people responded with the American flag emoji
# We'll do our best to pull out all the US varieties
us <- c("United States", "USA", "US", "U.S.", "Usa",
        "United States of America", "United states",
        "united states", "usa", "🇺🇸", "US of A",
        "U.SA", "U.s.a.", "U. S", "United State of America",
        "United States Of America", "UnitedStates",
        "united States", "United states of America",
        "United Sates", "UNITED STATES", "United States of america",
        "United Stated", "Unites States", "U.S.A", "United State",
        "U.S", "America", "U.S.A.", "Us", "us", "The United States",
        "U. S.", "U.s.", "United Stares", "United Statea",
        "Unites states", "The US", "Unite States", "United Sates of America",
        "United States of American", "United Status", "u.s.",
        "U.S>", "Uniited States", "United  States", "United STates",
        "United Stateds", "United Statees", "United States is America",
        "United States of Americas", "United Statesp", "United Statss",
        "United Stattes", "United Statues", "United Statws",
        "United Sttes", "United states of america", "United statew",
        "uS", "uSA", "united stated", "united states of america",
        "america", "UsA", "Untied States", "Unted States", "Uniyes States",
        "Unitied States", "San Francisco", "California")
# UK consists of England, Scotland, Wales, and Northern Ireland
uk <- c("United Kingdom", "UK", "England, UK", "uk", "U.K.", "Uk",
        "United Kindom", "United Kingdom.", "United Kingdomk",
        "UK (Northern Ireland)", "U.K", "U.K. (northern England)",
        "England/UK", "England, UK.", "United Kingdom (England)",
        "UK (England)", "Scotland, UK", "United kingdom", "united kingdom",
        "England, United Kingdom", "Wales (UK)", "Wales (United Kingdom)",
        "Wales, UK", "England", "Scotland", "Wales", "Northern Ireland",
        "Northern Ireland, United Kingdom", "Great Britain")
canada <- c("Canada", "canada", "CANADA", "Canda", "Canad", "Canada, Ottawa, ontario",
            "Canadw")
netherlands <- c("Netherlands", "The Netherlands", "netherlands",
                 "The netherlands", "Nederland", "the Netherlands",
                 "the netherlands")
new_zealand <- c("New Zealand", "NZ", "New zealand", "new zealand")
salary %>% 
  mutate(country_new = case_when(
    country %in% us ~ "USA",
    country %in% uk ~ "UK",
    country %in% canada ~ "Canada",
    country %in% netherlands ~ "Netherlands",
    country %in% new_zealand ~ "New Zealand",
    .default = country
  )) %>% 
  group_by(country_new) %>% 
  add_count() %>% 
  filter(n > 3) %>% 
  select(-c(country, n)) %>% 
  ungroup()

salary %>% 
  ggplot(aes(y = gender)) + geom_bar()
salary %>% 
  mutate(gender_new = case_when(
    gender == "Prefer not to answer" ~ "Other or prefer not to answer",
    is.na(gender) ~ "Other or prefer not to answer",
    .default = gender
  )) %>% 
  ggplot(aes(y = fct_infreq(gender_new))) + geom_bar()

salary %>% ggplot(aes(y = race)) + geom_bar()
# This is a whole new fun situation
salary %>% mutate(
  race_simplified = case_when(
    race == "White" ~ "White",
    race == "Asian or Asian American" ~ "Asian or Asian American",
    race == "Hispanic, Latino, or Spanish origin" ~ "Hispanic, Latino, or Spanish origin",
    race == "Middle Eastern or Northern African" ~ "Middle Eastern or Northern African",
    race == "Black or African American" ~ "Black or African American",
    race == "Native American or Alaska Native" ~ "Native American or Alaska Native",
    race == "Another option listed here or prefer not to answer" ~ "Other/Prefer not to answer",
    .default = "Multiple options"
    )
) %>% 
  ggplot(aes(y = fct_infreq(race_simplified))) + geom_bar()
  select(race_simplified, race) %>% print(n = 200)

salary %>% select(title) %>% View()

salary %>% 
  ggplot(aes(y = industry)) + geom_bar()

# age needs no cleaning
salary %>% 
  ggplot(aes(y = age)) + geom_bar()
# same for experience
salary %>% 
  ggplot(aes(y = fct_infreq(exp))) + geom_bar()
salary %>% 
  ggplot(aes(y = fct_infreq(spec_exp))) + geom_bar()
# Education has some missingness; we could impute it
# But based on comments on the website with the survey,
# these missing values might be for degrees for not listed
# So maybe we should make them "Other"
salary %>% 
  mutate(educ = case_when(
    is.na(educ) ~ "Other",
    .default = educ
  )) %>% 
  ggplot(aes(y = fct_infreq(educ))) + geom_bar()

salary_cleaned <- salary %>% 
  mutate(
    race_simplified = case_when(
      race == "White" ~ "White",
      race == "Asian or Asian American" ~ "Asian or Asian American",
      race == "Hispanic, Latino, or Spanish origin" ~ "Hispanic, Latino, or Spanish origin",
      race == "Middle Eastern or Northern African" ~ "Middle Eastern or Northern African",
      race == "Black or African American" ~ "Black or African American",
      race == "Native American or Alaska Native" ~ "Native American or Alaska Native",
      race == "Another option listed here or prefer not to answer" ~ "Other/Prefer not to answer",
      .default = "Multiple options"
    )
  ) %>% 
  mutate(gender_new = case_when(
    gender == "Prefer not to answer" ~ "Other or prefer not to answer",
    is.na(gender) ~ "Other or prefer not to answer",
    .default = gender
  )) %>% 
  mutate(educ = case_when(
    is.na(educ) ~ "Other",
    .default = educ
  )) %>% 
  mutate(salary_in_usd = case_when(
    currency == "USD" ~ salary,
    currency == "AUD/NZD" ~ salary*0.67, # not quite the same, NZD is 0.61
    currency == "CAD" ~ salary*0.72,
    currency == "CHF" ~ salary*1.16,
    currency == "EUR" ~ salary*1.09,
    currency == "GBP" ~ salary*1.31,
    currency == "HKD" ~ salary*0.13,
    currency == "JPY" ~ salary*0.0067,
    currency == "SEK" ~ salary*0.095,
    currency == "ZAR" ~ salary*0.057
  )) %>% 
  mutate(country_new = case_when(
    country %in% us ~ "USA",
    country %in% uk ~ "UK",
    country %in% canada ~ "Canada",
    country %in% netherlands ~ "Netherlands",
    country %in% new_zealand ~ "New Zealand",
    .default = country
  )) %>% 
  group_by(country_new) %>% 
  add_count() %>% 
  filter(n > 3) %>% 
  select(-c(country, n)) %>% 
  ungroup() %>% 
  select(age, exp, spec_exp, race_simplified, country_new,
         gender_new, salary_in_usd)
salary_cleaned
salary_cleaned %>% 
  ggplot(aes(y = reorder(spec_exp, salary_in_usd,
                         FUN = median), 
             x = salary_in_usd)) + geom_violin()

salary_cleaned %>% 
  ggplot(aes(y = reorder(exp, salary_in_usd,
                         FUN = median), 
             x = salary_in_usd)) + geom_boxplot()

salary_cleaned %>% 
  ggplot(aes(y = reorder(country_new, salary_in_usd,
                         FUN = median), 
             x = salary_in_usd)) + geom_boxplot() +
  geom_violin()
salary %>% select(us_state) %>% 
  table()
