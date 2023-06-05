library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(psych)

#1.

set.seed(1)
nbr_row <- 10000
start_date <- as.Date('2021-01-01')  
end_date <- as.Date('2022-12-31')


df <- data.frame(matrix(nrow = nbr_row))

person_id <- 1:nbr_row
sex_cd <- sample(c(0, 1, 2, 9, NA), nrow(df), replace=TRUE, prob=c(0.05, 0.45, 0.4, 0.04, 0.01))
#age_nm <- c(sample(18 : 115, size = nrow(df), replace = TRUE))
age_nm <- abs(round(rnorm(nrow(df), mean = 50, sd = 15)))
exitus_bl <- sample(c(TRUE, FALSE), size = nrow(df), replace=TRUE, prob=c(0.05, 0.95))
country_cd <- sample(c('BE', 'other', NA), nrow(df), replace=TRUE, prob=c(0.04, 0.95, 0.01))

residence_area_cd <- sample(c('NUTS1', 'NUTS2','NUTS3'), nrow(df), replace=TRUE, prob=c(0.10, 0.70, 0.20))
education_level_cd <- sample(c("Low", "Middle", "High"), nrow(df), replace=TRUE, prob=c(0.30, 0.50, 0.20))
income_category_cd <- sample(c('Low', 'Middle', 'High'), nrow(df), replace=TRUE, prob=c(0.20, 0.40, 0.40))
migration_background_cd <- sample(c('NATIVE', 'EU', 'NON-EU', 'PAR'), nrow(df), replace=TRUE)
household_type_cd <- sample(c("ALONE", "COUPLE", "COUPLE_CHILD", "LONE", "EXTENDED", "OTHER"), nrow(df), replace=TRUE, prob=c(0.15, 0.30, 0.35, 0.10, 0.05, 0.05))
hospi_due_to_covid_bl <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.15, 0.85))
#test_positive_to_covid_nm <- sample(c(0, 1, 2, 3, 4, 5, 6, 7), nrow(df), replace=TRUE, prob=c(0.25, 0.40, 0.20, 0.08, 0.04, 0.01, 0.01, 0.01))
test_nm <- sample(c(1, 2, 3, 4, 5, 6, 7), nrow(df), replace=TRUE, prob=c(0.05, 0.15, 0.15, 0.3, 0.18, 0.12, 0.05))
doses_nm <- sample(c(0, 1, 2, 3), nrow(df), replace=TRUE, prob=c(0.15, 0.15, 0.20, 0.5))
dose_1_brand_cd <- sample(c("BP", "MD", "AZ", "JJ", "NV", NA), nrow(df), replace=TRUE)
dose_1_dt <- sample(seq(as.Date(start_date), as.Date(end_date), by = "day"), nrow(df), replace=TRUE)
dose_2_brand_cd <- sample(c("BP", "MD", "AZ", "JJ", "NV", NA), nrow(df), replace=TRUE)
dose_2_dt <- sample(seq(as.Date(start_date), as.Date(end_date), by = "day"), nrow(df), replace=TRUE)
dose_3_brand_cd <- sample(c("BP", "MD", "AZ", "JJ", "NV", NA), nrow(df), replace=TRUE)
dose_3_dt <- sample(seq(as.Date(start_date), as.Date(end_date), by = "day"), nrow(df), replace=TRUE)

df <- data.frame(person_id, 
                 sex_cd,
                 age_nm,
                 exitus_bl,
                 country_cd,
                 residence_area_cd,
                 education_level_cd,
                 income_category_cd,
                 migration_background_cd,
                 household_type_cd,
                 hospi_due_to_covid_bl,
                 #test_positive_to_covid_nm,
                 test_nm,
                 doses_nm,
                 dose_1_brand_cd,
                 dose_1_dt,
                 dose_2_brand_cd,
                 dose_2_dt,
                 dose_3_brand_cd,
                 dose_3_dt
                 )

# Assign country of origin depending of migration_background_cd
df <- df %>% 
  mutate(country_origin_cd = case_when(migration_background_cd == 'NATIVE' ~ 'BE',
                                        migration_background_cd == 'EU' ~ 'other',
                                        migration_background_cd == 'NON-EU' ~ 'other',
                                        migration_background_cd == 'PAR' ~ 'other',
                                        .default = NA ))


# Assign fully_vaccinated_bl
df <- df %>% 
  mutate(fully_vaccinated_bl = case_when(
    dose_1_brand_cd %in% c('BP', 'MD', 'AZ', 'NV') & 
      dose_2_brand_cd %in% c('BP', 'MD', 'AZ', 'NV') &
      dose_3_brand_cd %in% c('BP', 'MD', 'AZ', 'NV') ~ TRUE,
    dose_1_brand_cd == 'JJ' & 
      dose_2_brand_cd == 'JJ' ~ TRUE, .default = FALSE))

# Create test_positive_to_covid_nm depending of test_nm

df$test_positive_to_covid_nm <- as.integer(runif(nrow(df), min = 0, max = df$test_nm))

# Assign date of death if exitus_bl == TRUE

exitus_dt <- data.frame(exitus_dt = sample(seq(as.Date(start_date), as.Date(end_date), by = "day"), nrow(df[df$exitus_bl == TRUE, ]), replace = TRUE))
exitus_true <- df %>% filter(exitus_bl == TRUE) %>% select(person_id)
exitus_true <- cbind(exitus_true, exitus_dt)

#     left join
df <- merge(x=df,y=exitus_true, 
             by="person_id", all.x=TRUE)

# reorder columns
colnames(df)
df <- df[, c(1, 2, 3, 4, 21, 5, 6, 7, 8, 9, 10, 12, 20, 13, 11, 14, 15, 16, 17, 18, 19)]

# custom csv name
number_formatter <- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), "K"),
    x < 1e9 ~ paste0(as.character(x/1e6), "M"),
    TRUE ~ "To be implemented..."
  )
}

# export to csv
write.csv(df, sprintf("use_case_1_synthetic_data_%s_individuals.csv", number_formatter(nbr_row)), row.names=FALSE)

describe(df)
