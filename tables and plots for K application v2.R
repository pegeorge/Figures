#####                     obtain patient list who are lacking TCDs 

library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(knitr)

rm(list=ls())


#####           Set the working directory where the original dataset is located
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/K application/")
list.files()


#               load the initial datasets      ----------------------------------------------------
load('../../Papers and Projects/DATABASES RAW/SCD/Output/combined_with_TCD.rda')


#               initial info
total_patients = length(unique(combined_with_TCD$corp_id))

ls(combined_with_TCD)


table_1.df = combined_with_TCD %>% dplyr::select(corp_id, adm_date, dsch_date, dsch_year, HU_at_visit, Insurance_at_visit, pat_class)

write.csv(table_1.df, file = 'table_1.df.csv')

str(table_1.df)


# Assume that the data is in a dataframe named df
table_1.df$adm_date <- as.Date(table_1.df$adm_date)
table_1.df$dsch_date <- as.Date(table_1.df$dsch_date)

# Arrange the data by corp_id and adm_date
table_1.df <- table_1.df %>%
  arrange(corp_id, adm_date)

# Calculate HU_sequence
table_1.df <- table_1.df %>%
  group_by(corp_id) %>%
  mutate(HU_break = HU_at_visit != lag(HU_at_visit, default = HU_at_visit[1])) %>%
  ungroup() %>%
  mutate(HU_sequence = cumsum(HU_break))

# Group the data by corp_id and HU_sequence, and calculate the duration in days
HU_sequences_days <- table_1.df %>%
  filter(HU_at_visit == 1) %>%
  group_by(corp_id, HU_sequence) %>%
  summarise(HU_duration = as.numeric(difftime(max(dsch_date), min(adm_date), units = "days")))

# The duration of the longest HU sequence for each patient is the longest period of continuous HU use
longest_HU_sequence_days <- HU_sequences_days %>%
  group_by(corp_id) %>%
  summarise(max_HU_duration = max(HU_duration))

# Get a list of all unique patient IDs
all_patients <- unique(table_1.df$corp_id)

# Get a list of patients who have used HU
HU_patients <- unique(longest_HU_sequence_days$corp_id)

# Find patients who have never used HU
never_HU_patients <- setdiff(all_patients, HU_patients)

# Create a data frame for patients who have never used HU
never_HU_df <- data.frame(corp_id = never_HU_patients, max_HU_duration = 0)

# Add the never HU patients to the longest_HU_sequence_days data frame
longest_HU_sequence_days <- rbind(longest_HU_sequence_days, never_HU_df)


# Define color palette
color_palette <- c("#f7f7f7", "#2c7bb6")


# Plot the histogram with a log scale for the y-axis
ggplot(longest_HU_sequence_days, aes(x = max_HU_duration)) +
  geom_histogram(bins = 20, color = "black", fill = color_palette[2], alpha = 0.7) +
  scale_y_log5() +
  labs(x = "Days of Continuous HU Use", y = "Number of Patients (Log Scale)", title = "Distribution of Long-term Continuous HU Use") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))









# Calculate the number of encounters
encounters_table <- table_1.df %>%
  group_by(dsch_year, pat_class) %>%
  summarise(n_encounters = n(), .groups = "drop") %>%
  spread(key = pat_class, value = n_encounters, fill = 0)

# Calculate the number of unique patients per year
unique_patients <- table_1.df %>%
  group_by(dsch_year) %>%
  summarise(n_unique_patients = n_distinct(corp_id), .groups = "drop")

# Join the unique patients per year to the encounters table
table_one <- left_join(encounters_table, unique_patients, by = "dsch_year")

# Display the table
print(table_one)

write.csv(table_one, 'table_one.csv')




#######        create age_at_visit and age_at_death variable ----------------------------------------------------
combined_with_TCD$deceased_date = mdy(combined_with_TCD$deceased_date)

df.1 = combined_with_TCD %>% 
  mutate(age_at_visit_days = (adm_date - dob), 
         age_at_death_days = (deceased_date - dob))

df.1$age_at_visit_years = format(round(as.numeric(df.1$age_at_visit_days / 365.25), 1), nsmall = 1)
df.1$age_at_death_years = format(round(as.numeric(df.1$age_at_death_days / 365.25), 1), nsmall = 1)




#######      next, create deceased (yes vs no) 
df.1 <- df.1 %>% 
  mutate(deceased_yn = ifelse(is.na(deceased_date), 0, 1))


#count number of patients who have died
df.1 %>% 
  group_by(deceased_yn) %>% 
  summarise(count = n_distinct(corp_id))



#######         next, create time between visits variable  ------------------------------------------------------------


#create variable of time between visits (includes all visits, OP ED IP)
df.1 <- df.1 %>% 
  group_by(corp_id) %>% 
  arrange(corp_id, adm_date) %>% 
  mutate(visit_number = seq_along(adm_date), 
         last_visit=last(adm_date), 
         age_at_last_visit_years = last(adm_date) - dob)

df.1$age_at_last_visit_years <- format(round(as.numeric(df.1$age_at_last_visit_years / 365.25), 1), nsmall = 1)

#create variable that shows max visit_number
df.1 <- df.1 %>% 
  group_by(corp_id) %>% 
  mutate(visit_number_max = max(visit_number))


#create a time_since and time_since_max variable
range(df.1$adm_date)
placeholder_date <- as.Date("2022-01-01")


df.1 <- df.1 %>% 
  group_by(corp_id) %>% 
  arrange(corp_id, adm_date) %>% 
  mutate(time_between_visits_days = ifelse(visit_number_max == 1, placeholder_date - adm_date, 
                             ifelse(visit_number < visit_number_max, adm_date-lag(adm_date), placeholder_date - adm_date)))

df.1 <- df.1 %>% 
  group_by(corp_id) %>% 
  mutate(time_between_visits_days_max = max(time_between_visits_days, na.rm = TRUE))

df.1$time_between_visits_days_max <- as.numeric(df.1$time_between_visits_days_max)

check.df = df.1 %>% select(corp_id, visit_number, visit_number_max, adm_date, time_between_visits_days, time_between_visits_days_max, age_at_last_visit_years)





#######         do the same as above, but for clinic visits only 

clinic.df = df.1 %>% filter(pat_class == 'OP')

#create variable of time between visits (includes all visits, OP ED IP)
clinic.df <- clinic.df %>% 
  group_by(corp_id) %>% 
  arrange(corp_id, adm_date) %>% 
  mutate(clinic_visit_number = seq_along(adm_date), 
         last_clinic_visit=last(adm_date), 
         age_at_last_clinic_visit_years = last(adm_date) - dob)

clinic.df$age_at_last_clinic_visit_years <- format(round(as.numeric(clinic.df$age_at_last_clinic_visit_years / 365.25), 1), nsmall = 1)

#create variable that shows max clinic_visit_number
clinic.df <- clinic.df %>% 
  group_by(corp_id) %>% 
  mutate(clinic_visit_number_max = max(clinic_visit_number))


#create a time_since and time_since_max variable
range(clinic.df$adm_date)
placeholder_date <- as.Date("2022-01-01")


clinic.df <- clinic.df %>% 
  group_by(corp_id) %>% 
  arrange(corp_id, adm_date) %>% 
  mutate(time_between_clinic_visits_days = ifelse(clinic_visit_number_max == 1, placeholder_date - adm_date, 
                                           ifelse(clinic_visit_number < clinic_visit_number_max, adm_date-lag(adm_date), placeholder_date - adm_date)))

clinic.df <- clinic.df %>% 
  group_by(corp_id) %>% 
  mutate(time_between_clinic_visits_days_max = max(time_between_clinic_visits_days, na.rm = TRUE))

clinic.df$time_between_clinic_visits_days_max <- as.numeric(clinic.df$time_between_clinic_visits_days_max)

df.2 = left_join(df.1, clinic.df)


#create ED visit number
ed.df <- df.2 %>% 
  group_by(corp_id) %>% 
  filter(pat_class == 'ED') %>% 
  arrange(corp_id, adm_date) %>% 
  mutate(ED_visit_number = seq_along(adm_date)) 

df.3 = left_join(df.2, ed.df)

check.df = df.3 %>% select(corp_id, adm_date, pat_class, age_at_visit_years, visit_number, time_between_visits_days, time_between_visits_days_max,
                           clinic_visit_number, time_between_clinic_visits_days, time_between_clinic_visits_days_max, ED_visit_number)

df.3 = df.3 %>% ungroup()
save(df.3, file = 'Output/df.3.rda')


rm(list = ls())
setwd("C:/Users/pegeorg/OneDrive - Emory University/Papers and Projects/SCD Access to care/")
load('Output/df.3.rda')


##  now looking at TCDs             -------------------------------------------------------------------------

df.3$age_at_last_clinic_visit_years = as.numeric(df.3$age_at_last_clinic_visit_years)

TCD.df = df.3 %>% select(1, age_at_last_clinic_visit_years, 51:67)

duplicate_rows <- TCD.df %>%
  select(corp_id, TCD_1, TCD_10) %>%
  duplicated()

TCD.df = TCD.df[!duplicate_rows,]

for (i in 3:17) {
  col_name <- paste("TCD_time_diff_", i-1, sep="")
  TCD.df[col_name] <- TCD.df[i+1] - TCD.df[i]
}


# RESTART HERE --- need to create variables for total number of TCDs, time between TCDs, etc etc 

TCD.df$TCD_count <- rowSums(!is.na(TCD.df[,3:19]))

table(TCD.df$TCD_count)


TCD.df[, 20:35] <- lapply(TCD.df[, 20:35], as.numeric)

TCD.df$max_time_between_TCD <- apply(TCD.df[, 20:34], 1, max, na.rm = TRUE)
TCD.df$max_time_between_TCD[TCD.df$max_time_between_TCD == -Inf] <- NA

TCD.df = TCD.df %>% 
  mutate(adequate_TCD = if_else(TCD_count/(age_at_last_clinic_visit_years - 2) < 0.67 | 
           max_time_between_TCD > 365*1.6, 0, 1))

TCD.df$adequate_TCD <- ifelse(TCD.df$age_at_last_clinic_visit_years < 2.5, NA, TCD.df$adequate_TCD)
TCD.df$adequate_TCD <- ifelse(TCD.df$age_at_last_clinic_visit_years < 3 & TCD.df$age_at_last_clinic_visit_years > 2 
                                & TCD.df$TCD_count >=1, 1, TCD.df$adequate_TCD)

str(TCD.df)

table(TCD.df$adequate_TCD)

df.4 = left_join(df.3, TCD.df)

save(df.4, file = 'Output/df.4.rda')


