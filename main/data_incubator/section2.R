library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Data Incubator")

#==== load data ====
data2016 <- import("data/PartD_Prescriber_PUF_NPI_16.txt", colClasses = c("npi" = "character"))
data2017 <- import("data/PartD_Prescriber_PUF_NPI_17.txt", colClasses = c("npi" = "character"))


#==== find answers =====
# In 2017, what was the average number of beneficiaries per provider? Due to the suppression of data for those with few beneficiaries, we can only include those with more than 10 beneficiaries.
data2017 %>% 
  filter(bene_count > 10) %>% 
  summarise(mean(bene_count))

# For each provider, estimate the length of the average prescription from the total_day_supply and total_claim_count. What is the median, in days, of the distribution of this value across all providers?
data2017 %>% 
  summarise(median(total_day_supply / total_claim_count))

# Work out for each Specialty the fraction of drug claims that are for brand-name drugs. Include only providers for whom the relevant information has not been suppressed, and consider only specialties with at least 1000 total claims. What is the standard deviation of these fractions?
data2017 %>% 
  filter(
    !is.na(specialty_description),
    !is.na(brand_claim_count),
    !is.na(total_claim_count)
  ) %>% 
  group_by(specialty_description) %>% 
  summarise(
    brand_claim_count = sum(brand_claim_count),
    total_claim_count = sum(total_claim_count),
    brand_claim_percent = sum(brand_claim_count) / sum(total_claim_count)
  ) %>% 
  filter(total_claim_count > 1000) %>% 
  summarise(
    sd_brand_claim_percent = sd(brand_claim_percent)
  )
  
# Find the ratio of beneficiaries with opioid prescriptions to beneficiaries with antibiotics prescriptions in each state. Assume that each beneficiary attends only a single provider. What is the difference between the largest and smallest ratios?
data2017 %>% 
  group_by(nppes_provider_state) %>% 
  summarise(
    opiod_to_antibiotic = sum(opioid_bene_count, na.rm = TRUE) / sum(antibiotic_bene_count, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  summarise(
    diff = max(opiod_to_antibiotic) - min(opiod_to_antibiotic)
  )

# For each provider where the relevant columns are not suppressed, work out the fraction of claims for beneficiaries age 65 and older, as well as the fraction of claims for beneficiaries with a low-income subsidy. What is the Pearson correlation coefficient between these values?
data2017 %>% 
  filter(
    !is.na(total_claim_count),
    !is.na(total_claim_count_ge65),
    !is.na(lis_claim_count)
  ) %>% 
  mutate(
    ge65_percent = total_claim_count_ge65 / total_claim_count,
    lis_percent = lis_claim_count / total_claim_count
  ) %>%
  summarise(
    corr = cor(ge65_percent, lis_percent)
  )

# Let's find which states have surprisingly high supply of opioids, conditioned on specialty. Work out the average length of an opioid prescription for each provider. For each (state, specialty) pair with at least 100 providers, calculate the average of this value across all providers. Then find the ratio of this value to an equivalent quantity calculated from providers in each specialty across all states. What is the largest such ratio?
state_statistics <- data2017 %>% 
  filter(
    !is.na(opioid_day_supply),
    !is.na(opioid_claim_count),
    opioid_claim_count > 0
  ) %>% 
  mutate(average_length_opioid = opioid_day_supply / opioid_claim_count) %>% 
  group_by(nppes_provider_state, specialty_description) %>% 
  mutate(
    count = n()
  ) %>% 
  filter(count > 100) %>% 
  summarise(
    state_average_length_opioid = mean(average_length_opioid)
  )

national_statistics <- data2017 %>% 
  filter(
    !is.na(opioid_day_supply),
    !is.na(opioid_claim_count),
    opioid_claim_count > 0
  ) %>% 
  mutate(average_length_opioid = opioid_day_supply / opioid_claim_count) %>% 
  group_by(specialty_description) %>% 
  mutate(
    count = n()
  ) %>% 
  filter(count > 100) %>% 
  summarise(
    national_average_length_opioid = mean(average_length_opioid)
  )

state_statistics %>% 
  left_join(national_statistics, by = "specialty_description") %>% 
  ungroup() %>% 
  mutate(
    state_national_ratio = state_average_length_opioid / national_average_length_opioid
  ) %>% 
  filter(state_national_ratio == max(state_national_ratio))

# For each provider for whom the information is not suppressed, figure out the average cost per day of prescriptions in both 2016 and 2017. Use this to estimate the inflation rate for daily prescription costs per provider. What is the average inflation rate across all providers?
data2016 %>% 
  filter(
    !is.na(total_drug_cost),
    !is.na(total_day_supply)
  ) %>% 
  transmute(
    npi = npi,
    average_cost_2016 = total_drug_cost / total_day_supply
  ) %>% 
  inner_join(
    data2017 %>% 
      filter(
        !is.na(total_drug_cost),
        !is.na(total_day_supply)
      ) %>% 
      transmute(
        npi = npi,
        average_cost_2017 = total_drug_cost / total_day_supply
      ),
    by = "npi"
  ) %>% 
  mutate(
    inflation_rate = average_cost_2017 / average_cost_2016
  ) %>% 
  summarise(
    inflation_rate = mean(inflation_rate)
  )

# Consider all providers with a defined specialty in both years. Find the fraction of providers who left each specialty between 2016 and 2017. What is the largest such fraction, when considering specialties with at least 1000 proviers in 2016? Note that some specialties have a fraction of 1 due to specialty name changes between 2016 and 2017; disregard these specialties in calculating your answer.
data2016 %>% 
  select(npi, specialty_description_2016 = specialty_description) %>% 
  inner_join(
    data2017 %>% 
      select(npi, specialty_description_2017 = specialty_description),
    by = "npi"
  ) %>% 
  group_by(specialty_description_2016) %>% 
  mutate(
    count_2016 = n()
  ) %>% 
  filter(count_2016 > 1000) %>% 
  summarise(
    change_ratio = sum(specialty_description_2016 != specialty_description_2017) / n()
  ) %>% 
  filter(
    change_ratio != 1
  ) %>% 
  filter(
    change_ratio == max(change_ratio)
  )
