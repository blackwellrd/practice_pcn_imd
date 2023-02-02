# =========================================================================== #
#                                                                             #
# Title:        Practice and PCN Level IMD 2019 Converter                     #
# Filename:     practice_pcn_imd.R                                            #
# Description:  Convert Lower-layer Super Output Area (LSOA) Indices of       #
#               Multiple Deprivation (IMD) 2019 data into Practice and PCN    #
#               level using GP Practice Registration by LSOA and PCN          #
#               Membership data                                               #
# Author:       Richard Blackwell                                             #
# Email:        richard.blackwell@swahsn.com                                  #
# Date:         2023-02-02                                                    #
#                                                                             #
# =========================================================================== #

# Load libraries
# ==============

if(!require(tidyverse)){
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(readxl)){
  install.packages('readxl')
  library(readxl)
}
  
# Load data sources and simplify
# ==============================

# IMD data - import data, select relevant fields and rename fields
df_imd <- read.csv('./data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>%
  select(1, 5, 53) %>% 
  rename_with(.fn = function(X){c('lsoa11cd', 'imd_score', 'imd_popn')})

# GP practice registration data - import data, select relevant fields and rename fields
df_practice_popn <- read.csv('./data/gp-reg-pat-prac-lsoa-all.csv') %>%
  select(3, 5, 7) %>%
  rename_with(.fn = function(x){c('practice_code', 'lsoa11cd', 'reg_popn')})

# Current English GP Practices - import data, select relevant fields, filter data and rename fields
df_practice <- read.csv('./data/epraccur.csv', header = FALSE) %>%
  select(1, 2, 10, 13, 15, 26) %>%
  rename_with(.fn = function(x){c('practice_code', 'practice_name', 'postcode', 'status_code', 'subicb_code', 'prescribing_code')}) %>%
  filter(status_code == 'A' & prescribing_code == 4) %>%
  select(-c('status_code', 'prescribing_code'))

# Current English PCNs - import data, select relevant fields, filter data and rename fields
df_pcn <- read_excel(path = './data/ePCN.xlsx', sheet = 'PCNDetails') %>%
  select(1, 2, 3, 6, 12) %>%
  rename_with(.fn = function(x){c('pcn_code', 'pcn_name', 'subicb_code', 'close_date', 'postcode')}) %>%
  filter(is.na(close_date)) %>%
  select(-close_date)
df_pcn_member <- read_excel(path = './data/ePCN.xlsx', sheet = 'PCN Core Partner Details') %>%
  select(1, 5, 10) %>%
  rename_with(.fn = function(x){c('practice_code', 'pcn_code', 'depart_date')}) %>%
  filter(is.na(depart_date)) %>%
  select(-depart_date)

# Create PCN LSOA popn data
# -------------------------

# Join the practice registration to the PCN membership data and group back up into PCN level
# NB: Practices not registered as a member of a PCN will be assigned the PCN code 'U', unallocated
df_pcn_popn <- df_practice_popn %>% 
  left_join(
    df_pcn_member,
    by = c('practice_code' = 'practice_code')
  ) %>%
  replace_na(list(pcn_code = 'U')) %>%
  group_by(pcn_code, lsoa11cd) %>%
  summarise(reg_popn = sum(reg_popn), .groups = 'keep') %>%
  ungroup()

# Convert LSOA IMD data to Practice Level 
# ---------------------------------------

# Perform inner join to IMD data which will remove populations from unknown 
# LSOAs 'NO2011' and from Welsh LSOAs 'W*' and calculate the population 
# weighted IMD score
df_practice_imd <- df_practice_popn %>% 
  inner_join(
    df_imd,
    by = c('lsoa11cd' = 'lsoa11cd')
  ) %>% 
  mutate(popn_weighted_imd_score = imd_score * reg_popn)

# Self join the practice IMD data to get practice overall population
df_practice_imd <- df_practice_imd %>% 
  group_by(practice_code) %>% 
  summarise(
    practice_popn = sum(reg_popn),
    popn_weighted_imd_score = sum(popn_weighted_imd_score),
    .groups ='keep'
  ) %>%
  ungroup() %>%
  transmute(practice_code, imd_score = popn_weighted_imd_score / practice_popn) %>%
  mutate(imd_decile = ntile(desc(imd_score), 10))

# Add in the practice name and postcode from the current English GP practice data
df_practice_imd <- df_practice_imd %>% 
  left_join(
    df_practice,
    by = c('practice_code' = 'practice_code')
  ) %>%
  select(
    practice_code,
    practice_name,
    postcode,
    subicb_code,
    imd_score,
    imd_decile
  ) %>%
  replace_na(list(practice_code = 'Unknown', postcode = 'Unknown', subicb_code = 'UNK'))

# Convert LSOA IMD data to PCN Level 
# ----------------------------------

# Perform inner join to IMD data which will remove populations from unknown 
# LSOAs 'NO2011' and from Welsh LSOAs 'W*' and calculate the population 
# weighted IMD score
df_pcn_imd <- df_pcn_popn %>% 
  inner_join(
    df_imd,
    by = c('lsoa11cd' = 'lsoa11cd')
  ) %>% 
  mutate(popn_weighted_imd_score = imd_score * reg_popn)

# Self join the PCN IMD data to get PCN overall population
df_pcn_imd <- df_pcn_imd %>% 
  group_by(pcn_code) %>% 
  summarise(
    pcn_popn = sum(reg_popn),
    popn_weighted_imd_score = sum(popn_weighted_imd_score),
    .groups ='keep'
  ) %>%
  ungroup() %>%
  transmute(pcn_code, imd_score = popn_weighted_imd_score / pcn_popn) %>%
  mutate(imd_decile = ntile(desc(imd_score), 10))

# Add in the pcn name and postcode and sub-ICB code from the current English PCN data
df_pcn_imd <- df_pcn_imd %>% 
  left_join(
    df_pcn,
    by = c('pcn_code' = 'pcn_code')
  ) %>%
  select(
    pcn_code,
    pcn_name,
    postcode,
    subicb_code,
    imd_score,
    imd_decile
  ) %>%
  replace_na(list(pcn_code = 'Unknown', postcode = 'Unknown', subicb_code = 'UNK'))

dir.create('./outputs', showWarnings = FALSE, recursive = TRUE)
write.csv(df_practice_imd, './outputs/practice_imd.csv', row.names = FALSE)
write.csv(df_pcn_imd, './outputs/pcn_imd.csv', row.names = FALSE)