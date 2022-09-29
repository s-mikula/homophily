#### Libraries ####

library(dtplyr)
library(tidyverse)
library(fixest)
library(broom)
library(modelsummary)
library(Formula)

# R version:

# platform       x86_64-pc-linux-gnu         
# arch           x86_64                      
# os             linux-gnu                   
# system         x86_64, linux-gnu           
# status                                     
# major          4                           
# minor          1.1                         
# year           2021                        
# month          08                          
# day            10                          
# svn rev        80725                       
# language       R                           
# version.string R version 4.1.1 (2021-08-10)
# nickname       Kick Things 

# fixest: version 0.10.0
# modelsummary: version 0.9.2
# broom: version 0.7.9

#
# Please, note, that the estimation is quite demanding in terms of RAM.
# We recommend to use computer equipped with 256GB RAM. 
#


#### Data ####

# Remove all objects
rm(list = ls())

options("modelsummary_format_numeric_latex" = "plain")

time_start <- Sys.time()

# Load data (available from Zenodo repository: DOI number 10.5281/zenodo.7070556)
load("replication_data_20220910.RData")

# Tables
# rdata...main data table used in regressions
# homo_occupation...auxiliary data table used in descriptive stats
# municipality_ages...auxiliary data table used in descriptive stats

##### Variables #####

# Outcome:
# pref...number of preferential votes

# Variables of interest:
# homo_municipality...indicator variable for candidate running in municipality of his/her residence
# homo_(education/occupation/age/gender)...percentage of population sharing the characteristic of the candidate

# Other variables:
# KOD_OBEC...municipality ID in CISOB classification
# total_fe...ID of candidate-election pair
# maxPORCISLO...number of candidates on the ballot
# cluster_ID...ID for error term clustering
# POC_HLASU...total number of votes cast for the party in the given municipality
# small_municipality...indicator variable for a municipality with a population below the median (defined separately 
# for each year and constituency)
# year...election year (election ID)
# VOLKRAJ...constituency ID
# PORCISLO...position of the candidate on the ballot
# VEK...age
# agecat...age category
# MANDAT...indicator variable for elected candidates
# tertiary_educ...indicator variable for candidates with tertiary education
# gender...male/female
# ger_share...indicator variable for a municipality being dominated by ethnic Germans in 1930


#### Descriptives ####

##### Table 1: Demographic characteristics #####

tab1_col1 <- rdata %>% 
  distinct(total_fe, .keep_all = TRUE) %>% 
  mutate(
    male = gender == "male"
  ) %>%
  select(VEK,male,tertiary_educ,ISCOcand) %>% 
  mutate(
    ISCO_0 = ISCOcand == "0",
    ISCO_1 = ISCOcand == "1",
    ISCO_2 = ISCOcand == "2",
    ISCO_3 = ISCOcand == "3",
    ISCO_4 = ISCOcand == "4",
    ISCO_5 = ISCOcand == "5",
    ISCO_6 = ISCOcand == "6",
    ISCO_7 = ISCOcand == "7",
    ISCO_8 = ISCOcand == "8",
    ISCO_9 = ISCOcand == "9",
    ISCO_Inactive = ISCOcand == "Inactive",
    ISCO_NotClassified = ISCOcand == "NotClassified"
  ) %>% 
  select(-ISCOcand) %>% 
  mutate(
    across(
      where(
        is.logical
      ),
      as.integer
    )
  ) %>% 
  pivot_longer(everything()) %>% 
  group_by(
    name
  ) %>% 
  summarise(
    smean = mean(value, na.rm = TRUE),
    ssd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  # To %
  mutate(
    smean = ifelse(name != "VEK", 100*smean, smean),
    ssd = ifelse(name != "VEK", 100*ssd, ssd)
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(-name, names_to = "stat", values_to = "candidates")


tab1_col2 <- homo_occupation %>% 
  group_by(ISCOcand) %>% 
  summarise(
    smean = mean(homo_occupation, na.rm = TRUE),
    ssd = sd(homo_occupation, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    name = str_c("ISCO_",ISCOcand)
  ) %>% 
  select(-ISCOcand) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(-name, names_to = "stat", values_to = "all_munic")

small_ids <- rdata %>% 
  filter(small_municipality) %>% 
  distinct(KOD_OBEC) %>% 
  pull(KOD_OBEC)

tab1_col3 <- homo_occupation %>% 
  filter(KOD_OBEC %in% small_ids) %>% 
  group_by(ISCOcand) %>% 
  summarise(
    smean = mean(homo_occupation, na.rm = TRUE),
    ssd = sd(homo_occupation, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    name = str_c("ISCO_",ISCOcand)
  ) %>% 
  select(-ISCOcand) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(-name, names_to = "stat", values_to = "small_munic")

munic_char <- left_join(
  rdata %>% 
    filter(gender == "male") %>% 
    distinct(census_year,KOD_OBEC,.keep_all = TRUE) %>% 
    select(KOD_OBEC,census_year,homo_gender),
  rdata %>% 
    filter(tertiary_educ) %>% 
    distinct(census_year,KOD_OBEC,.keep_all = TRUE) %>% 
    select(KOD_OBEC,census_year,homo_education)
)

munic_all <- munic_char %>% 
  select(starts_with("homo")) %>% 
  pivot_longer(
    everything()
  ) %>% 
  group_by(
    name
  ) %>% 
  summarise(
    smean = mean(value, na.rm = TRUE),
    ssd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(-name, names_to = "stat", values_to = "all_munic") %>% 
  mutate(
    name = case_when(
      str_detect(name,"education") ~ "tertiary_educ",
      str_detect(name,"gender") ~ "male"
    )
  )

munic_all <- rdata %>% 
  distinct(KOD_OBEC,year) %>% 
  left_join(.,municipalities_age) %>% 
  summarise(
    smean = mean(AGEmean, na.rm = TRUE),
    ssd = sd(AGEmean, na.rm = TRUE)
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(everything(),names_to = "stat", values_to = "all_munic") %>% 
  mutate(name = "VEK") %>% 
  bind_rows(munic_all,.)

munic_small <- munic_char %>% 
  filter(KOD_OBEC %in% small_ids) %>% 
  select(starts_with("homo")) %>% 
  pivot_longer(
    everything()
  ) %>% 
  group_by(
    name
  ) %>% 
  summarise(
    smean = mean(value, na.rm = TRUE),
    ssd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(-name, names_to = "stat", values_to = "small_munic") %>% 
  mutate(
    name = case_when(
      str_detect(name,"education") ~ "tertiary_educ",
      str_detect(name,"gender") ~ "male"
    )
  )

munic_small <- rdata %>% 
  distinct(KOD_OBEC,year) %>% 
  filter(KOD_OBEC %in% small_ids) %>% 
  left_join(.,municipalities_age) %>% 
  summarise(
    smean = mean(AGEmean, na.rm = TRUE),
    ssd = sd(AGEmean, na.rm = TRUE)
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(everything(),names_to = "stat", values_to = "small_munic") %>% 
  mutate(name = "VEK") %>% 
  bind_rows(munic_small,.)


### Print Table 1:

table1 <- list(
  tab1_col1 %>% filter(name != "ISCO_Inactive"),
  bind_rows(tab1_col2,munic_all),
  bind_rows(tab1_col3,munic_small)
) %>% 
  reduce(
    full_join
  ) %>% 
  mutate(
    name = factor(
      name,
      levels = c(
        "VEK","male","tertiary_educ",
        "ISCO_0","ISCO_1","ISCO_2",
        "ISCO_3","ISCO_4","ISCO_5",
        "ISCO_6","ISCO_7","ISCO_8",
        "ISCO_9","ISCO_NotClassified"
      ),
      labels = c(
        "Age (years)","Male (%)","Tertiary educated (%)",
        "Occupation (ISCO 0, =1)",
        "Occupation (ISCO 1, =1)",
        "Occupation (ISCO 2, =1)",
        "Occupation (ISCO 3, =1)",
        "Occupation (ISCO 4, =1)",
        "Occupation (ISCO 5, =1)",
        "Occupation (ISCO 6, =1)",
        "Occupation (ISCO 7, =1)",
        "Occupation (ISCO 8, =1)",
        "Occupation (ISCO 9, =1)",
        "Occupation (Unclassified in ISCO, =1)"
      )
    )
  ) %>% 
  arrange(name,stat) %>% 
  mutate(
    name = as.character(name),
    name = ifelse(stat == "smean",name,"")
  ) %>% 
  select(-stat)

print("----------------- Table 1 -----------------")
print(table1, n = 30)


##### Table 2: Similarity #####

# All municipalities
tab2_col1 <- rdata %>% 
  select(starts_with("homo")) %>% 
  mutate(
    across(
      where(
        is.logical
      ),
      as.double
    )
  ) %>% 
  pivot_longer(everything()) %>% 
  group_by(
    name
  ) %>% 
  summarise(
    smean = mean(value, na.rm = TRUE),
    ssd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(-name, names_to = "stat", values_to = "all_munic")

# Sub-sample of small municipalities
tab2_col2 <- rdata %>% 
  filter(small_municipality) %>% 
  select(starts_with("homo")) %>% 
  mutate(
    across(
      where(
        is.logical
      ),
      as.double
    )
  ) %>% 
  pivot_longer(everything()) %>% 
  group_by(
    name
  ) %>% 
  summarise(
    smean = mean(value, na.rm = TRUE),
    ssd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      format,
      trim = TRUE,
      digits = 1,
      nsmall = 3,
      scientific = FALSE
    )
  ) %>% 
  mutate(
    ssd = str_c("(",ssd,")")
  ) %>% 
  pivot_longer(-name, names_to = "stat", values_to = "small_munic")

# Complete table
table2 <- full_join(tab2_col1,tab2_col2) %>% 
  mutate(
    name = factor(
      name,
      levels = c(
        "homo_education",
        "homo_occupation",
        "homo_age",
        "homo_gender",
        "homo_municipality"
      ),
      labels = c(
        "Share of population with the same education level as the candidate (%)",
        "Share of population with the same occupation as the candidate (%)",
        "Share of population of the same age as the candidate (%)",
        "Share of population of the same gender as the candidate (%)",
        "Candidate lives in the municipality (=1)"
      )
    )
  ) %>% 
  arrange(name,stat) %>% 
  mutate(
    name = ifelse(stat == "ssd","",as.character(name))
  ) %>% 
  select(-stat)

print("----------------- Table 2 -----------------")
print(table2, n = 20)

#### Regressions ####

print_table <- function(estlist, fname = NULL){
  vrs <- c(
    "homo_education" = "Share of population with the same education level as the candidate (%)",
    "homo_occupation" = "Share of population with the same occupation as the candidate (%)",
    "homo_age" = "Share of population of the same age as the candidate (%)",
    "homo_gender" = "Share of population of the same gender as the candidate (%)",
    "homo_municipalityTRUE" = "Candidate lives in the municipality (=1)",
    "homo_education:position_cat[0,5]" = "[1,5] x var. of interest",
    "homo_education:position_cat(5,10]" = "(5,10] x var. of interest",
    "homo_education:position_cat(10,15]" = "(10,15] x var. of interest",
    "homo_education:position_cat(15,20]" = "(15,20] x var. of interest",
    "homo_education:position_cat(20,25]" = "(20,25] x var. of interest",
    "homo_education:position_cat(25,30]" = "(25,30] x var. of interest",
    "homo_occupation:position_cat[0,5]" = "[1,5] x var. of interest",
    "homo_occupation:position_cat(5,10]" = "(5,10] x var. of interest",
    "homo_occupation:position_cat(10,15]" = "(10,15] x var. of interest",
    "homo_occupation:position_cat(15,20]" = "(15,20] x var. of interest",
    "homo_occupation:position_cat(20,25]" = "(20,25] x var. of interest",
    "homo_occupation:position_cat(25,30]" = "(25,30] x var. of interest",
    "homo_age:position_cat[0,5]" = "[1,5] x var. of interest",
    "homo_age:position_cat(5,10]" = "(5,10] x var. of interest",
    "homo_age:position_cat(10,15]" = "(10,15] x var. of interest",
    "homo_age:position_cat(15,20]" = "(15,20] x var. of interest",
    "homo_age:position_cat(20,25]" = "(20,25] x var. of interest",
    "homo_age:position_cat(25,30]" = "(25,30] x var. of interest",
    "homo_gender:position_cat[0,5]" = "[1,5] x var. of interest",
    "homo_gender:position_cat(5,10]" = "(5,10] x var. of interest",
    "homo_gender:position_cat(10,15]" = "(10,15] x var. of interest",
    "homo_gender:position_cat(15,20]" = "(15,20] x var. of interest",
    "homo_gender:position_cat(20,25]" = "(20,25] x var. of interest",
    "homo_gender:position_cat(25,30]" = "(25,30] x var. of interest",
    "homo_education:position_cat(30,35]" = "(30,35] x var. of interest",
    "homo_education:position_cat(35,40]" = "(35,40] x var. of interest",
    "homo_education:position_cat(40,45]" = "(40,45] x var. of interest",
    "homo_education:position_cat(45,50]" = "(45,50] x var. of interest",
    "homo_education:position_cat(50,55]" = "(50,55] x var. of interest",
    "homo_education:position_cat(55,60]" = "(55,60] x var. of interest",
    "homo_occupation:position_cat(30,35]" = "(30,35] x var. of interest",
    "homo_occupation:position_cat(35,40]" = "(35,40] x var. of interest",
    "homo_occupation:position_cat(40,45]" = "(40,45] x var. of interest",
    "homo_occupation:position_cat(45,50]" = "(45,50] x var. of interest",
    "homo_occupation:position_cat(50,55]" = "(50,55] x var. of interest",
    "homo_occupation:position_cat(55,60]" = "(55,60] x var. of interest",
    "homo_age:position_cat(30,35]" = "(30,35] x var. of interest",
    "homo_age:position_cat(35,40]" = "(35,40] x var. of interest",
    "homo_age:position_cat(40,45]" = "(40,45] x var. of interest",
    "homo_age:position_cat(45,50]" = "(45,50] x var. of interest",
    "homo_age:position_cat(50,55]" = "(50,55] x var. of interest",
    "homo_age:position_cat(55,60]" = "(55,60] x var. of interest",
    "homo_gender:position_cat(30,35]" = "(30,35] x var. of interest",
    "homo_gender:position_cat(35,40]" = "(35,40] x var. of interest",
    "homo_gender:position_cat(40,45]" = "(40,45] x var. of interest",
    "homo_gender:position_cat(45,50]" = "(45,50] x var. of interest",
    "homo_gender:position_cat(50,55]" = "(50,55] x var. of interest",
    "homo_gender:position_cat(55,60]" = "(55,60] x var. of interest",
    "homo_education:tertiary_educFALSE" = "Candidate without tertiary education (=1) x homo variable (%)",
    "homo_education:tertiary_educTRUE" = "Candidate with tertiary education (=1) x homo variable (%)",
    "homo_occupation:ISCOcand0" = "Candidate working in ISCO 0 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand1" = "Candidate working in ISCO 1 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand2" = "Candidate working in ISCO 2 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand3" = "Candidate working in ISCO 3 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand4" = "Candidate working in ISCO 4 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand5" = "Candidate working in ISCO 5 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand6" = "Candidate working in ISCO 6 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand7" = "Candidate working in ISCO 7 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand8" = "Candidate working in ISCO 8 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcand9" = "Candidate working in ISCO 9 occupation (=1) x homo variable (%)",
    "homo_occupation:ISCOcandNotClassified" = "Candidate working in occupation not classified in ISCO (=1) x homo variable (%)",
    "homo_age:agecatage1929" = "Candidate in age category [18,29] (=1) x homo variable (%)",
    "homo_age:agecatage2939" = "Candidate in age category [29,39] (=1) x homo variable (%)",
    "homo_age:agecatage3949" = "Candidate in age category [39,49] (=1) x homo variable (%)",
    "homo_age:agecatage4959" = "Candidate in age category [49,59] (=1) x homo variable (%)",
    "homo_age:agecatage5969" = "Candidate in age category [59,69] (=1) x homo variable (%)",
    "homo_age:agecatage6979" = "Candidate in age category [69,79] (=1) x homo variable (%)",
    "homo_age:agecatage7989" = "Candidate in age category [79,89] (=1) x homo variable (%)",
    "homo_gender:genderfemale" = "Female candidate (=1) x homo variable (%)",
    "homo_gender:gendermale" = "Male candidate (=1) x homo variable (%)",
    "homo_municipality:position_cat[0,5]" = "[1,5] x var. of interest",
    "homo_municipality:position_cat(5,10]" = "(5,10] x var. of interest",
    "homo_municipality:position_cat(10,15]" = "(10,15] x var. of interest",
    "homo_municipality:position_cat(15,20]" = "(15,20] x var. of interest",
    "homo_municipality:position_cat(20,25]" = "(20,25] x var. of interest",
    "homo_municipality:position_cat(25,30]" = "(25,30] x var. of interest",
    "homo_municipality:position_cat(30,35]" = "(30,35] x var. of interest",
    "homo_municipality:position_cat(35,40]" = "(35,40] x var. of interest",
    "homo_municipality:position_cat(40,45]" = "(40,45] x var. of interest",
    "homo_municipality:position_cat(45,50]" = "(45,50] x var. of interest",
    "homo_municipality:position_cat(50,55]" = "(50,55] x var. of interest",
    "homo_municipality:position_cat(55,60]" = "(55,60] x var. of interest"
  )
  
  modelsummary(
    estlist,
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    fmt = function(x) format(x, digits = 1, nsmall = 4, scientific = FALSE, big.mark = ","),
    coef_map = vrs,
    gof_omit = "^(?!Num)",
    output = "data.frame"
  ) %>% 
    as_tibble() %>% 
    mutate(
      rn = row_number(),
      term = ifelse(
        rn %in% seq(from = 1, to = 1000, by = 2),
        term,""
      )
    ) %>% 
    select(term,contains("Model")) %>% 
    print(n=200)
}

##### Formulas #####

modell <- list(
  formula(pref ~ homo_education | total_fe + KOD_OBEC),
  formula(pref ~ homo_occupation | total_fe + KOD_OBEC),
  formula(pref ~ homo_age | total_fe + KOD_OBEC),
  formula(pref ~ homo_gender | total_fe + KOD_OBEC),
  formula(pref ~ homo_municipality | total_fe + KOD_OBEC ),
  formula(pref ~ homo_education + homo_occupation + homo_age + 
            homo_gender + homo_municipality | total_fe + KOD_OBEC)
) %>% map(as.Formula)

modell_lpm <- modell %>% 
  map(update, I(pref>0) ~ .)

modell_position <- list(
  formula(pref ~ homo_education:position_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_occupation:position_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_age:position_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_gender:position_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_municipality:position_cat | total_fe + KOD_OBEC)
) %>% map(as.Formula)

modell_population <- list(
  formula(pref ~ homo_education:population_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_occupation:population_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_age:population_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_gender:population_cat | total_fe + KOD_OBEC),
  formula(pref ~ homo_municipality:population_cat | total_fe + KOD_OBEC)
) %>% map(as.Formula)

modell_municipality <- list(
  formula(pref ~ homo_education | total_fe + KOD_OBEC),
  formula(pref ~ homo_occupation | total_fe + KOD_OBEC),
  formula(pref ~ homo_age | total_fe + KOD_OBEC),
  formula(pref ~ homo_gender | total_fe + KOD_OBEC),
  formula(pref ~ homo_education + homo_occupation + homo_age + 
            homo_gender| total_fe + KOD_OBEC)
) %>% map(as.Formula)

modell_levels <- list(
  formula(pref ~ homo_education:tertiary_educ | total_fe + KOD_OBEC),
  formula(pref ~ homo_occupation:ISCOcand | total_fe + KOD_OBEC),
  formula(pref ~ homo_age:agecat | total_fe + KOD_OBEC),
  formula(pref ~ homo_gender:gender | total_fe + KOD_OBEC)
) %>% map(as.Formula)

modell_ger <- list(
  pref ~ homo_education + homo_education:ger_share | total_fe + KOD_OBEC,
  pref ~ homo_occupation + homo_occupation:ger_share | total_fe + KOD_OBEC,
  pref ~ homo_age + homo_age:ger_share | total_fe + KOD_OBEC,
  pref ~ homo_gender + homo_gender:ger_share | total_fe + KOD_OBEC,
  pref ~ homo_municipality + homo_municipality:ger_share | total_fe + KOD_OBEC
) %>% map(as.Formula)


##### Table 3 ####
estimates_baseline <- modell %>% 
  map(
    feglm,
    data = rdata,
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table 3 ----------------')
print_table(estimates_baseline,"estimates_baseline")

rm(estimates_baseline)

##### Table 4 ####
estimates_small <- modell %>% 
  map(
    feglm,
    data = filter(rdata, small_municipality),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table 4 ----------------')
print_table(estimates_small,"estimates_small")

rm(estimates_small)

##### Table A.2 #####

estimates_baseline_levels <- modell_levels %>% 
  map(
    feglm,
    data = rdata,
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table A.2 ----------------')
print_table(estimates_baseline_levels,"estimates_baseline_levels")

rm(estimates_baseline_levels)

##### Table 5 #####

estimates_small_levels <- modell_levels %>% 
  map(
    feglm,
    data = filter(rdata, small_municipality),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table 5 ----------------')
print_table(estimates_small_levels,"estimates_small_levels")

rm(estimates_small_levels)

##### Table A.6 #####

estimates_baseline_weights <- modell %>% 
  map(
    feglm,
    data = rdata,
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson,
    weights = ~population
  )

print('------------- Table A.6 ----------------')
print_table(estimates_baseline_weights,"estimates_baseline_weights")

rm(estimates_baseline_weights)

##### Table 9 #####

estimates_small_weights <- modell %>% 
  map(
    feglm,
    data = filter(rdata, small_municipality),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC, 
    family = quasipoisson,
    weights = ~population
  )

print('------------- Table 9 ----------------')
print_table(estimates_small_weights,"estimates_small_weights")

rm(estimates_small_weights)

##### Table A.5 #####

rdata_municipality <- rdata %>% 
  group_by(KOD_OBEC,year) %>% 
  mutate(
    any_municipality = any(homo_municipality)
  ) %>% 
  ungroup() %>% 
  filter(!any_municipality)

estimates_baseline_municipality <- 
  modell_municipality %>% 
  map(
    feglm,
    data = rdata_municipality,
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table A.5 ----------------')
print_table(estimates_baseline_municipality,"estimates_baseline_municipality")

rm(estimates_baseline_municipality)

##### Table 8 #####

estimates_small_municipality <- 
  modell_municipality %>% 
  map(
    feglm,
    data = filter(rdata_municipality, small_municipality),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table 8 ----------------')
print_table(estimates_small_municipality,"estimates_small_municipality")

rm(estimates_small_municipality)

##### Table A.3 #####

estimates_position <- modell_position %>% 
  map(
    feglm,
    data = rdata %>% mutate(homo_municipality = as.double(homo_municipality)),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table A.3 ----------------')
print_table(estimates_position,"estimates_position")

rm(estimates_position)

##### Table 6 #####
estimates_small_position <- modell_position %>% 
  map(
    feglm,
    data = filter(rdata, small_municipality) %>% 
      mutate(homo_municipality = as.double(homo_municipality)),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table 6 ----------------')
print_table(estimates_small_position,"estimates_small_position")

rm(estimates_small_position)

##### Table A.1 #####

rdata_bypopulation <- rdata %>%
  distinct(KOD_OBEC,year, .keep_all = TRUE) %>%
  select(KOD_OBEC, year,  population) %>%
  #filter(KOD_OBEC != "554782") %>%
  group_by(year) %>%
  mutate(
    population_cat = cut(population, breaks = quantile(population), include.lowest = TRUE, labels = 1:4)
  ) %>% 
  ungroup() %>% 
  select(-population) %>% 
  left_join(rdata,.) %>% 
  mutate(
    homo_municipality = as.double(homo_municipality)
  )


estimates_baseline_population <- modell_population %>% 
  map(
    feglm,
    data = rdata_bypopulation,
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table A.1 ----------------')
modelsummary(
  estimates_baseline_population,
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  fmt = function(x) format(x, digits = 1, nsmall = 4, scientific = FALSE, big.mark = ","),
  gof_omit = "^(?!Num)",
  output = "data.frame"
) %>% 
  as_tibble() %>% 
  mutate(
    rn = row_number(),
    term = ifelse(
      rn %in% seq(from = 1, to = 1000, by = 2),
      term,""
    )
  ) %>% 
  select(term,contains("Model")) %>% 
  print()

rm(estimates_baseline_population)

##### Table 7 #####

int_mod <- modell_ger %>% 
  map(
    feglm,
    data = filter(rdata, small_municipality) %>% 
      mutate(homo_municipality = as.double(homo_municipality)),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table 7 ----------------')
modelsummary(
  int_mod,
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  fmt = function(x) format(x, digits = 1, nsmall = 4, scientific = FALSE, big.mark = ","),
  gof_omit = "^(?!Num)",
  output = "data.frame"
) %>% 
  as_tibble() %>% 
  mutate(
    rn = row_number(),
    term = ifelse(
      rn %in% seq(from = 1, to = 1000, by = 2),
      term,""
    )
  ) %>% 
  select(term,contains("Model")) %>% 
  print()

rm(int_mod)

##### Table A.4 #####

int_mod <- modell_ger %>% 
  map(
    feglm,
    data = rdata %>% 
      mutate(homo_municipality = as.double(homo_municipality)),
    offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC,
    family = quasipoisson
  )

print('------------- Table A.4 ----------------')
modelsummary(
  int_mod,
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  fmt = function(x) format(x, digits = 1, nsmall = 4, scientific = FALSE, big.mark = ","),
  gof_omit = "^(?!Num)",
  output = "data.frame"
) %>% 
  as_tibble() %>% 
  mutate(
    rn = row_number(),
    term = ifelse(
      rn %in% seq(from = 1, to = 1000, by = 2),
      term,""
    )
  ) %>% 
  select(term,contains("Model")) %>% 
  print()

rm(int_mod)

##### Table 10 #####
estimates_baseline_lpm <- modell_lpm %>% 
  map(
    feols,
    data = rdata,
    #offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC
  )

print('------------- Table 10 ----------------')
print_table(estimates_baseline_lpm,"estimates_baseline_lpm")

rm(estimates_baseline_lpm)

##### Table A.7 #####

estimates_small_lpm <- modell_lpm %>% 
  map(
    feols,
    data = filter(rdata, small_municipality),
    #offset = ~log(POC_HLASU),
    cluster = ~cluster_ID + KOD_OBEC
  )

print('------------- Table A.7 ----------------')
print_table(estimates_small_lpm,"estimates_small_lpm")

rm(estimates_small_lpm)

##### Regression in text: Number of votes #####

agg_rdata <- rdata %>% 
  group_by(year,KOD_OBEC,VOLKRAJ,KSTRANA) %>% 
  summarise(
    POC_HLASU = max(POC_HLASU),
    homo_municipality = any(homo_municipality),
    small_municipality = first(small_municipality),
    population = max(population),
    .groups = "drop"
  ) %>% 
  mutate(
    total_fe = str_c(year,"_",VOLKRAJ,"_",KSTRANA)
  )

agg_large <- feglm(
  POC_HLASU ~ homo_municipality | total_fe + KOD_OBEC,
  offset = ~log(population),
  cluster = ~total_fe + KOD_OBEC,
  data = filter(agg_rdata, !small_municipality),
  family = quasipoisson
)

agg_small <- feglm(
  POC_HLASU ~ homo_municipality | total_fe + KOD_OBEC,
  offset = ~log(population),
  cluster = ~total_fe + KOD_OBEC,
  data = filter(agg_rdata, small_municipality),
  family = quasipoisson
)

etable(list(agg_large,agg_small)) %>% print()

rm(agg_small,agg_large)

################################################################################
time_end <- Sys.time()
total_duration <- time_end-time_start
print(total_duration)