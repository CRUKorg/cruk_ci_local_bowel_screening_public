#==============================================================================#
#    Data preparation for GP bowel screening tool      
#==============================================================================#

# Created by Martina Slapkova, Amar Ahmed, Matt Grant & Ofure Eren

# Stage 1 of 2 in the pipeline: raw data in, one clean row-per-practice table out.
# Stage 2 (02_fit_model.qmd) picks up this script's output and fits the statistical model.

# For where each raw file in data/raw/ actually comes from, and how to refresh postcodes.csv from a fresh ONS Postcode Directory download, see docs/SOURCES.md

library(tidyverse) # loads packages needed
rm(list=ls()) # clears memory and removes leftover objects

## 1. IMPORT RAW DATA ----------------------------------------------------------

### Screening data for all GP practice areas ----
bowelscreening <- read_csv("data/raw/bowelscreening.csv")
names(bowelscreening)<-gsub("\\s","",names(bowelscreening)) #tidy up column names

### Population age dist ----
PopulationAgeDistribution <- read_csv("data/raw/gp_popdist.csv")

### Deprivation data ----
gp_dep <- read_csv("data/raw/gp_dep.csv")
names(gp_dep)<-gsub("\\s",".",names(gp_dep)) #tidy up column names

### Ethnicity data ----
ethnicity_msoa <- read_csv("data/raw/ethnicity_msoa.csv")
ethnicity_msoa <- ethnicity_msoa %>% filter(grepl("^E02", mnemonic)) # England MSOA

### Address list ----
gp_address <- read_csv("data/raw/gp_address.csv", col_names = FALSE, col_types = cols(.default = "c")) %>%
  distinct(X1, .keep_all = TRUE) %>%
  mutate(Address = apply(across(X5:X9), 1, function(x) str_to_title(paste(x[!is.na(x) & x != ""], collapse = ", ")))) %>%
  select(OrgCode = X1, Address, Postcode = X10, Commissioner = X15)

### Postcode list ----
ons_postcode <- 
  read_csv("data/raw/postcodes.csv") %>%
  rename_all(function(x) gsub(" ", "", x))

### Rural/urban classifications ----

# ru21ind holds the ONS Rural Urban Classification code for each postcode's area (currently the 2021 six-category system - see docs/SOURCES.md)
# ruc21desc_table just maps each code to a plain English label, e.g "UN1" -> "Urban: Nearer to a major town or city".

ruc21desc_table <- 
  read_csv("data/raw/ru21ind_desc.csv") %>%
  rename_all(function(x) gsub(" ", "", x))

### PCN to ICB lookup table ----

PCN_ICB_lookup <- read_csv("data/raw/pcn_core_partner_details.csv",
                                   col_names = FALSE, col_types = cols(.default = 'c')) %>%
  {stopifnot(ncol(.) == 11); .} %>%
  rename(OrgCode = X1,
         OrgName = X2,
         SubICBCode = X3,
         SubICBName = X4,
         PCNCode = X5,
         PCNName = X6,
         PCN_SubICBCode = X7,
         PCN_SubICBName = X8,
         StartDate = X9,
         EndDate = X10,
         GP_PCN_shareSubICB = X11) %>%
  mutate(ICB = str_split_i(SubICBName, " - ", 1)) #Extract ICB name from sub-ICB name

#-------------------------------------------------------------------------------
# A note on how these tables join together:
#
# Almost every raw file here is already published at GP-practice level, so most joins below
# are matched on OrgCode/OrgName, no bridging needed (deprivation, screening, male proportion, 
# address, PCN/ICB grouping all work this way).
#
# Ethnicity is the only exception. It is published at MSOA level, so there's no practice-level 
# ethnicity figure to join on directly. Postcode is the only thing that connects the two, so 
# section 6.2 builds a two-step bridge instead: (via its postcode) -> MSOA -> ethnicity figure.
# This also applies for the rural/urban code for section 7 since postcodes.csv holds both
# columns together.
#-------------------------------------------------------------------------------



## 2.  BUILD AGE AND DEPRIVATION PROFILE ---------------------------------------


### 2.1 Age distribution ----
#tidy columns
PopulationAgeDistribution <- PopulationAgeDistribution %>%
  rename(OrgCode = ORG_CODE, 
         Age = AGE_GROUP_5, 
         Value = NUMBER_OF_PATIENTS)

#filter down to just gp, gp code, age band and population for age band 
agedist <- PopulationAgeDistribution %>% 
  filter(ORG_TYPE == "GP", SEX != "ALL") %>%
  group_by(OrgCode, Age) %>%
  summarise(value = sum(Value)) %>% 
  select(OrgCode, Age, value)

#move age band and totals to columns
agedist_spread <- agedist %>% 
  pivot_wider(names_from = Age, values_from = value)

# creating screening-specific age group and total (60-74 for bowel screening)

agedist_spread <- agedist_spread %>% 
  rowwise() %>% 
  mutate(age60_74 = sum(c(`60_64`, `65_69`, `70_74`))) %>%
  rename(total = "ALL") %>% ungroup()

agedist_spread <- agedist_spread %>% 
  mutate(prop_6074 = age60_74 / total)

#select only necessary columns
gp_prop_age <- agedist_spread %>% 
  select(`OrgCode`, total, age60_74, prop_6074)

rm(agedist, agedist_spread)


### 2.2 Deprivation ----
gp_dep2 <- gp_dep %>% 
  filter(Area.Type =="GPs", Time.period =="2025")  %>% #filter to GPs and 2025 deprivation
  select(Area.Code, Area.Name, Sex, Age, Time.period, Value, Count, 
         Denominator, Recent.Trend, Time.period.range) %>%
  rename("Value_dep" = Value,
         OrgCode = Area.Code,
         OrgName = Area.Name)

#combine with age distribution
age_dep <- 
  left_join(gp_dep2, gp_prop_age, by = "OrgCode") %>% 
  select(OrgCode, Value_dep, age60_74, prop_6074, total) 

rm(gp_dep, gp_dep2)



## 3) ADD BOWEL SCREENING DATA -------------------------------------------------
# deprivation: persons, all ages, timeperiod = 2025
# age: persons, 60+, timeperiod = 2025
# screening: persons, 60-74, timeperiod = 2024/25

bowelscreening_GP <- bowelscreening %>% 
  filter(AreaType == "GPs", Timeperiod == "2024/25") %>%
  select(ParentCode:AreaName, Value:UpperCI95.0limit, Count:Denominator) %>%
  rename("NumberScreened" = Count, 
         "NumberEligible" = Denominator, 
         "pct_coverage" = Value,
         OrgCode = AreaCode,
         OrgName = AreaName)

#join screening info to age and deprivation info
bowelscreen_demog <- 
  left_join(bowelscreening_GP, age_dep, by = "OrgCode") %>%
  rename("IMD" = Value_dep, 
         "GPlistsize" = total) %>% 
  select(OrgCode, OrgName, pct_coverage:GPlistsize)

rm(bowelscreening, bowelscreening_GP, age_dep)



## 4) CALCULATE MALE PROPORTION OF THE ELIGIBLE POPULATION ---------------------
#Proportion of males (taken from age distribution source)
male_popdist <- PopulationAgeDistribution %>%
  filter(ORG_TYPE == "GP", SEX == "MALE") %>%
  select(`OrgCode`, SEX, Age, Value) %>%
  pivot_wider(names_from = Age, values_from = Value) %>% 
  rowwise() %>% 
  mutate(M_age60_74 = sum(c(`60_64`, `65_69`, `70_74`))) %>%
  rename(M_total = "ALL") %>% ungroup() %>%
  select(`OrgCode`, SEX, M_age60_74, M_total)

#Get proportion of males within the screening age only 
male_popdist <- left_join(male_popdist, gp_prop_age, 
                          by = "OrgCode") %>%
  select(OrgCode:M_age60_74, age60_74) %>%
  mutate(Prop_M_6074 = M_age60_74/age60_74)

#join male info to screening/age/dep dataset
gp_demog_eligible <- left_join(bowelscreen_demog, male_popdist, by = "OrgCode") %>%
  rename("age60_74" = age60_74.x) %>%
  select(-age60_74.y)



## 5) ADD PRACTICE ADDRESS -----------------------------------------------------
# gp_address is the only file linking a practice's identity (OrgCode) to its physical
# postcode, so everything geographic downstream depends on it

gp_demog_eligiblelist <- left_join(gp_demog_eligible, gp_address, by = "OrgCode")

rm(bowelscreen_demog, gp_demog_eligible, gp_address)




## 6) ADD ETHNICITY DATA  ------------------------------------------------------


### 6.1 Calculate ethnicity percentage ----
ethnicity_msoa_select <- ethnicity_msoa %>%
  rename(
    "msoa21name" = `2021 super output area - middle layer`, 
    "msoa21" = mnemonic, 
    "ethnic_denom" = `Total: All usual residents`,
    "ethnic_whitebrit" = `White: English, Welsh, Scottish, Northern Irish or British`
  ) %>%
  mutate(ethnic_whitebrit_perc=(ethnic_whitebrit/ethnic_denom)*100) %>%
  select(msoa21name, msoa21, ethnic_denom, ethnic_whitebrit, ethnic_whitebrit_perc)

rm(ethnicity_msoa)


### 6.2 Link to postcode / MSOA ----
# Ethnicity data is published at MSOA level, there's no practice code to join on directly
# so practice -> postcode -> MSOA -> ethnicity, using postcodes.csv (built from the 
# ONS Postcode Directory) as the connector
 
#add msoa to data set
dataset_msoa <- left_join(gp_demog_eligiblelist, ons_postcode, by = "Postcode")

#add ethnicity to data set
dataset_all <- left_join(dataset_msoa, ethnicity_msoa_select, by = "msoa21") %>%
  rename("listsize2020" = "GPlistsize") 



## 7) ADD RURAL/URBAN CLASSIFICATION  ------------------------------------------
# ru21ind was already attached above in 6.2 via the postcodes.csv
# this attaches matching plain-English from ru21ind_desc.csv
dataset_all <- left_join(dataset_all, ruc21desc_table, by = "ru21ind") 

rm(ons_postcode, gp_demog_eligiblelist,
   dataset_msoa, ethnicity_msoa_select, ruc21desc_table)
rm(gp_prop_age, male_popdist, PopulationAgeDistribution)


## 8) ADD PCN/ICB GROUPING -----------------------------------------------------
PCN_cutoff <- as.Date("2025-03-31") # end of 2024/25, matching the March 2025 GP registrations
PCN_all <- PCN_ICB_lookup %>%
  select(OrgCode, OrgName, ICB, SubICBCode, SubICBName, PCNCode, PCNName, StartDate, EndDate) %>%
  mutate(StartDate = as.Date(StartDate, "%Y%m%d"),
         EndDate = as.Date(EndDate, "%Y%m%d"))

# 1. Membership in place on the cutoff date
PCN_active <- PCN_all %>%
  filter(StartDate <= PCN_cutoff,
         is.na(EndDate) | EndDate >= PCN_cutoff) %>%
  mutate(PCN_match = "On cutoff date")

# 2. Practices with no memberships on that date: use their current PCN if they have one,
# otherwise their most recent earlier one

PCN_nearest <- PCN_all %>%
  filter(!OrgCode %in% PCN_active$OrgCode) %>%
  arrange(OrgCode, desc(is.na(EndDate)), desc(EndDate), desc(StartDate)) %>%
  group_by(OrgCode) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(PCN_match = "Nearest record")

PCN <- bind_rows(PCN_active, PCN_nearest)

dataset_all <- left_join(dataset_all, PCN, by = c("OrgCode")) %>%
  select(-OrgName.y, -StartDate, -EndDate) %>%
  rename(OrgName = OrgName.x) %>%
  mutate(PCNName = replace_na(PCNName, "Not in a PCN"))

# 3. Practices in no PCN: take their ICB from the commissioner (sub-ICB) code in the practice list

sub_icb_lookup <- PCN_ICB_lookup %>%
  distinct(SubICBCode, SubICBName, ICB) %>%
  rename(ICB_epr = ICB, SubICBName_epr = SubICBName)

dataset_all <- dataset_all %>%
  left_join(sub_icb_lookup, by = c("Commissioner" = "SubICBCode")) %>%
  mutate(PCN_match = if_else(is.na(ICB) & !is.na(ICB_epr), "No PCN, ICB from practice list", PCN_match),
         SubICBCode = if_else(is.na(ICB) & !is.na(ICB_epr), Commissioner, SubICBCode),
         SubICBName = if_else(is.na(ICB) & !is.na(ICB_epr), SubICBName_epr, SubICBName),
         ICB = coalesce(ICB, ICB_epr)) %>%
  select(-Commissioner, -ICB_epr, -SubICBName_epr)

rm(sub_icb_lookup)


stopifnot(!any(duplicated(dataset_all$OrgCode)))
cat('Practices matched to a PCN on the cutoff date:', sum(dataset_all$PCN_match == "On cutoff date", na.rm = TRUE), '\n')
cat('Practices using their nearest PCN record:', sum(dataset_all$PCN_match == "Nearest record", na.rm = TRUE), '\n')
cat('Practices with an ICB from the practice list, not the PCN file:', sum(dataset_all$PCN_match == "No PCN, ICB from practice list", na.rm = TRUE), '\n')
cat('Practices still without an ICB (left out of the model):', sum(is.na(dataset_all$ICB)), '\n')
rm(PCN_ICB_lookup, PCN_all, PCN_active, PCN_nearest, PCN)

## 9). FINALISE AND WRITE OUTPUT -----------------------------------------------
# Converts proportions to whole number percentages
dataset_all <- dataset_all %>%
  mutate(prop_6074 = prop_6074 * 100,
         Prop_M_6074 = Prop_M_6074 * 100) %>%
  select(-SEX)

# One row per GP practice, ready for 02_fit_model.qmd
write_csv(dataset_all, "data/processed/cleaned_dataset.csv")
