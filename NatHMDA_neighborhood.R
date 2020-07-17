## ---------------------------
##
## Script: NatHMDA_neighborhood.R
## Author: Rob Pitingolo
## Created: December 1, 2019
## 
## Description: Steps to prepare HMDA national tract-level file
##
## ---------------------------

library(tidyverse)

# Set working directory
# MM: If you open an R project from RStudio (File -> Open Project...) you can use relative file paths within the project and dont need to set working directory
setwd("D:/NATDATA/hmda-neighborhood")


# Read national file and add county code
nathmda_in <- read.csv("L:/Libraries/HMDA/Raw/2018_lar.txt",sep="|")
#nathmda_in <- read.csv("2018_lar.txt",sep="|")

nathmda_clean <- mutate(nathmda_in, ucounty =str_pad(county_code, 5, pad = "0")) %>%
                        filter(state_code %in% c("DC")) 
  

# Read income limits file and define max limits for each county
# MM: Am reading in Section8-FY18.csv using relative file path
# il_in <- read.csv("D:/NATDATA/hmda-neighborhood/Income Limits/Section8-FY18.csv")
il_in <- read.csv("Income Limits/Section8-FY18.csv") 
# MM: rather than using mutate to create new columns for vlowmax and lowmax, you can rename the old columns using dplyr function 'rename' https://dplyr.tidyverse.org/reference/select.html
il_clean <- mutate(il_in, st_in =str_pad(State, 2, pad = "0"),
                   cnt_in =str_pad(County, 3, pad = "0"),
                   ucounty = paste0(st_in, cnt_in),
                   #Very low income max (Under 50% AMI)
                   vlowmax= l50_4, 
                   #Low income max (50-80% AMI)
                   lowmax= l80_4,
                   #Medium income max (80-100% AMI)
                   medmax= (l80_4/.8)*1.2) %>% 
                   #Keep unique counties
                   distinct(ucounty, .keep_all = TRUE) 


# Load state and county lists from RDS object
states <- readRDS("states.rds")
counties <- readRDS("counties.rds")
counties2 = data.frame(counties) %>% 
  unite(ucounty, state_fips,county_fips, sep = "", remove = FALSE)


# Merge national file state codes
nathmda_states <- merge(nathmda_clean, states, by.x="state_code", by.y="state_code", all.x=TRUE)


# Merge national file and counties, flag and create dummy codes for missing tracts/counties
nathmda_geo <- merge(nathmda_states, counties2, by.x="ucounty", by.y="ucounty", all.x=TRUE) %>% 
    mutate(missing_tract = case_when(is.na(census_tract)~ 1, TRUE ~ 0), 
           missing_county = case_when(is.na(county_code)~ 1, TRUE ~ 0),
           geo2010 = as.character(census_tract),
           geo2010 = if_else(missing_tract==1 & missing_county==0, paste0(ucounty, "000000"), geo2010),
           geo2010 = if_else(missing_tract==1 & missing_county==1, paste0(st_fips, "000000000"), geo2010)
           )


# Merge income limit data and create characteristic flags
nathmda_flags <-merge(nathmda_geo, il_clean, by.x="ucounty", by.y="ucounty", all.x=TRUE)%>%
    mutate(
          #Create income flags 
          actualincome=income*1000,
          missing_income = case_when(is.na(income)~ 1, TRUE ~ 0),
          vlowinc_flag = if_else(actualincome<=vlowmax,1,0),
          lowinc_flag = if_else(actualincome<=lowmax & actualincome>vlowmax,1,0),
          medinc_flag = if_else(actualincome<=medmax & actualincome>lowmax,1,0),
          highinc_flag = if_else(medmax<=actualincome,1,0),
          
          #Loan type flags
          conv_flag = if_else(loan_type==1,1,0),
          gov_flag = if_else(loan_type %in% c(2,3,4),1,0),
          
          #Loan purpose flags
          purch_flag = if_else(loan_purpose==1,1,0),
          impr_flag = if_else(loan_purpose==2,1,0),
          refi_flag = if_else(loan_purpose %in% c(31,32),1,0),
          othpurp_flag = if_else(loan_purpose==4,1,0),
          
          #Purchase type flags 
          prop1_4_flag = if_else(derived_dwelling_category %in% c('Single Family (1-4 Units):Site-Built','Single Family (1-4 Units):Manufactured'),1,0),
          prop5_flag = if_else(derived_dwelling_category %in% c('Multifamily:Site-Built (5+ Units)','Multifamily:Manufactured (5+ Units)'),1,0),
          
          #Outcome flags
          app_flag = 1,
          orig_flag = if_else(action_taken==1,1,0),
          deny_flag = if_else(action_taken==3,1,0),
          othoutcome_flag = if_else(action_taken %in% c(2,4,5,6,7,8),1,0),

          #Lein flag
          lein1_flag = if_else(lien_status==1,1,0),
          
          #Owner occupancy flag
          owner_flag = if_else(occupancy_type==1,1,0),
          secondary_flag = if_else(occupancy_type==2,1,0),
          investor_flag = if_else(occupancy_type==3,1,0),
          
          #Ethnicity flags
          hisp_flag = if_else(derived_ethnicity=='Hispanic or Latino',1,0),
          
          #Calculate Race for each applicant and co-applicant separately
          newrace_app = case_when(applicant_ethnicity_1 == 1 | applicant_ethnicity_1 %in% 11:14 ~ "Hispanic",
                              !is.na(applicant_race_2) ~ "Multiple Races",
                              applicant_race_1 %in% 6:7 ~ "Race Not Available",
                              is.na(applicant_race_1) ~ "Race Not Available",
                              applicant_race_1 == 5 ~ "White",
                              applicant_race_1 == 3 ~ "Black",
                              applicant_race_1 == 2 | applicant_race_1 %in% 21:27 ~ "Asian",
                              applicant_race_1 == 4 | applicant_race_1 %in% 41:44 ~ "Native Hawaiian or Other Pacific Islander",
                              applicant_race_1 == 1 ~ "American Indian or Alaskan Native",
                              TRUE ~ "NA"),
          
          newrace_coapp = case_when(co_applicant_ethnicity_1 == 5 | co_applicant_race_1 == 8 ~ "No co-applicant",
                              co_applicant_ethnicity_1 == 1 | co_applicant_ethnicity_1 %in% 11:14 ~ "Hispanic",
                              !is.na(co_applicant_race_2) ~ "Multiple Races",
                              co_applicant_race_1 %in% 6:7 ~ "Race Not Available",
                              is.na(co_applicant_race_1) ~ "Race Not Available",
                              co_applicant_race_1 == 5 ~ "White",
                              co_applicant_race_1 == 3 ~ "Black",
                              co_applicant_race_1 == 2 | co_applicant_race_1 %in% 21:27 ~ "Asian",
                              co_applicant_race_1 == 4 | co_applicant_race_1 %in% 41:44 ~ "Native Hawaiian or Other Pacific Islander",
                              co_applicant_race_1 == 1 ~ "American Indian or Alaskan Native",
                              TRUE ~ "NA"),
          
          #If there is a co-applicant and the co-applicant race is different from the applicant race
          hhmixedrace_flag = if_else(newrace_coapp != "No co-applicant" & newrace_app != "Race Not Available" & newrace_coapp != "Race Not Available" & (newrace_app!=newrace_coapp),1,0),

          #Calculate a household race
          hhrace = case_when(hhmixedrace_flag==1 ~ "Mixed",
                             newrace_app=="White" ~ "White",
                             newrace_app=="Black" ~ "Black",
                             newrace_app=="Asian" ~ "Asian",
                             newrace_app=="Native Hawaiian or Other Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
                             newrace_app=="American Indian or Alaskan Native" ~ "American Indian or Alaskan Native",
                             newrace_app=="Race Not Available" ~ "Race Not Available",
                             TRUE ~ "NA"),
          
          
          wht_flag = if_else(hhrace=='White',1,0),
          blk_flag = if_else(hhrace=='Black or African American',1,0),
          narace_flag = if_else(hhrace=='Race Not Available',1,0),
          nhpi_flag = if_else(hhrace=='Native Hawaiian or Other Pacific Islander',1,0),
          ais_flag = if_else(hhrace=='Asian',1,0),
          api_flag = if_else(hhrace %in% c('Native Hawaiian or Other Pacific Islander','Asian'),1,0),

          #Specific Asian race flags (NOT filtered by Hispanic)
          aind_flag = if_else(applicant_race_1 == 21,1,0),
          achi_flag = if_else(applicant_race_1 == 22,1,0),
          afil_flag = if_else(applicant_race_1 == 23,1,0),
          ajap_flag = if_else(applicant_race_1 == 24,1,0),
          akor_flag = if_else(applicant_race_1 == 25,1,0),
          avie_flag = if_else(applicant_race_1 == 26,1,0),
          
          #Other race flag
          othrace_flag = if_else(hisp_flag==0 & wht_flag==0 & blk_flag==0 & ais_flag==0 & nhpi_flag ==0 & narace_flag==0,1,0), 
  
          #Gender flags
          male_flag = if_else(derived_sex=='Male',1,0),
          female_flag = if_else(derived_sex=='Female',1,0),
          nasex_flag = if_else(derived_sex=='Sex Not Available',1,0),
          
          #Loan amount flag
          missing_loan_amount = case_when(is.na(loan_amount)~ 1, TRUE ~ 0)
          
          )
          
table(nathmda_flags$newrace_app)
table(nathmda_flags$newrace_coapp)
table(nathmda_flags$hhrace)

          

# This function creats final variables by taking any flag variables that are input
# and checking that all conditions are equal to one
create_var <- function(...){
  all_vars <- list(...)
  vname=if_else(all(all_vars==1), 1, 0)
  return(vname)
}


# Create all combined indicators
nathmda_comb <- nathmda_flags %>%
  rowwise() %>%
  
  #Top-line indicators
  mutate(orig_lein1=create_var(orig_flag,lein1_flag)) %>%
  mutate(orig_sf=create_var(orig_flag,prop1_4_flag)) %>%
  mutate(conv_purch=create_var(conv_flag,purch_flag)) %>%
  mutate(conv_refi=create_var(conv_flag,refi_flag)) %>%
  mutate(deny_sf=create_var(conv_flag,prop1_4_flag,deny_flag)) %>%
  
  #Denials by race 
  mutate(wht_deny=create_var(wht_flag,prop1_4_flag,deny_flag)) %>% 
  mutate(blk_deny=create_var(blk_flag,prop1_4_flag,deny_flag)) %>%
  mutate(hisp_deny=create_var(hisp_flag,prop1_4_flag,deny_flag)) %>%
  mutate(api_deny=create_var(api_flag,prop1_4_flag,deny_flag)) %>%
  mutate(ais_deny=create_var(ais_flag,prop1_4_flag,deny_flag)) %>%
  mutate(nhpi_deny=create_var(nhpi_flag,prop1_4_flag,deny_flag)) %>%
  mutate(narace_deny=create_var(narace_flag,prop1_4_flag,deny_flag)) %>%
  mutate(othrace_deny=create_var(othrace_flag,prop1_4_flag,deny_flag)) %>%
  
  #Denials by specific race 
  mutate(aind_deny=create_var(aind_flag,prop1_4_flag,deny_flag)) %>% 
  mutate(achi_deny=create_var(achi_flag,prop1_4_flag,deny_flag)) %>% 
  mutate(afil_deny=create_var(afil_flag,prop1_4_flag,deny_flag)) %>% 
  mutate(ajap_deny=create_var(ajap_flag,prop1_4_flag,deny_flag)) %>% 
  mutate(akor_deny=create_var(akor_flag,prop1_4_flag,deny_flag)) %>% 
  mutate(avie_deny=create_var(avie_flag,prop1_4_flag,deny_flag)) %>% 
  
  #Denials by income
  mutate(vlowinc_deny=create_var(vlowinc_flag,prop1_4_flag,deny_flag)) %>%
  mutate(lowinc_deny=create_var(lowinc_flag,prop1_4_flag,deny_flag)) %>%
  mutate(medinc_deny=create_var(medinc_flag,prop1_4_flag,deny_flag)) %>%
  mutate(highinc_deny=create_var(highinc_flag,prop1_4_flag,deny_flag)) %>%
  mutate(missinginc_deny=create_var(missing_income,prop1_4_flag,deny_flag)) %>%
  
  #Refis by race 
  mutate(wht_refi=create_var(wht_flag,conv_flag,refi_flag)) %>% 
  mutate(blk_refi=create_var(blk_flag,conv_flag,refi_flag)) %>%
  mutate(hisp_refi=create_var(hisp_flag,conv_flag,refi_flag)) %>%
  mutate(api_refi=create_var(api_flag,conv_flag,refi_flag)) %>%
  mutate(ais_refi=create_var(ais_flag,conv_flag,refi_flag)) %>%
  mutate(nhpi_refi=create_var(nhpi_flag,conv_flag,refi_flag)) %>%
  mutate(narace_refi=create_var(narace_flag,conv_flag,refi_flag)) %>%
  mutate(othrace_refi=create_var(othrace_flag,conv_flag,refi_flag)) %>%
  
  #Refis by specific race 
  mutate(aind_refi=create_var(aind_flag,conv_flag,refi_flag)) %>% 
  mutate(achi_refi=create_var(achi_flag,conv_flag,refi_flag)) %>% 
  mutate(afil_refi=create_var(afil_flag,conv_flag,refi_flag)) %>% 
  mutate(ajap_refi=create_var(ajap_flag,conv_flag,refi_flag)) %>% 
  mutate(akor_refi=create_var(akor_flag,conv_flag,refi_flag)) %>% 
  mutate(avie_refi=create_var(avie_flag,conv_flag,refi_flag)) %>% 
  
  #Refis by income
  mutate(vlowinc_refi=create_var(vlowinc_flag,conv_flag,refi_flag)) %>%
  mutate(lowinc_refi=create_var(lowinc_flag,conv_flag,refi_flag)) %>%
  mutate(medinc_refi=create_var(medinc_flag,conv_flag,refi_flag)) %>%
  mutate(highinc_refi=create_var(highinc_flag,conv_flag,refi_flag)) %>%
  mutate(missinginc_refi=create_var(missing_income,conv_flag,refi_flag)) %>%
  
  #Owner-occ purchase loans by race
  mutate(wht_purch=create_var(wht_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(blk_purch=create_var(blk_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(hisp_purch=create_var(hisp_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(api_purch=create_var(api_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(ais_purch=create_var(ais_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(nhpi_purch=create_var(nhpi_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(narace_purch=create_var(narace_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(othrace_purch=create_var(othrace_flag,owner_flag,purch_flag,conv_flag)) %>%
  
  #Owner-occ purchase loans by specific race 
  mutate(aind_purch=create_var(aind_flag,owner_flag,purch_flag,conv_flag)) %>% 
  mutate(achi_purch=create_var(achi_flag,owner_flag,purch_flag,conv_flag)) %>% 
  mutate(afil_purch=create_var(afil_flag,owner_flag,purch_flag,conv_flag)) %>% 
  mutate(ajap_purch=create_var(ajap_flag,owner_flag,purch_flag,conv_flag)) %>% 
  mutate(akor_purch=create_var(akor_flag,owner_flag,purch_flag,conv_flag)) %>% 
  mutate(avie_purch=create_var(avie_flag,owner_flag,purch_flag,conv_flag)) %>% 
  
  #Owner-occ purchase loans by income
  mutate(vlowinc_purch=create_var(vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(lowinc_purch=create_var(lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(medinc_purch=create_var(medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(highinc_purch=create_var(highinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(missinginc_purch=create_var(missing_income,owner_flag,purch_flag,conv_flag)) %>%

  #Owner-occ purchase loans to white borrowers by income
  mutate(wht_vlowinc=create_var(wht_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(wht_lowinc=create_var(wht_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(wht_medinc=create_var(wht_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(wht_highinc=create_var(wht_flag,highinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  
  #Owner-occ purchase loans to black borrowers by income
  mutate(blk_vlowinc=create_var(blk_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(blk_lowinc=create_var(blk_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(blk_medinc=create_var(blk_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(blk_highinc=create_var(blk_flag,highinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  
  #Owner-occ purchase loans to Hispanic borrowers by income
  mutate(hisp_vlowinc=create_var(hisp_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(hisp_lowinc=create_var(hisp_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(hisp_medinc=create_var(hisp_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(hisp_highinc=create_var(hisp_flag,highinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  
  #Owner-occ purchase loans to API borrowers by income
  mutate(api_vlowinc=create_var(api_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(api_lowinc=create_var(api_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(api_medinc=create_var(api_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(api_highinc=create_var(api_flag,highinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  
  #Owner-occ purchase loans to missing race borrowers by income
  mutate(narace_vlowinc=create_var(narace_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(narace_lowinc=create_var(narace_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(narace_medinc=create_var(narace_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(narace_highinc=create_var(narace_flag,highinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  
  #Owner-occ purchase loans to other race borrowers by income
  mutate(othrace_vlowinc=create_var(othrace_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(othrace_lowinc=create_var(othrace_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(othrace_medinc=create_var(othrace_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(othrace_highinc=create_var(othrace_flag,highinc_flag,owner_flag,purch_flag,conv_flag))%>%
  
  #Owner-occ purchase loans by gender 
  mutate(man_purch=create_var(male_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(wom_purch=create_var(female_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(nasex_purch=create_var(nasex_flag,owner_flag,purch_flag,conv_flag)) %>%
  
  #Owner-occ purchase loans to male borrowers by income
  mutate(man_vlowinc=create_var(male_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(man_lowinc=create_var(male_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(man_medinc=create_var(male_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(man_highinc=create_var(male_flag,highinc_flag,owner_flag,purch_flag,conv_flag))%>%
  
  #Owner-occ purchase loans to female borrowers by income
  mutate(wom_vlowinc=create_var(female_flag,vlowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(wom_lowinc=create_var(female_flag,lowinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(wom_medinc=create_var(female_flag,medinc_flag,owner_flag,purch_flag,conv_flag)) %>%
  mutate(wom_highinc=create_var(female_flag,highinc_flag,owner_flag,purch_flag,conv_flag))%>%

  #Median borrower income
  mutate(medincome = if_else(owner_flag ==1 & purch_flag ==1,actualincome,NaN))


#Final summarize by tract (state for testing)
hmda_tract <- nathmda_comb %>%
  group_by(
          #census_tract
           state_code
           ) %>%
  summarize(total_n = n(),
            
            apps_all = sum(app_flag, na.rm = TRUE),
            lein1 = sum(lein1_flag, na.rm = TRUE),
            deny_all = sum(deny_flag, na.rm = TRUE),
            deny_sf = sum(deny_sf, na.rm = TRUE),
            orig_lein1 =sum(orig_lein1, na.rm = TRUE),
            orig_sf =sum(orig_sf, na.rm = TRUE),
            conv_purch = sum(conv_purch, na.rm = TRUE),
            conv_refi = sum(conv_refi, na.rm = TRUE),
            purchases = sum(purch_flag, na.rm = TRUE),
            refis = sum(refi_flag, na.rm = TRUE),
            
            wht_deny = sum(wht_deny, na.rm = TRUE),
            blk_deny = sum(blk_deny, na.rm = TRUE),
            hisp_deny = sum(hisp_deny, na.rm = TRUE),
            api_deny = sum(api_deny, na.rm = TRUE),
            ais_deny = sum(ais_deny, na.rm = TRUE),
            nhpi_deny = sum(nhpi_deny, na.rm = TRUE),
            narace_deny = sum(narace_deny, na.rm = TRUE),
            othrace_deny = sum(othrace_deny, na.rm = TRUE),
            
            aind_deny = sum(aind_deny, na.rm = TRUE),
            achi_deny = sum(achi_deny, na.rm = TRUE),
            afil_deny = sum(afil_deny, na.rm = TRUE),
            ajap_deny = sum(ajap_deny, na.rm = TRUE),
            akor_deny = sum(akor_deny, na.rm = TRUE),
            avie_deny = sum(avie_deny, na.rm = TRUE),
            
            vlowinc_deny = sum(vlowinc_deny, na.rm = TRUE),
            lowinc_deny = sum(lowinc_deny, na.rm = TRUE),
            medinc_deny = sum(medinc_deny, na.rm = TRUE),
            highinc_deny = sum(highinc_deny, na.rm = TRUE),
            missinginc_deny = sum(missinginc_deny, na.rm = TRUE),
            
            wht_refi = sum(wht_refi, na.rm = TRUE),
            blk_refi = sum(blk_refi, na.rm = TRUE),
            hisp_refi = sum(hisp_refi, na.rm = TRUE),
            api_refi = sum(api_refi, na.rm = TRUE),
            ais_refi = sum(ais_refi, na.rm = TRUE),
            nhpi_refi = sum(nhpi_refi, na.rm = TRUE),
            narace_refi = sum(narace_refi, na.rm = TRUE),
            othrace_refi = sum(othrace_refi, na.rm = TRUE),
            
            aind_refi = sum(aind_refi, na.rm = TRUE),
            achi_refi = sum(achi_refi, na.rm = TRUE),
            afil_refi = sum(afil_refi, na.rm = TRUE),
            ajap_refi = sum(ajap_refi, na.rm = TRUE),
            akor_refi = sum(akor_refi, na.rm = TRUE),
            avie_refi = sum(avie_refi, na.rm = TRUE),
            
            vlowinc_refi = sum(vlowinc_refi, na.rm = TRUE),
            lowinc_refi = sum(lowinc_refi, na.rm = TRUE),
            medinc_refi = sum(medinc_refi, na.rm = TRUE),
            highinc_refi = sum(highinc_refi, na.rm = TRUE),
            missinginc_refi = sum(missinginc_refi, na.rm = TRUE),
            
            wht_deny = sum(wht_deny, na.rm = TRUE),
            blk_deny = sum(blk_deny, na.rm = TRUE),
            hisp_deny = sum(hisp_deny, na.rm = TRUE),
            api_deny = sum(api_deny, na.rm = TRUE),
            ais_deny = sum(ais_deny, na.rm = TRUE),
            nhpi_deny = sum(nhpi_deny, na.rm = TRUE),
            narace_deny = sum(narace_deny, na.rm = TRUE),
            othrace_deny = sum(othrace_deny, na.rm = TRUE),
            
            wht_purch = sum(wht_purch, na.rm = TRUE),
            blk_purch = sum(blk_purch, na.rm = TRUE),
            hisp_purch = sum(hisp_purch, na.rm = TRUE),
            api_purch = sum(api_purch, na.rm = TRUE),
            narace_purch = sum(narace_purch, na.rm = TRUE),
            othrace_purch = sum(othrace_purch, na.rm = TRUE),
            
            aind_purch = sum(aind_purch, na.rm = TRUE),
            achi_purch = sum(achi_purch, na.rm = TRUE),
            afil_purch = sum(afil_purch, na.rm = TRUE),
            ajap_purch = sum(ajap_purch, na.rm = TRUE),
            akor_purch = sum(akor_purch, na.rm = TRUE),
            avie_purch = sum(avie_purch, na.rm = TRUE),
            
            vlowinc_purch = sum(vlowinc_purch, na.rm = TRUE),
            lowinc_purch = sum(lowinc_purch, na.rm = TRUE),
            medinc_purch = sum(medinc_purch, na.rm = TRUE),
            highinc_purch = sum(highinc_purch, na.rm = TRUE),
            
            wht_vlowinc = sum(wht_vlowinc, na.rm = TRUE),
            wht_lowinc = sum(wht_lowinc, na.rm = TRUE),
            wht_medinc = sum(wht_medinc, na.rm = TRUE),
            wht_highinc = sum(wht_highinc, na.rm = TRUE),
            
            blk_vlowinc = sum(blk_vlowinc, na.rm = TRUE),
            blk_lowinc = sum(blk_lowinc, na.rm = TRUE),
            blk_medinc = sum(blk_medinc, na.rm = TRUE),
            blk_highinc = sum(blk_highinc, na.rm = TRUE),
            
            hisp_vlowinc = sum(hisp_vlowinc, na.rm = TRUE),
            hisp_lowinc = sum(hisp_lowinc, na.rm = TRUE),
            hisp_medinc = sum(hisp_medinc, na.rm = TRUE),
            hisp_highinc = sum(hisp_highinc, na.rm = TRUE),
            
            api_vlowinc = sum(api_vlowinc, na.rm = TRUE),
            api_lowinc = sum(api_lowinc, na.rm = TRUE),
            api_medinc = sum(api_medinc, na.rm = TRUE),
            api_highinc = sum(api_highinc, na.rm = TRUE),
            
            narace_vlowinc = sum(narace_vlowinc, na.rm = TRUE),
            narace_lowinc = sum(narace_lowinc, na.rm = TRUE),
            narace_medinc = sum(narace_medinc, na.rm = TRUE),
            narace_highinc = sum(narace_highinc, na.rm = TRUE),
            
            othrace_vlowinc = sum(othrace_vlowinc, na.rm = TRUE),
            othrace_lowinc = sum(othrace_lowinc, na.rm = TRUE),
            othrace_medinc = sum(othrace_medinc, na.rm = TRUE),
            othrace_highinc = sum(othrace_highinc, na.rm = TRUE),
            
            missing_borrower_income = sum(missing_income, na.rm = TRUE),
            median_borrower_income = median(medincome, na.rm = TRUE), 
            
            missing_loan_amount = sum(missing_loan_amount, na.rm = TRUE),
            median_loan_amount = median(loan_amount, na.rm = TRUE),
            
            missing_tract = sum(missing_tract, na.rm = TRUE)
            
            ) %>% 
  arrange(
          #census_tract
          state_code
          ) 
view(hmda_tract)



#End of program