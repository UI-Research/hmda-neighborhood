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

#Location of raw HMDA data (txt):
rawfile <- "//sas1/dcdata/Libraries/HMDA/Raw/2018_public_lar_pipe.txt"

#Location of income limit data (csv): 
ilfile <- "D:/NATDATA/hmda-neighborhood/Income Limits/Section8-FY18.csv"

#Location of county and states geography files (rds)
statefile <- "D:/NATDATA/hmda-neighborhood/states.rds"
countyfile <- "D:/NATDATA/hmda-neighborhood/counties.rds"

#Where to export final CSV:
outfile <- "D:/NATDATA/hmda-neighborhood/hmda_tract.csv"

#Start time for log
starttime <- Sys.time()
print(paste("Start Time",starttime))


# Read national file and add county code
nathmda_in <- read_delim(rawfile,delim="|") %>%
    mutate(#If the census tract is coded as "na" then switch to proper missing
        census_tract=case_when(census_tract=="na" ~ as.character(NA), 
                               TRUE ~ as.character(census_tract)),
        #If the county is coded as "na" or "99999" then switch to proper missing
        county_code=case_when(county_code %in% c("na","N/AN/","99999") ~ as.character(NA), 
                              TRUE ~ as.character(county_code)),
        #Check the state part of the county code
        ust = substr(county_code,1,2),
        #Create ucounty from the county_code var if the state part is valid (00,03,07 AND 80) are not state FIPS codes
        ucounty = case_when(ust %in% c("00","03","07","80") ~ as.character(NA), 
                            TRUE ~ as.character(county_code))) %>%
        #filter(state_code %in% c("DC")) 
        slice(1:20000)
  

# Read income limits file and define max limits for each county
il_in <- read_csv(ilfile) %>%
            mutate(#Create padded state FIPs code
                   st_in =str_pad(State, 2, pad = "0"),
                   #Created padded county code
                   cnt_in =str_pad(County, 3, pad = "0"),
                   #Create combined state + county code
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
states <- readRDS(statefile)
counties <- readRDS(countyfile) %>% 
  unite(ucounty, state_fips,county_fips, sep = "", remove = FALSE)


# Merge national file state codes
nathmda_states <- merge(nathmda_in, states, by.x="state_code", by.y="state_code", all.x=TRUE)


# Merge national file and counties, flag and create dummy codes for missing tracts/counties
nathmda_geo <- merge(nathmda_states, counties, by.x="ucounty", by.y="ucounty", all.x=TRUE) %>% 
  mutate(missing_tract = case_when(is.na(census_tract)~ 1, TRUE ~ 0), 
         missing_county = case_when(is.na(ucounty)~ 1, TRUE ~ 0),
         geo2010 = case_when(#If Census tract is valid then geo2010 is the input census tract
           missing_tract==0 ~ as.character(census_tract),
           #If the tract is missing but the county is valid then code as state + county + 000000
           missing_tract==1 & missing_county==0 ~ as.character(paste0(ucounty, "000000")),
           #If the tract and county are missing then code as state + 000000000
           missing_tract==1 & missing_county==1 & !is.na(st_fips) ~ as.character(paste0(st_fips, "000000000")),
           #If the geography is completely invalid, use 99999999999
           TRUE ~ "99999999999"))


# Merge income limit data and create characteristic flags
nathmda_flags <-merge(nathmda_geo, il_in, by.x="ucounty", by.y="ucounty", all.x=TRUE)%>%
    mutate(
          #Create income flags 
          actualincome=income*1000,
          missing_income = case_when(is.na(income)~ 1, TRUE ~ 0),
          vlowinc_flag = if_else(actualincome<=vlowmax,1,0),
          lowinc_flag = if_else(actualincome<=lowmax & actualincome>vlowmax,1,0),
          modinc_flag = if_else(actualincome<=medmax & actualincome>lowmax,1,0),
          highinc_flag = if_else(medmax<=actualincome,1,0),
          income_avail = if_else(missing_income !=1,1,0),
          
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
          
          #If there is a co-applicant flag if the co-applicant race is different from the applicant race
          hhmixedrace_flag = if_else(newrace_coapp != "No co-applicant" & newrace_app != "Race Not Available" & newrace_coapp != "Race Not Available" & (newrace_app!=newrace_coapp),1,0),

          #Calculate a household race from the applicant and co-applicant race
          hhrace = case_when(hhmixedrace_flag==1 ~ "Mixed",
                             newrace_app=="Hispanic" ~ "Hispanic",
                             newrace_app=="Multiple Races" ~ "Multiple Races",
                             newrace_app=="White" ~ "White",
                             newrace_app=="Black" ~ "Black",
                             newrace_app=="Asian" ~ "Asian",
                             newrace_app=="Native Hawaiian or Other Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
                             newrace_app=="American Indian or Alaskan Native" ~ "American Indian or Alaskan Native",
                             newrace_app=="Race Not Available" ~ "Race Not Available",
                             TRUE ~ "NA"),
          
          #Flags for individual race/ethnicity variables
          wht_flag = if_else(hhrace=='White',1,0),
          blk_flag = if_else(hhrace=='Black',1,0),
          hisp_flag = if_else(hhrace=='Hispanic',1,0),
          nhpi_flag = if_else(hhrace=='Native Hawaiian or Other Pacific Islander',1,0),
          ais_flag = if_else(hhrace=='Asian',1,0),
          api_flag = if_else(hhrace %in% c('Native Hawaiian or Other Pacific Islander','Asian'),1,0),
          aian_flag = if_else(hhrace=='American Indian or Alaskan Native',1,0),
          mrace_flag = if_else(hhrace=='Multiple Races',1,0),
          mixed_flag = if_else(hhrace=='Mixed',1,0),
          narace_flag = if_else(hhrace=='Race Not Available',1,0),
          
          #Race is available denominator
          race_avail = if_else(narace_flag!=1,1,0),

          #Specific Asian race flags (NOT filtered by Hispanic)
          aind_flag = if_else(applicant_race_1 == 21,1,0),
          achi_flag = if_else(applicant_race_1 == 22,1,0),
          afil_flag = if_else(applicant_race_1 == 23,1,0),
          ajpn_flag = if_else(applicant_race_1 == 24,1,0),
          akor_flag = if_else(applicant_race_1 == 25,1,0),
          avie_flag = if_else(applicant_race_1 == 26,1,0),
          
          #Other race flag
          othrace_flag = if_else(hisp_flag==0 & wht_flag==0 & blk_flag==0 & ais_flag==0 & nhpi_flag ==0 & narace_flag==0,1,0), 
  
          #Gender flags
          male_flag = if_else(derived_sex=='Male',1,0),
          female_flag = if_else(derived_sex=='Female',1,0),
          jointsex_flag = if_else(derived_sex=='Joint',1,0),
          nasex_flag = if_else(derived_sex=='Sex Not Available',1,0),
          sex_avail = if_else(nasex_flag != 1,1,0),
          sex_income_avail = if_else(sex_avail==1 & missing_income ==0,1,0),
          
          #Loan amount flag
          missing_loan_amount = case_when(is.na(loan_amount)~ 1, TRUE ~ 0),
          
          #Standard flag for all neighborhood HMDA indicators
          std_flag = if_else(owner_flag==1 & purch_flag==1 & lein1_flag==1 & prop1_4_flag==1 & orig_flag==1,1,0)
          ) 


#Check frequencies of new race categories          
table(nathmda_flags$newrace_app)
table(nathmda_flags$newrace_coapp)
table(nathmda_flags$hhrace)
table(nathmda_flags$race_avail)


# This function creats final variables by taking any flag variables that are input
# and checking that all conditions are equal to one
create_var <- function(...){
  all_vars <- list(...)
  vname=if_else(all(all_vars==1), 1, 0)
  return(vname)
}


# Create all combined indicators
nathmda_comb <- rowwise(nathmda_flags) %>% 
  mutate(#Top-line indicators
         owner_purch = create_var(std_flag),
         
         #Denominators
         income_avail=create_var(income_avail,std_flag),
         race_avail=create_var(race_avail,std_flag),
         race_income_avail=create_var(race_avail,income_avail,std_flag),
         sex_avail=create_var(sex_avail,std_flag),
         sex_income_avail=create_var(sex_avail,income_avail,std_flag),
         
         #Owner-occ purchase loans by race
         wht_purch=create_var(wht_flag,std_flag),
         blk_purch=create_var(blk_flag,std_flag),
         hisp_purch=create_var(hisp_flag,std_flag),
         api_purch=create_var(api_flag,std_flag),
         ais_purch=create_var(ais_flag,std_flag),
         nhpi_purch=create_var(nhpi_flag,std_flag),
         aian_purch=create_var(aian_flag,std_flag),
         mrace_purch=create_var(mrace_flag,std_flag),
         narace_purch=create_var(narace_flag,std_flag),
         othrace_purch=create_var(othrace_flag,std_flag),
  
         #Owner-occ purchase loans by specific race 
         aind_purch=create_var(aind_flag,std_flag),
         achi_purch=create_var(achi_flag,std_flag),
         afil_purch=create_var(afil_flag,std_flag),
         ajpn_purch=create_var(ajpn_flag,std_flag),
         akor_purch=create_var(akor_flag,std_flag),
         avie_purch=create_var(avie_flag,std_flag),
         
         #Owner-occ purchase loans by income
         vlowinc_purch=create_var(vlowinc_flag,std_flag),
         lowinc_purch=create_var(lowinc_flag,std_flag),
         modinc_purch=create_var(modinc_flag,std_flag),
         highinc_purch=create_var(highinc_flag,std_flag),
         missinginc_purch=create_var(missing_income,std_flag),
         
         #Owner-occ purchase loans to white borrowers by income
         wht_vlowinc=create_var(wht_flag,vlowinc_flag,std_flag),
         wht_lowinc=create_var(wht_flag,lowinc_flag,std_flag),
         wht_modinc=create_var(wht_flag,modinc_flag,std_flag),
         wht_highinc=create_var(wht_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to black borrowers by income
         blk_vlowinc=create_var(blk_flag,vlowinc_flag,std_flag),
         blk_lowinc=create_var(blk_flag,lowinc_flag,std_flag),
         blk_modinc=create_var(blk_flag,modinc_flag,std_flag),
         blk_highinc=create_var(blk_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to Hispanic borrowers by income
         hisp_vlowinc=create_var(hisp_flag,vlowinc_flag,std_flag),
         hisp_lowinc=create_var(hisp_flag,lowinc_flag,std_flag),
         hisp_modinc=create_var(hisp_flag,modinc_flag,std_flag),
         hisp_highinc=create_var(hisp_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to American Indian or Alaskan Native borrowers by income
         aian_vlowinc=create_var(aian_flag,vlowinc_flag,std_flag),
         aian_lowinc=create_var(aian_flag,lowinc_flag,std_flag),
         aian_modinc=create_var(aian_flag,modinc_flag,std_flag),
         aian_highinc=create_var(aian_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to API borrowers by income
         api_vlowinc=create_var(api_flag,vlowinc_flag,std_flag),
         api_lowinc=create_var(api_flag,lowinc_flag,std_flag),
         api_modinc=create_var(api_flag,modinc_flag,std_flag),
         api_highinc=create_var(api_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to multiple race/ethnicity borrowers by income
         mrace_vlowinc=create_var(mrace_flag,vlowinc_flag,std_flag),
         mrace_lowinc=create_var(mrace_flag,lowinc_flag,std_flag),
         mrace_modinc=create_var(mrace_flag,modinc_flag,std_flag),
         mrace_highinc=create_var(mrace_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to missing race borrowers by income
         narace_vlowinc=create_var(narace_flag,vlowinc_flag,std_flag),
         narace_lowinc=create_var(narace_flag,lowinc_flag,std_flag),
         narace_modinc=create_var(narace_flag,modinc_flag,std_flag),
         narace_highinc=create_var(narace_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to other race borrowers by income
         othrace_vlowinc=create_var(othrace_flag,vlowinc_flag,std_flag),
         othrace_lowinc=create_var(othrace_flag,lowinc_flag,std_flag),
         othrace_modinc=create_var(othrace_flag,modinc_flag,std_flag),
         othrace_highinc=create_var(othrace_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans by gender
         man_purch=create_var(male_flag,std_flag),
         wom_purch=create_var(female_flag,std_flag),
         mixedsex_purch=create_var(jointsex_flag,std_flag),
         nasex_purch=create_var(nasex_flag,std_flag),
         
         #Owner-occ purchase loans to male borrowers by income
         man_vlowinc=create_var(male_flag,vlowinc_flag,std_flag),
         man_lowinc=create_var(male_flag,lowinc_flag,std_flag),
         man_modinc=create_var(male_flag,modinc_flag,std_flag),
         man_highinc=create_var(male_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to female borrowers by income
         wom_vlowinc=create_var(female_flag,vlowinc_flag,std_flag),
         wom_lowinc=create_var(female_flag,lowinc_flag,std_flag),
         wom_modinc=create_var(female_flag,modinc_flag,std_flag),
         wom_highinc=create_var(female_flag,highinc_flag,std_flag),
         
         #Owner-occ purchase loans to female borrowers by income
         mixedsex_vlowinc=create_var(jointsex_flag,vlowinc_flag,std_flag),
         mixedsex_lowinc=create_var(jointsex_flag,lowinc_flag,std_flag),
         mixedsex_modinc=create_var(jointsex_flag,modinc_flag,std_flag),
         mixedsex_highinc=create_var(jointsex_flag,highinc_flag,std_flag),
         
         #Medians for owner-occ purchase loans
         medincome = if_else(std_flag ==1,actualincome,NaN),
         medloanamount = if_else(std_flag ==1,loan_amount,NaN)
         ) 
  
  



#List of sum variables to include in the final file
final_vars <- c(#Top-line vars
                "app_flag","owner_purch", 
                
                #Denominator vars
                "income_avail","race_avail","race_income_avail","sex_avail","sex_income_avail",
                
                #Race/ethnicity vars
                "wht_purch","blk_purch","hisp_purch","api_purch","mrace_purch","narace_purch","othrace_purch",
                "aian_purch","aind_purch","achi_purch","afil_purch","ajpn_purch","akor_purch","avie_purch",
                
                #Income vars
                "vlowinc_purch","lowinc_purch","modinc_purch","highinc_purch",
                
                #Combined race/income vars
                "wht_vlowinc","wht_lowinc","wht_modinc","wht_highinc",
                "blk_vlowinc","blk_lowinc","blk_modinc","blk_highinc",
                "hisp_vlowinc","hisp_lowinc","hisp_modinc","hisp_highinc",
                "aian_vlowinc","aian_lowinc","aian_modinc","aian_highinc",
                "api_vlowinc","api_lowinc","api_modinc","api_highinc",
                "mrace_vlowinc","mrace_lowinc","mrace_modinc","mrace_highinc",
                "narace_vlowinc","narace_lowinc","narace_modinc","narace_highinc",
                "othrace_vlowinc","othrace_lowinc","othrace_modinc","othrace_highinc",
                
                #Gender vars
                #"man_purch","wom_purch","mixedsex_purch","nasex_purch",
                
                #Gender by income
                #"man_vlowinc","man_lowinc","man_modinc","man_highinc",
                #"wom_vlowinc","wom_lowinc","wom_modinc","wom_highinc",
                #"mixedsex_vlowinc","mixedsex_lowinc","mixedsex_modinc","mixedsex_highinc",
                
                #Missing vars
                "missing_tract"
                )


#Summarize by tract 
hmda_tract <- nathmda_comb %>%
  group_by(geo2010) %>% 
  summarise(across(final_vars, sum, na.rm = TRUE),
            across(c(medincome,medloanamount), median, na.rm = TRUE)) %>% 
  rename(apps_all=app_flag,
         owner_purchases=owner_purch,
         median_income=medincome,
         median_loan_amount=medloanamount,
         missing_geo=missing_tract) %>% 
  arrange(geo2010) 


#Final cleanup
hmda_final <- mutate(hmda_tract, invalid_geo = if_else(missing_geo>0,1,0)) %>%
  select(-missing_geo)


#Export final CSV
write_csv(hmda_final, outfile)


#End time for log
endtime <- Sys.time()
totaltime <- endtime - starttime
print(paste("End Time",endtime))
print(totaltime)

#End of program