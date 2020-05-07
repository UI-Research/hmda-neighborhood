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
#library(noncensus)

# Set working directory
# MM: If you open an R project from RStudio (File -> Open Project...) you can use relative file paths within the project and dont need to set working directory
setwd("D:/NATDATA/HMDA")


# Read national file and add county code
#nathmda_in <- read.csv("L:/Libraries/HMDA/Raw/2018_lar.txt",sep="|",nrows=20000)
nathmda_in <- read.csv("2018_lar.txt",sep="|")

nathmda_clean <- mutate(nathmda_in, ucounty =str_pad(county_code, 5, pad = "0")) 
  

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


# Load county list from noncensus package
# MM: am now reading the counties rds file provided by Rob rather than getting it from the noncensus package
# data(counties)
counties <- readRDS("counties.rds")
counties2 = data.frame(counties) %>% 
  unite(ucounty, state_fips,county_fips, sep = "", remove = FALSE)


# Merge national file and counties, flag missing tracts/counties
nathmda_geo <- merge(nathmda_clean, counties2, by.x="ucounty", by.y="ucounty", all.x=TRUE) %>% 
    mutate(missing_tract = case_when(is.na(census_tract)~ 1, TRUE ~ 0), 
           missing_county = case_when(is.na(county_code)~ 1, TRUE ~ 0))


# Merge income limit data
nathmda_flags <-merge(nathmda_geo, il_clean, by.x="ucounty", by.y="ucounty", all.x=TRUE)%>%
    mutate(
          #Create income flags 
          actualincome=income*1000,
          missing_income = case_when(is.na(income)~ 1, TRUE ~ 0),
          vlowinc_flag = if_else(actualincome<=vlowmax,1,0),
          lowinc_flag = if_else(actualincome<=lowmax & actualincome>vlowmax,1,0),
          medinc_flag = if_else(actualincome<=medmax & actualincome>lowmax,1,0),
          highinc_flag = if_else(medmax<=actualincome,1,0),
          
          #Outcome flags
          app_flag = 1,
          orig_flag = if_else(action_taken==1,1,0),
          deny_flag = if_else(action_taken==3,1,0),
          purch_flag = if_else(action_taken==1 & loan_purpose==1,1,0),
          refi_flag = if_else(action_taken==1 & (loan_purpose==31 | loan_purpose==32),1,0),
          othpurp_flag = if_else(action_taken==1 & (loan_purpose==2 | loan_purpose==4),1,0),
          
          #Lein flag
          lein1_flag = if_else(lien_status==1,1,0),
          
          #SF home flag
          # MM: when using or (|) statements for one variable, you can alternatively use check if the variable is in a list, for example, if_else(variable %in% c('option1','option2'),... ,...)
          sf_flag = if_else(derived_dwelling_category=='Single Family (1-4 Units):Site-Built' | derived_dwelling_category=='Single Family (1-4 Units):Manufactured',1,0),
          
          #Owner occupancy flag
          owner_flag = if_else(occupancy_type==1,1,0),
          
          #Race/ethnicity flags
          hisp_flag = if_else(derived_ethnicity=='Hispanic or Latino',1,0),
          wht_flag = if_else(hisp_flag==0 & derived_race=='White',1,0),
          blk_flag = if_else(hisp_flag==0 & derived_race=='Black or African American',1,0),
          api_flag = if_else(hisp_flag==0 & (derived_race=='Asian' | derived_race=='Native Hawaiian or Other Pacific Islander') ,1,0),
          narace_flag = if_else(derived_race=='Race Not Available',1,0),
          othrace_flag = if_else(hisp_flag==0 & wht_flag==0 & blk_flag==0 & api_flag==0 & narace_flag==0,1,0)) 
#%>%
  
          #filter(
          #Keep only first lein mortgages
          #lein1_flag == 1,
          
          #Keep only SF (1-4unit) homes
          #sf_flag ==1)  
          
# MM: rather than the three separate functions, you can have one function that takes in a variable numer of arguments using the elipses (...)
create_all_comb <- function(...){
  # MM: Then convert the function arguments to a list
  all_vars <- list(...)
  # MM: Then check that all of the variables in the list equal one, and set vname based on that
  vname=if_else(all(all_vars==1), 1, 0)
  return(vname)
}

# Functions to combine multiple variables based on flags
create2comb <- function(v1,v2){
  vname=if_else(v1==1 & v2==1,1,0)
  return(vname)}

create3comb <- function(v1,v2,v3){
  vname=if_else(v1==1 & v2==1 & v3==1,1,0)
  return(vname)}

create4comb <- function(v1,v2,v3,v4){
  vname=if_else(v1==1 & v2==1 & v3==1 & v4==1,1,0)
  return(vname)}


# Create all combined indicators
nathmda_comb <- nathmda_flags %>%
  rowwise() %>%
  #Denials by race 
  mutate(wht_deny=create2comb(wht_flag,deny_flag)) %>% 
  mutate(blk_deny=create2comb(blk_flag,deny_flag)) %>%
  mutate(hisp_deny=create2comb(hisp_flag,deny_flag)) %>%
  mutate(api_deny=create2comb(api_flag,deny_flag)) %>%
  mutate(narace_deny=create2comb(narace_flag,deny_flag)) %>%
  mutate(othrace_deny=create2comb(othrace_flag,deny_flag)) %>%
  
  #Denials by income
  mutate(vlowinc_deny=create2comb(vlowinc_flag,deny_flag)) %>%
  mutate(lowinc_deny=create2comb(lowinc_flag,deny_flag)) %>%
  mutate(medinc_deny=create2comb(medinc_flag,deny_flag)) %>%
  mutate(highinc_deny=create2comb(highinc_flag,deny_flag)) %>%
  mutate(missinginc_deny=create2comb(missing_income,deny_flag)) %>%
  
  #Refis by race 
  mutate(wht_refi=create2comb(wht_flag,refi_flag)) %>% 
  mutate(blk_refi=create2comb(blk_flag,refi_flag)) %>%
  mutate(hisp_refi=create2comb(hisp_flag,refi_flag)) %>%
  mutate(api_refi=create2comb(api_flag,refi_flag)) %>%
  mutate(narace_refi=create2comb(narace_flag,refi_flag)) %>%
  mutate(othrace_refi=create2comb(othrace_flag,refi_flag)) %>%
  
  #Refis by income
  mutate(vlowinc_refi=create2comb(vlowinc_flag,refi_flag)) %>%
  mutate(lowinc_refi=create2comb(lowinc_flag,refi_flag)) %>%
  mutate(medinc_refi=create2comb(medinc_flag,refi_flag)) %>%
  mutate(highinc_refi=create2comb(highinc_flag,refi_flag)) %>%
  mutate(missinginc_refi=create2comb(missing_income,refi_flag)) %>%
  
  #Refis by race 
  mutate(wht_deny=create2comb(wht_flag,deny_flag)) %>% 
  mutate(blk_deny=create2comb(blk_flag,deny_flag)) %>%
  mutate(hisp_deny=create2comb(hisp_flag,deny_flag)) %>%
  mutate(api_deny=create2comb(api_flag,deny_flag)) %>%
  mutate(narace_deny=create2comb(narace_flag,deny_flag)) %>%
  mutate(othrace_deny=create2comb(othrace_flag,deny_flag)) %>%
  
  #Owner-occ purchase loans by race
  mutate(wht_purch=create3comb(wht_flag,owner_flag,purch_flag)) %>%
  mutate(blk_purch=create3comb(blk_flag,owner_flag,purch_flag)) %>%
  mutate(hisp_purch=create3comb(hisp_flag,owner_flag,purch_flag)) %>%
  mutate(api_purch=create3comb(api_flag,owner_flag,purch_flag)) %>%
  mutate(narace_purch=create3comb(narace_flag,owner_flag,purch_flag)) %>%
  mutate(othrace_purch=create3comb(othrace_flag,owner_flag,purch_flag)) %>%
  
  #Owner-occ purchase loans by income
  mutate(vlowinc_purch=create3comb(vlowinc_flag,owner_flag,purch_flag, na.rm=TRUE)) %>%
  mutate(lowinc_purch=create3comb(lowinc_flag,owner_flag,purch_flag, na.rm=TRUE)) %>%
  mutate(medinc_purch=create3comb(medinc_flag,owner_flag,purch_flag, na.rm=TRUE)) %>%
  mutate(highinc_purch=create3comb(highinc_flag,owner_flag,purch_flag, na.rm=TRUE)) %>%

  #Owner-occ purchase loans to white borrowers by income
  mutate(wht_vlowinc=create4comb(wht_flag,vlowinc_flag,owner_flag,purch_flag)) %>%
  mutate(wht_lowinc=create4comb(wht_flag,lowinc_flag,owner_flag,purch_flag)) %>%
  mutate(wht_medinc=create4comb(wht_flag,medinc_flag,owner_flag,purch_flag)) %>%
  mutate(wht_highinc=create4comb(wht_flag,highinc_flag,owner_flag,purch_flag)) %>%
  
  #Owner-occ purchase loans to black borrowers by income
  mutate(blk_vlowinc=create4comb(blk_flag,vlowinc_flag,owner_flag,purch_flag)) %>%
  mutate(blk_lowinc=create4comb(blk_flag,lowinc_flag,owner_flag,purch_flag)) %>%
  mutate(blk_medinc=create4comb(blk_flag,medinc_flag,owner_flag,purch_flag)) %>%
  mutate(blk_highinc=create4comb(blk_flag,highinc_flag,owner_flag,purch_flag)) %>%
  
  #Owner-occ purchase loans to Hispanic borrowers by income
  mutate(hisp_vlowinc=create4comb(hisp_flag,vlowinc_flag,owner_flag,purch_flag)) %>%
  mutate(hisp_lowinc=create4comb(hisp_flag,lowinc_flag,owner_flag,purch_flag)) %>%
  mutate(hisp_medinc=create4comb(hisp_flag,medinc_flag,owner_flag,purch_flag)) %>%
  mutate(hisp_highinc=create4comb(hisp_flag,highinc_flag,owner_flag,purch_flag)) %>%
  
  #Owner-occ purchase loans to API borrowers by income
  mutate(api_vlowinc=create4comb(api_flag,vlowinc_flag,owner_flag,purch_flag)) %>%
  mutate(api_lowinc=create4comb(api_flag,lowinc_flag,owner_flag,purch_flag)) %>%
  mutate(api_medinc=create4comb(api_flag,medinc_flag,owner_flag,purch_flag)) %>%
  mutate(api_highinc=create4comb(api_flag,highinc_flag,owner_flag,purch_flag)) %>%
  
  #Owner-occ purchase loans to missing race borrowers by income
  mutate(narace_vlowinc=create4comb(narace_flag,vlowinc_flag,owner_flag,purch_flag)) %>%
  mutate(narace_lowinc=create4comb(narace_flag,lowinc_flag,owner_flag,purch_flag)) %>%
  mutate(narace_medinc=create4comb(narace_flag,medinc_flag,owner_flag,purch_flag)) %>%
  mutate(narace_highinc=create4comb(narace_flag,highinc_flag,owner_flag,purch_flag)) %>%
  
  #Owner-occ purchase loans to other race borrowers by income
  mutate(othrace_vlowinc=create4comb(othrace_flag,vlowinc_flag,owner_flag,purch_flag)) %>%
  mutate(othrace_lowinc=create4comb(othrace_flag,lowinc_flag,owner_flag,purch_flag)) %>%
  mutate(othrace_medinc=create4comb(othrace_flag,medinc_flag,owner_flag,purch_flag)) %>%
  mutate(othrace_highinc=create4comb(othrace_flag,highinc_flag,owner_flag,purch_flag))


# MM: what is the purpose of this function? If not being used then you can remove
createsum <- function(vname,var){
  vname=sum(var)}


#Final summarize by tract (state for testing)
hmda_tract <- nathmda_comb %>%
  group_by(#cenus_tract
           state_code) %>%
  summarize(total_n = n(),
            
            apps = sum(app_flag),
            denials = sum(deny_flag),
            originations =sum(orig_flag),
            purchases = sum(purch_flag),
            refis = sum(refi_flag),
            
            wht_deny = sum(wht_deny),
            blk_deny = sum(blk_deny),
            hisp_deny = sum(hisp_deny),
            api_deny = sum(api_deny),
            narace_deny = sum(narace_deny),
            othrace_deny = sum(othrace_deny),
            
            vlowinc_deny = sum(vlowinc_deny),
            lowinc_deny = sum(lowinc_deny),
            medinc_deny = sum(medinc_deny),
            highinc_deny = sum(highinc_deny),
            missinginc_deny = sum(missinginc_deny),
            
            wht_refi = sum(wht_refi),
            blk_refi = sum(blk_refi),
            hisp_refi = sum(hisp_refi),
            api_refi = sum(api_refi),
            narace_refi = sum(narace_refi),
            othrace_refi = sum(othrace_refi),
            
            vlowinc_refi = sum(vlowinc_refi),
            lowinc_refi = sum(lowinc_refi),
            medinc_refi = sum(medinc_refi),
            highinc_refi = sum(highinc_refi),
            missinginc_refi = sum(missinginc_refi),
            
            wht_deny = sum(wht_deny),
            blk_deny = sum(blk_deny),
            hisp_deny = sum(hisp_deny),
            api_deny = sum(api_deny),
            narace_deny = sum(narace_deny),
            othrace_deny = sum(othrace_deny),
            
            wht_purch = sum(wht_purch),
            blk_purch = sum(blk_purch),
            hisp_purch = sum(hisp_purch),
            api_purch = sum(api_purch),
            narace_purch = sum(narace_purch),
            othrace_purch = sum(othrace_purch),
            
            # MM: ADDING `na.rm = TRUE` removes NAs from the calculation, if there are any NAs in the calculation the result will be 0
            # MM: You can add them to the rest of the sum summarize statements as well, i noticed that many of the results are NA
            vlowinc_purch = sum(vlowinc_purch, na.rm = TRUE),
            lowinc_purch = sum(lowinc_purch, na.rm = TRUE),
            medinc_purch = sum(medinc_purch, na.rm = TRUE),
            highinc_purch = sum(highinc_purch, na.rm = TRUE),
            
            wht_vlowinc = sum(wht_vlowinc),
            wht_lowinc = sum(wht_lowinc),
            wht_medinc = sum(wht_medinc),
            wht_highinc = sum(wht_highinc),
            
            blk_vlowinc = sum(blk_vlowinc),
            blk_lowinc = sum(blk_lowinc),
            blk_medinc = sum(blk_medinc),
            blk_highinc = sum(blk_highinc),
            
            hisp_vlowinc = sum(hisp_vlowinc),
            hisp_lowinc = sum(hisp_lowinc),
            hisp_medinc = sum(hisp_medinc),
            hisp_highinc = sum(hisp_highinc),
            
            api_vlowinc = sum(api_vlowinc),
            api_lowinc = sum(api_lowinc),
            api_medinc = sum(api_medinc),
            api_highinc = sum(api_highinc),
            
            narace_vlowinc = sum(narace_vlowinc),
            narace_lowinc = sum(narace_lowinc),
            narace_medinc = sum(narace_medinc),
            narace_highinc = sum(narace_highinc),
            
            othrace_vlowinc = sum(othrace_vlowinc),
            othrace_lowinc = sum(othrace_lowinc),
            othrace_medinc = sum(othrace_medinc),
            othrace_highinc = sum(othrace_highinc),
            
            missing_borrower_income = sum(is.na(actualincome)),
            median_borrower_income = median(actualincome, na.rm = TRUE)
            ) %>% 
  arrange(#census_tract
          state_code) 
view(hmda_tract)



#End of program