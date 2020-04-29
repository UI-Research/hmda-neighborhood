# National Census-tract level summary file of Home Mortgage Disclosure Act (HMDA) indicators
This repository contains code for collecting, reformatting, and processing the HMDA loan-level data and summarizing to the Census-tract level for publication on the Urban Institute Data Catalog. 
## Source data
The source data is from the Dynamic National Loan-Level Dataset [currently available on the FFEIC website.](https://ffiec.cfpb.gov/data-publication/dynamic-national-loan-level-dataset) You also need to download HUD income limits for the correspondent year. Those are [currently found on the HUDUser website.](https://www.huduser.gov/portal/datasets/il.html)
## Running the code
Download the Dynamic National Loan-Level Dataset and update the filepath at the top of the R script to reference the location. This is a large file (approximately 5GB) so plan accordingly. Also update the filename for the current HUD income limits file. 

The program uses the "noncensus" R package to create a list of counties and county codes which is used to merge the HUD income limits data with the HMDA data. Please check that this package is still functioning as intended.

The output is a Census-tract level (each row is a Census tract) summary file for the entire U.S.A. 
