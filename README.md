# National Census-tract level summary file of Home Mortgage Disclosure Act (HMDA) indicators

"The US government collects and distributes an enormous database with information about US mortgages called the Home Mortgage Disclosure Act (HMDA) data. The HMDA dataset contains the most comprehensive publicly available information on mortgage market activity. Each fall, new HMDA data are made available. In 2016, almost 7,000 institutions released over 16 million records, making HMDA an invaluable administrative dataset on housing and homeownership for policymakers, regulators, and researchers."" -- [Urban Institute's Housing Finance Policy Center](https://www.urban.org/policy-centers/housing-finance-policy-center/projects/home-mortgage-disclosure-act-data)

While these data are extremely useful, they are also very detailed: each record in the dataset reflects details about an individual mortgage application. As a result, the unprocessed dataset is very large, and for many audiences, the data are too complex to enable them to answer questions of interest. This project derives Census-tract level statistics from the unprocessed HMDA dataset to make the data more useful and accessible to a wider audience. 

This repository contains code for collecting, reformatting, and processing the HMDA loan-level data and summarizing to the Census-tract level for publication on the Urban Institute Data Catalog. 

## Source data
The source data is from the Dynamic National Loan-Level Dataset [currently available on the FFEIC website.](https://ffiec.cfpb.gov/data-publication/dynamic-national-loan-level-dataset) You also need to download HUD income limits for the corresponding year. Those are [currently found on the HUDUser website.](https://www.huduser.gov/portal/datasets/il.html)

## Running the code
Download the Dynamic National Loan-Level Dataset and update the filepath at the top of the R script to reference the location. This is a large file (approximately 5GB) so plan accordingly. Also update the filename for the current HUD income limits file. 

The program uses the "noncensus" R package to create a list of counties and county codes which is used to merge the HUD income limits data with the HMDA data. Please check that this package is still functioning as intended.

The output is a Census-tract level (each row is a Census tract) summary file for the entire U.S.A. 
