# National Census-tract level summary file of Home Mortgage Disclosure Act (HMDA) indicators

"The US government collects and distributes an enormous database with information about US mortgages called the Home Mortgage Disclosure Act (HMDA) data. The HMDA dataset contains the most comprehensive publicly available information on mortgage market activity. Each fall, new HMDA data are made available. In 2016, almost 7,000 institutions released over 16 million records, making HMDA an invaluable administrative dataset on housing and homeownership for policymakers, regulators, and researchers."" -- [Urban Institute's Housing Finance Policy Center](https://www.urban.org/policy-centers/housing-finance-policy-center/projects/home-mortgage-disclosure-act-data)

While these data are extremely useful, they are also very detailed: each record in the dataset reflects details about an individual mortgage application. As a result, the unprocessed dataset is very large, and for many audiences, the data are too complex to enable them to answer questions of interest. This project derives Census-tract level statistics from the unprocessed HMDA dataset to make the data more useful and accessible to a wider audience.

This repository contains code for collecting, reformatting, and processing the HMDA loan-level data and summarizing to the Census-tract level. Final data files are published on the [Urban Institute Data Catalog](https://datacatalog.urban.org/dataset/home-mortgage-disclosure-act-neighborhood-summary-files-census-tract-level).

## Version Control

**`library(renv)`** is an R package that allows you to track your project dependencies (namely, the version of R you're working with and the various versions of packages that are used in the project). When you clone or pull the repository from GitHub, you'll want to run `renv::restore()`, which will ensure that you're working with all the correct versions of the packages used in the project. If you're contributing to the project, run `renv::snapshot()` before pushing commits to the repository--this will ensure that `renv.lock` (AKA "the lockfile"\--this is the behind-the-scenes record of your project dependencies) contains all the most current information about your packages. When you first clone the repository, you may need to specify `options(renv.consent = TRUE)` to enable the library to write files within the project directory.

## Scripts

-   **`NationalHMDA_TractSummaryIndicators.Rmd`:** this is the primary script where raw, loan application-level data are aggregated up to census tract-level indicators.

-   **`raw_hmda_data_to_parquet.R`:** this is a utility script that will convert the files available on the FFEIC website to .parquet files, which are significantly smaller and faster to work with. You can specify the input file path, but the default takes the form "raw_hmda_lar_YYYY.txt" where "YYYY" is the four digit year.

## Source Data

The source data is from the [Snapshot National Loan-Level Datasets published by FFIEC](https://ffiec.cfpb.gov/data-publication/snapshot-national-loan-level-dataset/2022), while income limits are taken from [the HUDUser website.](#0) To join income limits--which are specified at the county and subcounty levels--to census tracts, we use crosswalks from the Missouri Data Center's Geocorr application. Both income limits and crosswalks are included in this repository in the `/data-raw/` folder.

## Running the Code

-   Download the Dynamic National Loan-Level Datasets from the FFEIC website;

-   Run raw_hmda_data_to_parquet.R to convert the downloaded data into .parquet format;

-   Specify the desired year at the top of `NationalHMDA_TractSummaryIndicators.Rmd` and then run all chunks in the .Rmd.

The output is a Census-tract level (each row is a Census tract) summary file that will be written to `/data-final/`.
