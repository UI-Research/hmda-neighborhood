library(tidyverse)
library(arrow)
library(here)

raw_hmda_to_parquet = function(year, inpath = NULL, delimit_character = "|", outpath = NULL, subsetted_columns = NULL) {
  ## year: year of data.
  ## inpath: filepath to read raw .txt file from.
  ## delimit_character: the symbol used to separate values in the inpath file.
  ## outpath: filepath to write converted file to.
  ## subsetted_columns: the columns to select and retain in the outputted .parquet file.
     ## if null, default columns are selected.
  
  year = as.character(year)
  
  if (is.null(inpath)) { inpath = here("data-raw", paste0("raw_hmda_lar_", year, ".txt")) }
  if (is.null(outpath)) { outpath = here("data-raw", paste0("raw_hmda_lar_", year, ".parquet")) }
  
  if (!file.exists(inpath)) { stop("There is no file at the specified inpath.")}
  if (!str_detect(outpath, ".parquet")) { stop("Outpath must be to a '.parquet'-suffixed file.") }
  
  if (is.null(subsetted_columns)) {
    #The relevant HMDA columns
    subsetted_columns = c(
      "state_code",
      "county_code",
      "census_tract",
      "income",
      "loan_type",
      "loan_amount",
      "loan_purpose",
      "derived_dwelling_category",
      "action_taken",
      "lien_status",
      "occupancy_type",
      "applicant_ethnicity_1",
      "co_applicant_race_2",
      "co_applicant_ethnicity_1",
      "applicant_race_1",
      "co_applicant_race_1",
      "applicant_race_2",
      "derived_sex",
      "applicant_age",
      "co_applicant_age",
      "purchaser_type"
    )
  }
  
  ## a quick test prior to reading in full file
  raw_txt_test_delimit_character = tryCatch(
    { read_delim(inpath, delim = delimit_character, n_max = 5) },
    error = function(e) { stop("Error reading inpath file. Did you provide the correct delimit_character value for the input file-type?")})
  
  ## a quick test prior to reading in full file
  raw_txt_test_subsetted_columns = tryCatch(
    { read_delim(inpath, delim = delimit_character, col_select = all_of(subsetted_columns), n_max = 5) },
    error = function(e) { stop("Error reading inpath file. The subsetted columns may not be present in the inpath file.")})
  
  ## a callback function to read the full file in chunks
  read_chunk_callback = function(x, cols) { x %>% select(all_of(subsetted_columns)) }
  
  ## reading the fule in chunks
  raw_text_subsetted = tryCatch(
    { read_delim_chunked(inpath, delim = delimit_character, callback = DataFrameCallback$new(read_chunk_callback), chunk_size = 100000) },
    error = function(e) { stop(e)})
  
  ## writing the file to .parquet, a much more memory-efficient file format
  write_parquet(raw_text_subsetted, sink = outpath)
  
}

# raw_hmda_to_parquet(year = 2022) # ~5.9GB to ~375MB
# raw_hmda_to_parquet(year = 2021) # ~10GB to ~1.1GB
# raw_hmda_to_parquet(year = 2020) # ~9.8GB to ~375MB
# raw_hmda_to_parquet(year = 2019) # ~6.5GB to ~260 MB
# raw_hmda_to_parquet(year = 2018) # ~5.7GB to ~220 MB
