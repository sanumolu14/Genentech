# install.packages("admiral")   # only if not installed
# install.packages("metacore")
# install.packages("metatools")
# install.packages("pharmaversesdtm")
# install.packages("haven")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("logrx")

# packageVersion("xportr")

library(metacore)
library(metatools)
library(pharmaversesdtm)
library(admiral)
library(xportr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(logrx)
library(haven)


# setwd("C:/Users/suvar/Rworkdr/Genentech")
# dir.create("data/adsl", recursive = TRUE, showWarnings = FALSE)
sink("logs/adsl.log", append = TRUE, split = TRUE)
# Read in input SDTM data
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae
suppdm <- pharmaversesdtm::suppdm

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)
suppdm <- convert_blanks_to_na(suppdm)

# Combine Parent and Supp - very handy! ----
dm_suppdm <- combine_supp(dm, suppdm)

# metacore <- spec_to_metacore(
#   path = "./metadata/safety_specs.xlsx",
#   # All datasets are described in the same sheet
#   where_sep_sheet = FALSE
# ) %>%
#   select_dataset("ADSL")
#   
#   
agegr9_lookup <- exprs(
  ~condition,            ~AGEGR9, ~AGEGR9N,
  is.na(AGE),          "Missing",        4,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 50),  "18-50",        2,
  !is.na(AGE),             ">50",        3
)

format_agegr9 <- function(age) {
  case_when(
    age < 18 ~ "<18",
    between(age, 18, 50) ~ "18-50",
    age > 50 ~ ">50",
    TRUE ~ "Missing"
  )
}

format_agegr9n <- function(age) {
  case_when(
    age < 18 ~ 1,
    between(age, 18, 50) ~ 2,
    age > 50 ~ 3,
    TRUE ~ 4
  )
}

# 1. Vitals: last complete VS date with valid result
vs_alive <- vs %>%
  filter(
    !is.na(VSDTC),
    nchar(substr(VSDTC, 1, 10)) == 10,
    !(is.na(VSSTRESN) & is.na(VSSTRESC))
  ) %>%
  mutate(LSTAVLDT_VS = as.Date(substr(VSDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(LSTAVLDT_VS = max(LSTAVLDT_VS, na.rm = TRUE), .groups = "drop")

# 2. AE: last complete AE onset date
ae_alive <- ae %>%
  filter(
    !is.na(AESTDTC),
    nchar(substr(AESTDTC, 1, 10)) == 10
  ) %>%
  mutate(LSTAVLDT_AE = as.Date(substr(AESTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(LSTAVLDT_AE = max(LSTAVLDT_AE, na.rm = TRUE), .groups = "drop")

# 3. DS: last complete disposition date
ds_alive <- ds %>%
  filter(
    !is.na(DSSTDTC),
    nchar(substr(DSSTDTC, 1, 10)) == 10
  ) %>%
  mutate(LSTAVLDT_DS = as.Date(substr(DSSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(LSTAVLDT_DS = max(LSTAVLDT_DS, na.rm = TRUE), .groups = "drop")


adsl_cat <- derive_vars_cat(
  dataset = dm_suppdm ,
  definition = agegr9_lookup
) %>% 
  mutate(
    AGEGR9 = format_agegr9(AGE),
    AGEGR9N = format_agegr9n(AGE)
  )

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  )  %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl_preds  <- adsl_cat %>%
  # Treatment Start Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Treatment End Datetime
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>% 
  
  # Treatment Start and End Date
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM)) %>% # Convert Datetime variables to date
  # Treatment Start Time
  derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM)) %>%
  # ITTFL derivation
  mutate(
    ITTFL = ifelse(!is.na(ARM),'Y','N')
  ) 

# 4. Treatment: last treatment administration date from ADSL.TRTEDTM
trt_alive <- adsl_preds %>%
  filter(!is.na(TRTEDTM)) %>%
  mutate(LSTAVLDT_TRT = as.Date(TRTEDTM)) %>%
  select(USUBJID, LSTAVLDT_TRT)

# Merge and take max
adsl <- adsl_preds %>%
  left_join(vs_alive, by = "USUBJID") %>%
  left_join(ae_alive, by = "USUBJID") %>%
  left_join(ds_alive, by = "USUBJID") %>%
  left_join(trt_alive, by = "USUBJID") %>%
  rowwise() %>%
  mutate(
    LSTAVLDT = max(
      c(LSTAVLDT_VS, LSTAVLDT_AE, LSTAVLDT_DS, LSTAVLDT_TRT),
      na.rm = TRUE
    ),
    LSTAVLDT = ifelse(is.infinite(LSTAVLDT), NA, LSTAVLDT),
    LSTAVLDT1 = as.POSIXct.Date(LSTAVLDT, format = '%m-%d-%Y')
  ) %>%
  ungroup()

adsl <- adsl %>%
  mutate(LSTAVLDT = as.Date(LSTAVLDT1)) %>% 
    select(STUDYID, USUBJID,  ITTFL, AGEGR9,AGEGR9N,TRTSDTM,TRTSTMF,LSTAVLDT)

attr(adsl$ITTFL, "label") <- "Intent to Treatment Population Flag"
attr(adsl$AGEGR9, "label") <- "Age group9"
attr(adsl$AGEGR9N, "label") <- "Age group9 (N)"
attr(adsl$TRTSDTM, "label") <- "Treatment Start Date/Time"
attr(adsl$TRTSTMF, "label") <- "Treatment Start Time Imputation Flag"
attr(adsl$LSTAVLDT, "label") <- "Last Avaluation Date"



# dir <- tempdir('C:/Users/suvar/Rworkdr/Genentech/data/adam') # Specify the directory for saving the XPT file
# dir1 <- "C:/Users/suvar/Rworkdr/Genentech" # Specify the directory for metadata
# 
# metacore <- spec_to_metacore(
#   path = file.path(dir1, "metadata/adam_specs.xlsx"),
#   where_sep_sheet = FALSE
# ) %>%
#   select_dataset("ADSL")

# adsl %>%
#   # check_variables(metacore) %>% # Check all variables specified are present and no more
#   # check_ct_data(metacore, na_acceptable = TRUE) %>% # Checks all variables with CT only contain values within the CT
#   order_cols(metacore) %>% # Orders the columns according to the spec
#   sort_by_key(metacore) %>% # Sorts the rows by the sort keys
#   xportr_type(metacore, domain = "ADSL") %>% # Coerce variable type to match spec
#   xportr_length(metacore) %>% # Assigns SAS length from a variable level metadata
#   xportr_label(metacore) %>% # Assigns variable label from metacore specifications
#   xportr_df_label(metacore) %>% # Assigns dataset label from metacore specifications
#   xportr_write(file.path(dir, "adsl.xpt"))
# xportr_write(file.path(dir, "adsl.xpt"), metadata = metacore, domain = "ADSL")
# 
# 

# print(n=75,subset(metacore$ds_vars, dataset == "ADSL"))
# adsl %>%
#   check_variables(metacore, dataset_name = "ADSL") %>%
#   check_ct_data(metacore, dataset_name = "ADSL", na_acceptable = TRUE) %>%
#   order_cols(metacore, dataset_name = "ADSL") %>%
#   sort_by_key(metacore, dataset_name = "ADSL") %>%
#   xportr_type(metacore, dataset_name = "ADSL") %>%
#   xportr_length(metacore, dataset_name = "ADSL") %>%
#   xportr_label(metacore, dataset_name = "ADSL") %>%
#   xportr_df_label(metacore, dataset_name = "ADSL") %>%
#   xportr_write(file.path(dir, "adsl.xpt"))

write_xpt(adsl, "data/adam/adsl.xpt", version = 5)
cat("ADSL program completed:", as.character(Sys.time()), "\n")
sink()
