# install.packages("sdtm.oak")
# install.packages("pharmaverseraw")
# install.packages("dplyr")
# install.packages("logrx")
# install.packages("haven")


library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)
library(stringr)
library(logrx)
library(haven)

# logrx::axecute("01_create_ds_domain.R")
getwd()
setwd("C:/Users/suvar/Rworkdr/Genentech")

dir.create("programs", recursive = TRUE, showWarnings = FALSE)
dir.create("data/sdtm", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
sink("logs/ds.log", append = TRUE, split = TRUE)

cat("DS program started:", as.character(Sys.time()), "\n")

ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

ds_raw <- ds_raw %>%
  mutate(
    OTHERSP = na_if(str_trim(OTHERSP), ""),
    IT.DSTERM = na_if(str_trim(IT.DSTERM), ""),
    IT.DSDECOD = na_if(str_trim(IT.DSDECOD), "")
  ) %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

ds <- tibble::tibble()
# ds <- ds_raw %>%
#   select(oak_id, raw_source, patient_number)

# Add standard variables and derivations
# Start with required OAK variables
ds <- ds_raw %>%
  select(oak_id, raw_source, patient_number) %>% 
  
  # OTHERSP not missing -> DSTERM = OTHERSP
  assign_no_ct(
    raw_dat = ds_raw %>% filter(!is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  
  # OTHERSP not missing -> DSDECOD = OTHERSP
  assign_no_ct(
    raw_dat = ds_raw %>% filter(!is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  
  # OTHERSP missing -> DSTERM = IT.DSTERM
  assign_no_ct(
    raw_dat = ds_raw %>% filter(is.na(OTHERSP)),
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>% 
  
  # OTHERSP missing -> DSDECOD = IT.DSDECOD
  assign_no_ct(
    raw_dat = ds_raw %>% filter(is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    id_vars = oak_id_vars()
  )

# Now add STUDYID, DOMAIN, USUBJID, DSCAT, VISIT, VISITNUM
ds <- ds %>%
  left_join(
    ds_raw %>%
      select(oak_id, STUDY, PATNUM, OTHERSP, IT.DSDECOD, INSTANCE),
    by = "oak_id"
  ) %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", PATNUM),
    
    DSCAT = case_when(
      !is.na(OTHERSP) ~ "OTHER EVENT",
      DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",
      !is.na(DSDECOD) & DSDECOD != "Randomized" ~ "DISPOSITION EVENT",
      TRUE ~ NA_character_
    ),
    
    VISIT = case_when(
      str_detect(INSTANCE, regex("SCREEN|RETRIEVAL|AMBUL", ignore_case = TRUE)) ~ NA_character_,
      !is.na(INSTANCE) ~ str_to_title(INSTANCE),
      TRUE ~ NA_character_
    ),
    
    VISITNUM = case_when(
      VISIT == "Baseline" ~ 0,
      str_detect(VISIT, regex("^Week", ignore_case = TRUE)) ~
        as.numeric(as.integer(str_extract(VISIT, "\\d+"))),
      str_detect(VISIT, regex("Unscheduled", ignore_case = TRUE)) ~
        as.numeric(str_extract(VISIT, "\\d+\\.?\\d*")),
      TRUE ~ NA_real_
    )
  )


ds <- ds %>%
  mutate(
    DSSTDTC = DSDTC
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  arrange(USUBJID, VISITNUM, DSSTDTC, DSTERM) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "VISITNUM", "DSSTDTC", "DSTERM")
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT,
    DSDTC, DSSTDTC, DSSTDY,
    VISITNUM, VISIT
  )

write_xpt(ds, "data/sdtm/ds.xpt", version = 5)
cat("DS program completed:", as.character(Sys.time()), "\n")
sink()


