# install.packages("xportr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("logrx")
# install.packages("gtsummary")
 # install.packages("gt")
# Load libraries & data -------------------------------------
library(dplyr)
library(gtsummary)
library(gt)
library(logrx)
 
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# setwd("C:/Users/suvar/Rworkdr/Genentech")
# dir.create("outputs", showWarnings = FALSE)

# Start log --------------------------------------------------
# sink("logs/01_create_ae_summary_table.log", split = TRUE)
cat("Program started:", as.character(Sys.time()), "\n")

# Pre-processing --------------------------------------------
adae <- adae %>% 
  filter(
    # safety population
    SAFFL == "Y" & TRTEMFL == "Y"
    
  )

tbl <- adae %>% 
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs" 
  ) %>%
  add_overall(last = FALSE, col_label = "**Total**") %>%   # adds total column
  # sort_hierarchical(sort = list(AEBODSYS = "alphanumeric", AEDECOD = "descending",by = "frequency"), decreasing = TRUE)   # sort high to low
  # sort_hierarchical(tbl)
  sort_hierarchical(tbl, sort = list(AEBODSYS = "alphanumeric", AETERM = "descending")) 

gt_tbl <- as_gt(tbl)

# Save outputs ----------------------------------------------
gtsave(gt_tbl, "outputs/01_create_ae_summary_table.html")


# End log ----------------------------------------------------
cat("HTML created: outputs/01_create_ae_summary_table.html\n")

cat("Program ended:", as.character(Sys.time()), "\n")
# sink()
