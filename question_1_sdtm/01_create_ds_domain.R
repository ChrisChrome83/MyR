# Start writing to log
log_file <- file("question_1_sdtm/01_create_ds_domain.txt", open = "wt") 
sink(log_file, type = "output")
sink(log_file, type = "message")




# Install required packages
install.packages(c("admiral", "sdtm.oak", "pharmaverseraw", "dplyr", "tidyverse", "lubridate", "labelled"))
library("dplyr")
library("admiral")
library("sdtm.oak")
library("pharmaverseraw")
library("tidyverse")
library("lubridate")
library("labelled")


# Read in raw data
ds_raw <- pharmaverseraw::ds_raw
dm_raw <- pharmaverseraw::dm_raw

# Create informed consent date from DM
dm <- dm_raw %>%
  mutate(USUBJID=str_c(str_trim(STUDY, side = "right"), "/", str_trim(PATNUM, side = "right"))) %>%
  mutate(RFXSTDTC=as.character(format(mdy(IC_DT), "%Y-%m-%d"))) %>%
  select(c(USUBJID, RFXSTDTC,IC_DT))

# Generate Oak Id vars
ds_raw1 <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# Read in CT
study_ct <- read.csv("metadata/sdtm_ct.csv")

# Create format for VISITNUM from visit
vis <- study_ct %>%
  filter(term_code=="VISITNUM") %>%
  mutate(VISIT=collected_value) %>%
  mutate(VISITNUM=as.numeric(term_value)) %>%
  select(c(VISITNUM, VISIT))


# Create most easily defined variables 
ds1 <- ds_raw1 %>%
  mutate(STUDYID=STUDY) %>%
  mutate(DOMAIN="DS") %>%
  mutate(USUBJID=str_c(str_trim(STUDY, side = "right"), "/", str_trim(PATNUM, side = "right"))) %>%
  mutate(DSTERM=if_else(is.na(OTHERSP), IT.DSTERM, OTHERSP)) %>%
  mutate(DSDECOD=if_else(is.na(OTHERSP), IT.DSDECOD, OTHERSP)) %>%
  mutate(DSCAT=case_when(IT.DSDECOD=="Randomised" ~ "PROTOCOL MILESTONE", is.na(OTHERSP) ~ "OTHER EVENT", TRUE ~ "DISPOSITION EVENT")) %>%
  mutate(VISIT=INSTANCE) %>%
  mutate(DSDTC1=as.character(format(mdy(DSDTCOL), "%Y-%m-%d"))) %>%
  mutate(DSDTC=if_else(is.na(DSTMCOL), DSDTC1, str_c(str_trim(DSDTC1, side = "right"), "T", str_trim(DSTMCOL, side = "right")))) %>%
  mutate(DSSTDTC=as.character(format(mdy(IT.DSSTDAT), "%Y-%m-%d"))) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY")

# Add visit number 
ds2 <- left_join(ds1, vis, by="VISIT")


# Sort, add sequence and keep only required variables
ds3 <- ds2 %>%
  arrange(PATNUM, DSSTDTC) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC")) %>%
  select("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY")

#Add labels
ds4 <- ds3 %>%
  set_variable_labels(
    STUDYID  = "Study Identifier",
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    DSSEQ    = "Sequence Number",
    DSTERM   = "Reported Term for the Disposition Event",
    DSDECOD  = "Standardized Disposition Term",
    DSCAT    = "Category for Disposition Event ",
    VISITNUM = "Visit Name",
    VISIT    = "Visit Name",
    DSDTC    = "Date/Time of Collection",
    DSSTDTC  = "Start Date/Time of Disposition Event",
    DSSTDY   = "Study Day of Start of Disposition Event"
  )

# Write out dataset
saveRDS(ds4, "question_1_sdtm/ds.rds")


# Stop writing to log
sink(type = "output")
sink(type = "message")
close(log_file)