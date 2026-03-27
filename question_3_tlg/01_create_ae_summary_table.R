# Install required packages
install.packages(c("admiral", "pharmaverseadam", "dplyr", "tidyverse", "tern"))
library("dplyr")
library("tern")
library("admiral")
library("pharmaverseadam")

# Bring in SDTM data
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Keep just required variables and records 
ae1 <- adae %>%
    filter(TRTEMFL=="Y" & SAFFL=="Y") %>%
    select("STUDYID", "USUBJID", "AESOC", "AETERM", "ACTARM") %>%
    var_relabel(
      AESOC = "Primary  System Organ Class",
      AETERM = "Reported Term for the Adverse Event"
  )

sl1 <- adsl %>%
  filter(SAFFL=="Y") %>%
  select("STUDYID", "USUBJID", "ACTARM")


# Define the split function
split_fun <- drop_split_levels

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  analyze_num_patients(
    vars = "USUBJID",
    .stats = "unique",
    .labels = "Treatment Emergent AEs"
  ) %>%
  split_rows_by(
    "AESOC",
    child_labels = "visible",
    nested = FALSE,
    split_fun = split_fun,
    label_pos = "topleft",
    split_label = obj_label(adae$AESOC)
  ) %>%
  summarize_num_patients(var = "USUBJID",
                         .stats = "unique") %>%
  count_occurrences(
    vars = "AETERM",
    .indent_mods = -1L
  ) %>%
  append_varlabels(adae, "AETERM", indent = 1L)



result <- build_table(lyt, df = adae, alt_counts_df = adsl)
result


export_as_pdf(
  result, 
  file = "question_3_tlg/ae_summary_table.pdf",
  pg_width = 12,        # Width in inches (Landscape is often better for AE tables)
  pg_height =7,        # Height in inches
  fontsize = 8,      # Smaller font to fit more columns if needed
  paginate = TRUE    # Automatically splits the table into multiple pages if it's long
)