# Install required packages
install.packages(c("admiral", "pharmaverseadam", "dplyr", "tidyverse", "tern", "ggplot2"))
library("dplyr")
library("tern")
library("admiral")
library("pharmaverseadam")
library("ggplot2")

# Bring in SDTM data
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Keep just required variables and records 
ae1 <- adae %>%
  filter(TRTEMFL=="Y" & SAFFL=="Y") %>%
  select("STUDYID", "USUBJID", "AESOC", "AETERM", "ACTARM", "AESEV") 



ggplot(ae1, aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack") +
  scale_fill_manual(
    values = c(
      "MILD" = "#FFCCCC",   # Pale Red
      "MODERATE" = "green", 
      "SEVERE" = "blue"
    ),
    # This ensures the legend stays in your preferred order
    breaks = c("MILD", "MODERATE", "SEVERE") 
  ) +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Counts of AEs",
    fill = "Severity/Intensity"
  ) +
  theme_minimal()

ggsave(
  filename = "question_3_tlg/ae_severity_bar_chart.png",
  path = NULL,             # Saves to your current folder
  width = 10,              # Width in inches
  height = 6,              # Height in inches
  dpi = 300,               # High resolution (printing quality)
  bg = "white"             # Ensures the background isn't transparent
)

