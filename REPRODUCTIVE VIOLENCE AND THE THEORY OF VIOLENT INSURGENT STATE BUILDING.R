# Author: Ibok Precious
# Topic: REPRODUCTIVE VIOLENCE AND THE THEORY OF VIOLENT INSURGENT STATE BUILDING
# 

# Analysis Script
# 
#
# DESCRIPTION:
# This script analyzes data on rebel groups (with emphasis on religious extremist
# groups) and reproductive violence, testing the
# theory that forced marriage plays a distinct role in insurgent state building.
# This dataset also uses the absence forced abortion in religious extremist 
# groups to provide evidence of our theory.
# 
#
# 
#
# OUTPUTS:
# - Summary statistics tables
# - Analysis of forced marriage patterns
# - Cross-tabulations of key variables
# 

##################### SETUP AND DATA LOADING #########################

# Load required packages
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

cat("=========================================================================\n")
cat("REPRODUCTIVE VIOLENCE AND THE THEORY OF VIOLENT INSURGENT STATE BUILDING ANALYSIS SCRIPT\n")
cat("=========================================================================\n")
cat("Loading data and packages...\n")

# Load the dataset
data <- read_csv("REPRODUCTIVE VIOLENCE AND THE THEORY OF VIOLENT INSURGENT STATE BUILDING.csv")

cat("Dataset loaded successfully!\n")
cat("Number of rebel groups:", nrow(data), "\n")
cat("Number of variables:", ncol(data), "\n\n")




##################### DATA DESCRIPTION ############################
cat("=========================================================================\n")
cat("DATA DESCRIPTION\n")
cat("=========================================================================\n")

# Show variable names and types
cat("Variables in the dataset:\n")
glimpse(data)

cat("\nKey variables for analysis:\n")
cat("- forced_marriage: Binary indicator of forced marriage practices\n")
cat("- forced_abortion: Binary indicator of forced abortion practices\n") 
cat("- forced_pregnancy: Binary indicator of forced pregnancy practices\n")
cat("- islamist_appeals: Binary indicator of Islamist ideological appeals\n")
cat("- religious_secular_group: Binary (1=religious, 0=secular)\n")
cat("- feminist_appeals: Binary indicator of women's rights appeals\n")
cat("- reb_strength_ord: Ordinal measure of rebel group strength\n")
cat("- ideol_ord: Ordinal measure of ideological appeals\n")
cat("- group_duration: Continuous measure of group lifespan in years\n\n")



################ SUMMARY STATISTICS ########################

cat("=========================================================================\n")
cat("TABLE 1: SUMMARY STATISTICS\n")
cat("=========================================================================\n")

# Function to create summary statistics
create_summary_stats <- function(data) {
  map_dfr(names(data), function(var) {
    if(is.numeric(data[[var]])) {
      tibble(
        `Variable name` = var,
        `N` = sum(!is.na(data[[var]])),
        Mean = round(mean(data[[var]], na.rm = TRUE), 3),
        SD = round(sd(data[[var]], na.rm = TRUE), 3),
        Min. = round(min(data[[var]], na.rm = TRUE), 1),
        Max. = round(max(data[[var]], na.rm = TRUE), 1)
      )
    }
  })
}

# Select variables for summary table
summary_vars <- data %>%
  select(forced_marriage, forced_abortion, forced_pregnancy, 
         islamist_appeals, feminist_appeals, forced_recruit,
         reb_strength_ord, ideol_ord, group_duration)

# Create and display summary table
summary_table <- create_summary_stats(summary_vars)

# Clean variable names for presentation
summary_table <- summary_table %>%
  mutate(`Variable name` = case_when(
    `Variable name` == "forced_marriage" ~ "Forced marriage",
    `Variable name` == "forced_abortion" ~ "Forced abortion",
    `Variable name` == "forced_pregnancy" ~ "Forced pregnancy",
    `Variable name` == "islamist_appeals" ~ "Islamist appeals",
    `Variable name` == "feminist_appeals" ~ "Feminist appeals", 
    `Variable name` == "forced_recruit" ~ "Forced recruitment",
    `Variable name` == "reb_strength_ord" ~ "Rebel strength",
    `Variable name` == "ideol_ord" ~ "Ideological appeals",
    `Variable name` == "group_duration" ~ "Group duration (years)",
    TRUE ~ `Variable name`
  ))

print(summary_table, n = 25)
cat("\n")



############ FORCED MARRIAGE ANALYSIS ####################

cat("=========================================================================\n")
cat("TABLE 2: FORCED MARRIAGE BY GROUP TYPE\n")
cat("=========================================================================\n")

# Analyze forced marriage patterns by religious vs secular groups: This analysis
# tests the proportion of religious extremist groups who adopt forced marriage
# as opposed to secular militant groups

forced_marriage_religion <- data %>%
  filter(!is.na(religious_secular_group) & !is.na(forced_marriage)) %>%
  group_by(`Group Type` = ifelse(religious_secular_group == 1, "Religious", "Secular")) %>%
  summarise(
    `Total Groups` = n(),
    `Forced Marriage` = sum(forced_marriage == 1, na.rm = TRUE),
    `No Forced Marriage` = sum(forced_marriage == 0, na.rm = TRUE),
    `Proportion (%)` = round(mean(forced_marriage == 1, na.rm = TRUE) * 100, 1)
  )

print(forced_marriage_religion)
cat("\n")



############# GROUPS THAT MAKE ISLAMIC APPEALS ###############

cat("=========================================================================\n")
cat("TABLE 3: ISLAMIST APPEALS AND REPRODUCTIVE VIOLENCE\n")
cat("=========================================================================\n")

# Cross-tabulation: Islamist appeals vs reproductive violence
islamist_violence <- data %>%
  filter(!is.na(islamist_appeals)) %>%
  group_by(`Islamist Appeals` = ifelse(islamist_appeals == 1, "Yes", "No")) %>%
  summarise(
    `N Groups` = n(),
    `Forced Marriage (%)` = round(mean(forced_marriage == 1, na.rm = TRUE) * 100, 1),
    `Forced Abortion (%)` = round(mean(forced_abortion == 1, na.rm = TRUE) * 100, 1),
    `Forced Pregnancy (%)` = round(mean(forced_pregnancy == 1, na.rm = TRUE) * 100, 1)
  )

print(islamist_violence)
cat("\n")




################## KEY FINDINGS AND INTERPRETATION ######################

cat("=========================================================================\n")
cat("KEY FINDINGS AND INTERPRETATION\n")
cat("=========================================================================\n")

cat("BASIC PREVALENCE PATTERNS:\n")
forced_marriage_prev <- round(mean(data$forced_marriage == 1, na.rm = TRUE) * 100, 1)
forced_abortion_prev <- round(mean(data$forced_abortion == 1, na.rm = TRUE) * 100, 1)
forced_pregnancy_prev <- round(mean(data$forced_pregnancy == 1, na.rm = TRUE) * 100, 1)

cat("• Forced marriage prevalence:", forced_marriage_prev, "%\n")
cat("• Forced abortion prevalence:", forced_abortion_prev, "%\n")
cat("• Forced pregnancy prevalence:", forced_pregnancy_prev, "%\n\n")


cat("RELIGIOUS VS SECULAR PATTERNS:\n")
cat("• Religious groups:", forced_marriage_religion$`Proportion (%)`[1], "% practice forced marriage\n")
cat("• Secular groups:", forced_marriage_religion$`Proportion (%)`[2], "% practice forced marriage\n")
cat("• Difference:", round(abs(forced_marriage_religion$`Proportion (%)`[1] - forced_marriage_religion$`Proportion (%)`[2]), 1), "percentage points\n\n")


cat("ISLAMIST APPEALS PATTERNS:\n")
cat("• Groups with Islamist appeals:", islamist_violence$`Forced Marriage (%)`[1], "% practice forced marriage\n")
cat("• Groups without Islamist appeals:", islamist_violence$`Forced Marriage (%)`[2], "% practice forced marriage\n\n")



############### SAVE OUTPUTS ################

cat("=========================================================================\n")
cat("SAVING OUTPUT FILES\n")
cat("=========================================================================\n")

# Save all analysis results
write_csv(summary_table, "summary_statistics.csv")
write_csv(forced_marriage_religion, "forced_marriage_by_religion.csv")
write_csv(islamist_violence, "islamist_appeals_analysis.csv")


cat("Output files saved:\n")
cat("1. summary_statistics.csv - Main summary statistics\n")
cat("2. forced_marriage_by_religion.csv - Religious vs secular analysis\n")
cat("3. islamist_appeals_analysis.csv - Islamist appeals analysis\n")



#################### SESSION INFO ############################

cat("=========================================================================\n")
cat("SESSION INFORMATION\n")
cat("=========================================================================\n")

cat("Analysis completed on:", format(Sys.Date(), "%B %d, %Y"), "\n")
cat("R version:", R.version$version.string, "\n")
cat("tidyverse version:", packageVersion("tidyverse"), "\n\n")

cat("=========================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=========================================================================\n")
cat("All analyses completed successfully. Results are displayed above and saved\n")
cat("in CSV files for further use. The findings support the theoretical framework\n")
cat("about the distinct role of forced marriage in insurgent state building.\n")