###################################################################
# Title: Reproductive violence and the theory of violent insurgent state building
# Author: Ibok Precious
# Date: December 2025

############################################
# SETUP AND PACKAGE INSTALLATION
############################################

#Install required packages

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("gmodels")
# install.packages("logistf")
# install.packages("summarytools")

# Load required packages
library("readxl")
library("dplyr")
library("gmodels")
library("logistf")
library("summarytools")


###########################################################
# DATA IMPORT AND PREPARATION
# NOTE: Update the file path to match your local directory
###########################################################
df <- read_excel("REPRODUCTIVE VIOLENCE AND THE THEORY OF VIOLENT INSURGENT STATE BUILDING DATASET.xlsx")

# Convert relevant variables to numeric
df <- df %>%
 mutate(across(c(forced_marriage,
                 forced_pregnancy,
                 forced_abortion,
                 territorial_cont,
                 islamist_appeals,
                 feminist_appeals,
                 forced_recruit,
                 ethno_nat,
                 any_rape,
                 any_mprape,
                 islamic_secular_group),
               as.numeric)) 

###############################################################
# DESCRIPTIVE STATISTICS (TABLE 2)
###############################################################

# Select variables for summary statistics
summary_vars <- df %>%
  select(forced_marriage,
         forced_pregnancy,
         forced_abortion,
         islamic_secular_group,
         forced_recruit,
         territorial_cont,
         reb_strength_ord,
         region)
# Generate summary statistics
summary_stats <- summary_vars %>%
  desc(stats = c("mean", "sd", "min", "max", "n.valid"),
       transpose = TRUE)

print(summary_stats)


############################################################
# CROSS TABULATION ANALYSIS (TABLE 3)
############################################################

# Cross tabulation: Islamic extremist groups and forced marriage

CrossTable(df$islamic_secular_group,
           df$forced_marriage,
           prop.chisq = FALSE,
           prop.c = TRUE,
           prop.r = TRUE,
           prop.t = FALSE,
           dnn = c("Religious Secular Group", "Forced Marriage"))

# Cross tabulation: Islamic extremist groups and forced pregnancy

CrossTable(df$islamic_secular_group,
           df$forced_pregnancy,
           prop.chisq = FALSE,
           prop.c = TRUE,
           prop.r = TRUE,
           prop.t = FALSE,
           dnn = c("Religious Secular Group", "Forced Pregnancy"))

######################################################################
# FIRTH"S LOGISTIC REGRESSION MODELS (TABLE 4)
####################################################################

# Model 1: Forced marriage (Bivariate Model)

model1 <- logistf(forced_marriage ~ islamic_secular_group, 
                  data = df)
summary(model1)

# Model 2: Forced marriage (Full model with controls)

model2 <- logistf(forced_marriage ~ islamic_secular_group + 
                    territorial_cont + 
                    reb_strength_ord + 
                    forced_recruit + 
                    region,
                  data = df)
summary(model2)

# Model 3: Forced pregnancy (Bivariate Model)

model3 <- logistf(forced_pregnancy ~ islamic_secular_group, 
                  data = df)
summary(model3)

# Model 4: Forced pregnancy (Full model with controls)

model4 <- logistf(forced_pregnancy ~ islamic_secular_group +
                    territorial_cont + 
                    reb_strength_ord + 
                    forced_recruit + 
                    region,
                  data = df)
summary(model4)

################################################################
# ODD RATIOS AND CONFIDENCE INTERVALS
################################################################

# Model 1: Forced Marriage (Bivariate)
or_model1 <- exp(cbind(OR = coef(model1), confint(model1)))
print(or_model1)

# Model 2: Forced Marriage (Full model with controls)
or_model2 <- exp(cbind(OR = coef(model2), confint(model2)))
print(or_model2)

# Model 3: Forced Pregnancy (Bivariate)
or_model3 <- exp(cbind(OR = coef(model3), confint(model3)))
print(or_model3)

# Model 4: Forced Pregnancy (Full model with controls)
or_model4 <- exp(cbind(OR = coef(model4), confint(model4)))
print(or_model4)

#############################################################
# MODEL DIAGNOSTICS
############################################################

# Sample sizes for each model
cat("\nSample Sizes:\n")
cat("Model 1 (Forced Marriage - Bivariate): n =", model1$n, "\n")
cat("Model 2 (Forced Marriage - Full model with controls): n =", model2$n, "\n")
cat("Model 3 (Forced Pregnancy - Bivariate): n =", model3$n, "\n")
cat("Model 4 (Forced Pregnancy - Full model with controls): n =", model4$n, "\n")

# Log-likelihood values

cat("\nLog-Likelihood Values:\n")
cat("Model 1: ", model1$loglik[2], "\n")
cat("Model 2: ", model2$loglik[2], "\n")
cat("Model 3: ", model3$loglik[2], "\n")
cat("Model 4: ", model4$loglik[2], "\n")

  
  
#################################################################
# END OF SCRIPT
#################################################################
  
  