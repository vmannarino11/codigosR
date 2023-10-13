#                                                               Term Paper 2

#                                                               Advanced Econometrics

# Students: Juan Sebastian Navajas Jauregui, Julian Vergara, and Valentin Mannarino

# Let's start by clearing the environment
rm(list = ls())

# Install the required packages
#install.packages("stargazer")
#install.packages("plm")
#install.packages("Formula")

# Load the libraries to be used
library(openxlsx)
library(readxl)
library(stargazer)
library(plm)
library(Formula)

# Set the working directory
setwd("C:/Users/Usuario/OneDrive - Económicas - UBA/Valentin/Códigos/R/Cornwell and Trumbull - paper")
getwd()

# Import the dataset
base <- read_excel("cornwell2.xlsx")
View(base)

# Specify that we are working with panel data
base <- pdata.frame(base, index = c("county", "year"))

# Define the model to be estimated in the paper
modelo <- Formula(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity + lpctymle + lwcon
                  + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + west + central + urban + lpctmin)

# SECTION 1 #
# Perform the between estimation
estim_be <- plm(modelo, data = base, model = "between")

# Generate the table and export it to text
stargazer(estim_be,
          type="text",
          dep.var.labels=c("CRIME RATE"),
          out="between.txt")

# SECTION 2 #
# Perform the within estimation - for counties only
estim_fe <- plm(modelo, data = base, model = "within")

# Generate the table and export it to text
stargazer(estim_fe,
          type="text",
          dep.var.labels=c("CRIME RATE"),
          out="within.txt")

# Conduct the Hausman test for the absence of fixed effects. Hausman Test.
# H0: FE is consistent, BE is consistent
# HA: FE is consistent, BE is inconsistent
phtest(estim_fe, estim_be)

# SECTION 3 #
# Perform the random effects estimation
estim_re <- plm(modelo, data = base, model = "random")

# Generate the table and export it to text
stargazer(estim_re,
          type="text",
          dep.var.labels=c("CRIME RATE"),
          out="random.txt")

# Implement the Hausman test to compare fixed effects and random effects estimators
# H0: FE is consistent, RE is consistent and efficient
# HA: FE is consistent, RE is inconsistent
phtest(estim_fe, estim_re)

# Export a summary table with all estimations
stargazer(estim_be, estim_fe, estim_re,
          type="latex",
          dep.var.labels=c("CRIME RATE"),
          out="summary.txt")

# SECTION 5 #
# Random effects test
# plmtest
plmtest(estim_re)

# General serial correlation test
# pbgtest
pbgtest(estim_re)
pbgtest(estim_fe)

# Unobservable effects test
# pwtest
pwtest(modelo, data = base)

# Now, we will perform a series of tests related to serial correlation or random effects
# pbsytest

# Joint test of serial correlation and random effects (under normality and homoscedasticity)
pbsytest(modelo, data = base, test = "J")

# Robust local test for serial correlation or random effects, first-order serial correlation.
pbsytest(modelo, data = base, test = "AR")

# Robust local test for serial correlation or Random Effects, taking into account that the variance must be non-negative
pbsytest(modelo, data = base, test = "RE")

# Conditional LM test for AR(1) or MA(1) errors under Random Effects
# pbltest
pbltest(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + ldensity + lpctymle + lwcon
        + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + west + central + urban + lpctmin
        , data = base, alternative = "onesided")
