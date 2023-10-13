#                                                               Term Paper 3

#                                                               Advanced Econometrics

# Students: Juan Sebastian Navajas Jauregui, Julian Vergara, and Valentin Mannarino

# Start by clearing the environment
rm(list = ls())

# Install the required packages
# install.packages("stargazer")
# install.packages("tidyverse")
# install.packages("AER")

# Load the libraries to be used
library(stargazer)
library(haven)
library(AER)
library(tidyverse)

# Set the working directory
setwd("C:/Users/Usuario/OneDrive - Económicas - UBA/Valentin/Códigos/R/Variables instrumentales")
getwd()

# Import the dataset
data <- read_dta("QOB.dta")
View(data)

# Set decimals to 2 and disable scientific notation
options(scipen = 100)
options(digits = 3)

# SECTION 3 #
# A) Perform descriptive statistics.

# Start by calculating the means of education by QoB
tabla <- data %>%
  group_by(qob) %>%
  summarise(edu = mean(educ, na.rm = T))
stargazer(as.matrix(tabla), type='latex', summary = FALSE, digits = 2)

# Examine the correlations between variables
cor_data <- cor(data)

# Calculate the means of wages by QoB
tabla2 <- data %>%
  group_by(qob) %>%
  summarise(lsal = mean(lwage, na.rm = T))
stargazer(as.matrix(tabla2), type='latex', summary = FALSE, digits = 2)

# Combine both tables
tabla3 <- data %>%
  group_by(qob) %>%
  summarise(edu = mean(educ, na.rm = T),
            lsal = mean(lwage, na.rm = T))
stargazer(as.matrix(tabla3), type='text', summary = FALSE, digits = 2)

# B) Select men born between 1930 and 1939, inclusive.
data <- data %>%
  filter(yob >= 30 & yob <= 39)
View(data)

# Return to education using OLS

attach(data)

lmco <- lm(lwage ~ educ + ageq + ageqsq + race + married + smsa)

stargazer(lmco, type="latex")

# D) Create the instruments

data$z1 <- as.factor(ifelse(data$qob == 1, 1, 0))
data$z2 <- as.factor(ifelse(data$qob == 2, 1, 0))
data$z3 <- as.factor(ifelse(data$qob == 3, 1, 0))

# E) Test the instruments

ltest <- lm(educ ~ z1 + z2 + z3, data = data)
summary(ltest)
stargazer(ltest, type="latex")

# F) Returns to education - instruments from part (d) control ageq, ageqsq, race, married, and smsa

# First stage
attach(data)

etp1 <- lm(educ ~ z1 + z2 + z3 + ageq + ageqsq + race + married + smsa)
summary(etp1)

stargazer(etp1, type="latex")

data$ed_hat <- etp1$fitted.values

# G) Check the first stage

# H) Compare 2SLS, OLS, and IV

# Second stage for 2SLS
attach(data)
etp2 <- lm(lwage ~ ed_hat + ageq + ageqsq + race + married + smsa)
summary(etp2)

# IV
attach(data)

iv <- ivreg(lwage ~ educ + ageq + ageqsq + race + married + smsa |
              z1 + z2 + z3 + ageq + ageqsq + race + married + smsa)
summary(iv)

# Combine all tables
stargazer(lmco, etp1, etp2, iv, type="text",
          column.labels = c("OLS", "First stage", "Second stage", "IV Estimator"),
          dep.var.labels = c("edu", "lwage"), title = "Estimation Results", align = TRUE)
