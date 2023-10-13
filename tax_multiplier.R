# Clear the workspace
rm(list = ls())

# Set the working directory
setwd("C:/Users/Usuario/OneDrive - Económicas - UBA/Valentin/Códigos/R/tax_multiplier")
getwd()

# Install required libraries
#install.packages("vars")
#install.packages("astsa")
#install.packages("tseries")
#install.packages("tidyverse")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("tseries")
#install.packages("forecast")

# Load necessary packages
library(readxl)
library(lubridate)
library(tseries)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(tidyverse)
library(vars)
library(stargazer)
library(vars)
library(astsa)
library(tseries)
library(forecast)

# Import the dataset
data <- read_excel("Fulminante.xlsx", sheet = "IMP")

# Create the primary result + seigniorage variable
data <- data %>%
  mutate(resen = drprim + sen)

View(data)

# Indicate that we are working with time series data
tddpbi <- ts(data$ddpbi, start = c(2004, 1), frequency = 4)
tddgsinint <- ts(data$ddgsinint, start = c(2004, 1), frequency = 4)
tdrprim <- ts(data$drprim, start = c(2004, 1), frequency = 4)
tdiacp <- ts(data$diacp, start = c(2004, 1), frequency = 4)
tdcpriv <- ts(data$dcpriv, start = c(2004, 1), frequency = 4)
tddedtot <- ts(data$ddedtot, start = c(2004, 1), frequency = 4)
tcris <- ts(data$cris, start = c(2004, 1), frequency = 4)
tsen <- ts(data$sen, start = c(2004, 1), frequency = 4)
ttc <- ts(data$tc, start = c(2004, 1), frequency = 4)
tresen <- ts(data$resen, start = c(2004, 1), frequency = 4)

dataa <- cbind(tddpbi, tddgsinint, tdrprim, tdiacp, tdcpriv, tddedtot, tcris, tsen, ttc, tresen)
View(dataa)



# Now we perform the Augmented Dickey-Fuller Unit Root Test
# GDP # It has a unit root, p-value: 0.2271, Dickey-Fuller = -2.8536.
pbit <- adf.test(data$pbi)
print(pbit)

# Exports # It has a unit root, p-value: 0.5407, Dickey-Fuller = -2.0853.
xt <- adf.test(data$x)
print(xt)

# Imports # It has a unit root, p-value: 0.01, Dickey-Fuller = -4.8153.
impt <- adf.test(data$im)
print(impt)

# Monetary Base # It has a unit root, p-value: 0.9461, Dickey-Fuller = -0.90968.
bmt <- adf.test(data$bm)
print(bmt)

# Inflation Tax # No unit root, p-value:, Dickey-Fuller =
tinflt <- adf.test(data$tinfl)
print(tinflt)

# Seigniorage # No unit root, p-value:, Dickey-Fuller =
sent <- adf.test(data$sen)
print(sent)

# Revenues # It has a unit root, p-value: 0.6836, Dickey-Fuller = -1.7353.
ingt <- adf.test(data$ing)
print(ingt)

# Non-interest Public Expenditure # It has a unit root, p-value: 0.804, Dickey-Fuller = -1.4402.
gsinintt <- adf.test(data$gsinint)
print(gsinintt)

# Primary Result # It has a unit root, p-value: 0.09905, Dickey-Fuller = -3.1709.
rprimt <- adf.test(data$rprim)
print(rprimt)

# Interest Expenses # It has a unit root, p-value: 0.09319, Dickey-Fuller = -3.2072.
intt <- adf.test(data$int)
print(intt)

# Financial Result # It has a unit root, p-value: 0.2004, Dickey-Fuller = -2.9191.
resfint <- adf.test(data$resfin)
print(resfint)

# External Debt # No unit root, p-value: 0.01671, Dickey-Fuller = -3.9473.
dedextt <- adf.test(data$dedext)
print(dedextt)

# Internal Debt # No unit root, p-value: 0.2024, Dickey-Fuller = -2.914.
dedintt <- adf.test(data$dedint)
print(dedintt)

# Total Debt # No unit root, p-value: 0.02847, Dickey-Fuller = -3.7247.
dedtott <- adf.test(data$dedtot)
print(dedtott)

# Openness Index # It has a unit root, p-value: 0.6858, Dickey-Fuller = -1.7297.
iacpt <- adf.test(data$iacp)
print(iacpt)

# Public Consumption # It has a unit root, p-value: 0.8232, Dickey-Fuller = -1.3932.
cpubt <- adf.test(data$cpub)
print(cpubt)

# Inflation # No unit root, p-value: 0.08414, Dickey-Fuller = -3.2633.
inflt <- adf.test(data$infl)
print(inflt)

# Exchange Rate # It has a unit root, p-value: 0.99, Dickey-Fuller = 3.4337.
tct <- adf.test(data$tc)
print(tct)

# VAR with Specification (1)
dataaa <- na.omit(dataa)
x <- cbind(tddpbi, tddgsinint)
x2 <- na.omit(x)
model1 <- VAR(x2, p = 1)
coef_matrix <- coef(model1)
var_irf <- irf(model1, impulse = "tddgsinint", response = "tddpbi", ortho = FALSE, n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
plot(var_irf, main = "Figure 4: Real GDP Response to a Shock in Expenditure", xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

x3 <- cbind(tddgsinint, tddpbi)
x4 <- na.omit(x3)
model2 <- VAR(x4, p = 1)
coef_matrix <- coef(model2)
var_irf2 <- irf(model2, impulse = "tddpbi", response = "tddgsinint", ortho = FALSE, n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
plot(var_irf2, main = "Figure 5: Expenditure Response to a Real GDP Shock", xlab = "Quarters", ylab = "Expenditure Proportion", col = "darkgreen", family = "serif")

# VAR with Specification (2)
dataaa <- na.omit(dataa)
y <- cbind(tddpbi, tdrprim)
y2 <- na.omit(y)
model3 <- VAR(y2, p = 1)
coef_matrix <- coef(model3)
var_irf3 <- irf(model3, impulse = "tdrprim", response = "tddpbi", ortho = FALSE, n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
plot(var_irf3, main = "Figure 6: Real GDP Response to a Primary Result Shock", xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

dataaa <- na.omit(dataa)
y3 <- cbind(tdrprim, tddpbi)
y4 <- na.omit(y3)
model4 <- VAR(y4, p = 1)
coef_matrix <- coef(model4)
var_irf4 <- irf(model4, impulse = "tddpbi", response = "tdrprim", ortho = FALSE, n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
plot(var_irf4, main = "Figure 7: Primary Result Response to a Real GDP Shock", xlab = "Quarters", ylab = "Primary Result Proportion", col = "darkgreen", family = "serif")

# VAR with Specification (3)
dataaa <- na.omit(dataa)
z3 <- cbind(tddpbi, tresen)
z4 <- na.omit(z3)
model6 <- VAR(z4, p = 1)
coef_matrix <- coef(model6)
var_irf6 <- irf(model6, impulse = "tresen", response = "tddpbi", ortho = FALSE, n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
plot(var_irf6, main = "Figure 8: Response of Expenditure Financed by Taxes and Seigniorage to a Real GDP Shock", xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

dataaa <- na.omit(dataa)
z <- cbind(tresen, tddpbi)
z2 <- na.omit(z)
model5 <- VAR(z2, p = 1)
coef_matrix <- coef(model5)
var_irf5 <- irf(model5, impulse = "tddpbi", response = "tresen", ortho = FALSE, n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
plot(var_irf5, main = "Figure 9: Real GDP Response to a Shock in Expenditure Financed by Taxes and Seigniorage", xlab = "Quarters", ylab = "Expenditure Proportion", col = "darkgreen", family = "serif")

# SVAR - Blanchard & Quah Decomposition

# Specification 1
model11 <- VAR(x2, p = 2)
SVAR <- BQ(model11)
svar_irf <- irf(SVAR, impulse = "tddgsinint", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
svar_cum_irf <- irf(SVAR, impulse = "tddgsinint", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95, cumulative = TRUE)
plot(svar_irf, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")
plot(svar_cum_irf, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

model12 <- VAR(x2, p = 4)
SVAR2 <- BQ(model12)
svar_irf2 <- irf(SVAR2, impulse = "tddgsinint", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
svar_cum_irf2 <- irf(SVAR2, impulse = "tddgsinint", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95, cumulative = TRUE)
plot(svar_irf2, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")
plot(svar_cum_irf2, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

# Specification 2
model21 <- VAR(y2, p = 4)
SVAR3 <- BQ(model21)
svar_irf3 <- irf(SVAR3, impulse = "tdrprim", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
svar_cum_irf3 <- irf(SVAR3, impulse = "tdrprim", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95, cumulative = TRUE)
plot(svar_irf3, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")
plot(svar_cum_irf3, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

model22 <- VAR(y2, p = 2)
SVAR4 <- BQ(model22)
svar_irf4 <- irf(SVAR4, impulse = "tdrprim", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
svar_cum_irf4 <- irf(SVAR4, impulse = "tdrprim", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95, cumulative = TRUE)
plot(svar_irf4, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")
plot(svar_cum_irf4, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

# Specification 3
model31 <- VAR(z4, p = 4)
SVAR5 <- BQ(model31)
svar_irf5 <- irf(SVAR5, impulse = "tresen", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
svar_cum_irf5 <- irf(SVAR5, impulse = "tresen", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95, cumulative = TRUE)
plot(svar_irf5, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")
plot(svar_cum_irf5, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")

model32 <- VAR(z4, p = 2)
SVAR6 <- BQ(model32)
svar_irf6 <- irf(SVAR6, impulse = "tresen", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95)
svar_cum_irf6 <- irf(SVAR6, impulse = "tresen", response = "tddpbi", n.ahead = 16, boot = TRUE, runs = 1000, ci = 0.95, cumulative = TRUE)
plot(svar_irf6, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")
plot(svar_cum_irf6, xlab = "Quarters", ylab = "Real GDP Proportion", col = "darkgreen", family = "serif")














