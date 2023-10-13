#                                                               Term Paper 4

#                                                               Advanced Econometrics

# Students: Juan Sebastian Navajas Jauregui, Julian Vergara, and Valentin Mannarino

# Start by clearing the environment
rm(list = ls())

# Install the packages
# install.packages("mfx") 
# install.packages("margins") 

# Load the libraries
library(openxlsx)
library(mfx) 
library(margins) 
library(ggplot2) 
library(stargazer)
library(tidyverse)
library(readxl)
library(haven) 
library(AER) 
library(dplyr)

# Set the working directory
setwd("C:/Users/Usuario/OneDrive - Económicas - UBA/Valentin/Códigos/R/Eph")
getwd()

# Import the dataset
data <- read_xlsx("Data.xlsx")
View(data)

# Remove the INTENSI variable and missing values
data$INTENSI <- NULL
data = na.omit(data)

# Categorical variables
data$AGLOMERADO <- factor(data$AGLOMERADO)
data$CH14 <- factor(data$CH14)
data$CH07 <- factor(data$CH07)
data$CH08 <- factor(data$CH08)
data$CH10 <- factor(data$CH10)
data$CH11 <- factor(data$CH11)
data$CH13 <- factor(data$CH13)
data$educ_jefe <- factor(data$educ_jefe)
data$CAT_OCUP <- factor(data$CAT_OCUP)
data$CAT_INAC <- factor(data$CAT_INAC)
data$NIVEL_ED <- factor(data$NIVEL_ED)
data$ESTADO <- factor(data$ESTADO)

# Create three categories for the head of household's education. Up to primary - up to tertiary - More than university
data <- data %>%
  mutate(educ_jefe_2 = case_when(
    educ_jefe %in% c("1", "2", "3", "9") ~ "1",
    educ_jefe %in% c("4", "5") ~ "2",
    educ_jefe %in% c("6","7", "8") ~ "3"
  ))

data$educ_jefe_2 <- factor(data$educ_jefe_2)

# POINT A

# Calculate the correlation matrix
varint <- data[, c("deserta", "jmujer", "IPCF", "hermanos", "CH06", "mujer")]
matriz_cor <- cor(varint)
print(matriz_cor)
stargazer(as.matrix(matriz_cor), type='latex', summary = FALSE, digits = 4)

# Display a table for deserta
tabla1 <- data %>%
  group_by(jmujer) %>%
  summarise(deserta = mean(deserta))
stargazer(as.matrix(tabla1), type='latex', summary = FALSE, digits = 2)

# Display a table for deserta based on educ_jefe_2
tabla2 <- data %>%
  group_by(educ_jefe_2) %>%
  summarise(deserta = mean(deserta))
stargazer(as.matrix(tabla2), type = 'latex', summary = FALSE, digits = 2, 
          align = TRUE, header = FALSE, no.space = TRUE)

# POINT B
probit <- glm(deserta ~ mujer + jmujer + hermanos + educ_jefe_2 + IPCF + CH06, 
              family = binomial(link = "probit"), data = data)
stargazer(probit, type="latex", summary = FALSE, digits = 2, 
          align = TRUE, header = FALSE, no.space = TRUE)

# POINT C

# Marginal effects. Start by evaluating at the mean. Calculate how the probability changes when each variable increases by one percentage point.

ma_med <- probitmfx(deserta ~ mujer + jmujer + hermanos + educ_jefe_2 + IPCF + CH06, 
                    data = data, atmean = TRUE, robust = TRUE) 
names(ma_med)
stargazer(ma_med$mfxest, type='text', summary = FALSE, digits = 2, 
          align = TRUE, header = FALSE, no.space = TRUE)

# Evaluate at relevant points

ef1 <- margins(probit, at = list(IPCF = c(mean(data$IPCF), 
                                          min(data$IPCF),
                                          3330, 8900, 17200, 28800, 
                                          max(data$IPCF))))
ef11 <- as.data.frame(ef1) %>%
  filter(IPCF %in% c(mean(data$IPCF), 
                     min(data$IPCF),
                     3330, 8900, 17200, 28800,
                     max(data$IPCF))) %>%
  select(IPCF, starts_with("dydx")) %>%
  group_by(IPCF) %>%
  summarise_all(mean)
stargazer(t(ef11), summary = FALSE, digits = 3, type = "latex")

# POINT D

# Create a linear model
lm <- glm(deserta ~ mujer + jmujer + hermanos + educ_jefe_2 + IPCF + CH06, data = data)
stargazer(lm, type="latex")

# POINT E

# Set the IPCF variable to 1 for values less than 1
data$IPCF[data$IPCF < 1] <- 1
# Create the variable
data$ln_ing <- log(data$IPCF)

# POINT F - Relationship between the ln of income and the probability of dropping out. Choose a typical individual.

str(data)

# Typical individual - male with a male head of household:
pred <- with(data, data.frame(hermanos = median(hermanos), 
                              CH06 = median(CH06), 
                              ln_ing=data$ln_ing , 
                              mujer="0", jmujer="0", 
                              educ_jefe_2="1",
                              IPCF = median(IPCF)))
pred$mujer <- as.numeric(pred$mujer)
pred$jmujer <- as.numeric(pred$jmujer)

estimo <- glm(deserta ~ mujer + jmujer + hermanos + ln_ing + educ_jefe_2 + IPCF + CH06, family = binomial(link = "probit"), 
              data = data)

predict(estimo, pred, type="response")

pred[, "prediction"] <- predict(estimo, pred, type = "response")

ggplot(pred) +
  geom_line( aes(x = ln_ing, y = prediction), linewidth = 1.5) +
  theme(axis.text = element_text(family = "serif"), axis.title = element_text(family = "serif"),
        panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey", 
                                                                                   linetype = "dashed")) +
  xlab("Income per capita (logarithm)") +
  ylab("Prediction")

# Typical individual - female with a male head of household:

pred2 <- with(data, data.frame(hermanos = median(hermanos), 
                               CH06 = median(CH06), 
                               ln_ing=data$ln_ing , 
                               mujer="1", jmujer="0", 
                               educ_jefe_2="1",
                               IPCF = median(IPCF)))
pred2$mujer <- as.numeric(pred2$mujer)
pred2$jmujer <- as.numeric(pred2$jmujer)

predict(estimo, pred2, type="response")

pred2[, "prediction"] <- predict(estimo, pred2, type = "response")

ggplot(pred2) +
  geom_line( aes(x = ln_ing, y = prediction), linewidth = 1.5,) +
  theme(axis.text = element_text(family = "serif"), axis.title = element_text(family = "serif"), 
        panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey", 
                                                                                   linetype = "dashed")) +
  xlab("Income per capita (logarithm)") +
  ylab("Prediction")

# Typical individual - male with a female head of household:

pred3 <- with(data, data.frame(hermanos = median(hermanos), 
                               CH06 = median(CH06), 
                               ln_ing=data$ln_ing , 
                               mujer="0", jmujer="1", 
                               educ_jefe_2="1",
                               IPCF = median(IPCF)))
pred3$mujer <- as.numeric(pred3$mujer)
pred3$jmujer <- as.numeric(pred3$jmujer)

predict(estimo, pred3, type="response")

pred3[, "prediction"] <- predict(estimo, pred3, type = "response")

ggplot(pred3) +
  geom_line( aes(x = ln_ing, y = prediction), linewidth = 1.5,) +
  theme(axis.text = element_text(family = "serif"), axis.title = element_text(family = "serif"),
        panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey", 
                                                                                   linetype = "dashed")) +
  xlab("Income per capita (logarithm)") +
  ylab("Prediction")

# Typical individual - female with a female head of household:

pred4 <- with(data, data.frame(hermanos = median(hermanos), 
                               CH06 = median(CH06), 
                               ln_ing=data$ln_ing , 
                               mujer="1", jmujer="1", 
                               educ_jefe_2="1",
                               IPCF = median(IPCF)))
pred4$mujer <- as.numeric(pred4$mujer)
pred4$jmujer <- as.numeric(pred4$jmujer) 

predict(estimo, pred4, type="response")

pred4[, "prediction"] <- predict(estimo, pred4, type = "response")

ggplot(pred4) +
  geom_line( aes(x = ln_ing, y = prediction), linewidth = 1.5,) +
  theme(axis.text = element_text(family = "serif"), axis.title = element_text(family = "serif"),
        panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey", 
                                                                                   linetype = "dashed")) +
  xlab("Income per capita (logarithm)") +
  ylab("Prediction")

# Final table
stargazer(lm, probit, estimo, type="latex",
          column.labels = c("Linear Probability Model","Probit", "Probit with Ln"),
          dep.var.labels = c("deserta"), title = "Estimation Results", align = TRUE)
