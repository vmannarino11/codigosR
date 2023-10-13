#                                                         Advanced Econometrics

# Students: Juan Sebastian Navajas Jauregui, Julian Vergara, and Valentin Mannarino

# Clear the environment
rm(list = ls())

# Load libraries
library(openxlsx)
library('ggplot2')
library(readxl)

# Set the working directory
setwd("C:/Users/Usuario/OneDrive - Económicas - UBA/Valentin/Códigos/R/Supuestos Clásicos")
getwd()

# EXERCISE 1
# Given the model: yi = B0 + B1x1i + B2x2i + B3x3i + ui and the coefficients B0 = 300, B1 = 1, B2 = 1, and B3 = -1

# A) Create 50 different datasets, each with 100 observations:
# - Variables x1, x2, and x3 follow a uniform distribution [0,100].
# - The error term follows a normal distribution with E(u) = 0 and s2 = 1400.
# - Define y according to equation 1. Use seed values from 1 to 50, inclusive. Each dataset should have 100 observations of x1, x2, x3, ui, and yi.

# We start by setting the seed from 1 to 50
set.seed(1:50)

# Generate the loop for X1, X2, X3, U, and Y as per the instructions

listabd <- list()
for (i in 1:50) {
  X1 = runif(100, min = 0, max = 100)
  X2 = runif(100, min = 0, max = 100)
  X3 = runif(100, min = 0, max = 100)
  U = rnorm(100, mean = 0, sd = sqrt(1400))
  
  Y = 300 + X1 + X2 - X3 + U
  
  bdatostp <- paste("base_datostp", i, sep = "")
  base_datostp <- data.frame(X1, X2, X3, U, Y)
  listabd[[bdatostp]] <- base_datostp
}

# Verify that they have been created
names(listabd)
View(listabd)

# B) Display the correlation between the independent variables for one of the created datasets.

# Display the correlation for the 2nd dataset
cor(listabd[["base_datostp2"]][, c("X1", "X2", "X3")])

# C) Estimate the parameters (B0, B1, B2, and B3) for the 50 samples and save them in an Excel file called primera_estimacion.xlsx

# Perform the regressions for beta coefficients

coefficients_matrix <- matrix(nrow = length(listabd), ncol = 4)

for (i in 1:length(listabd)) {
  regression_model <- lm(Y ~ X1 + X2 + X3, data = listabd[[i]])
  coefficients_matrix[i, ] <- coefficients(regression_model)
  coefficients_df <- data.frame(coefficients_matrix)
  colnames(coefficients_df) <- c("B0", "B1", "B2", "B3")
  
  write.xlsx(coefficients_df, file = "primera_estimacion.xlsx", row.names = FALSE)
}

# D) Create two graphs based on the primera_estimacion.xlsx file: one for ??1 vs. ??2 and another for ??1 vs. ??3.

# Import the Excel file and name it estim
estim <- read_excel("primera_estimacion.xlsx")
View(estim)

# Correlation between coefficients B1 and B2 and create the graphs
ggplot(data = estim, aes(x = B1, y = B2)) + 
  geom_point(size = 2, shape = 1, color = 'black') + 
  xlab('B1') + 
  ylab('B2') + 
  theme_minimal()

# Correlation between coefficients B1 and B3 and create the graphs
ggplot(data = estim, aes(x = B1, y = B3)) + 
  geom_point(size = 2, shape = 1, color = 'black') + 
  xlab('B1') + 
  ylab('B3') + 
  theme_minimal()

# E) Use code to ensure that x2 is highly correlated with x1.

# Generate 50 new datasets where x1 and x2 are highly correlated using the same seeds.

set.seed(1:50)

list2 <- list()

for (i in 1:50) {
  x1 = runif(100, min = 0, max = 100)
  x2 <- scale(matrix(rnorm(100), ncol = 1))
  xs <- cbind(scale(x1), x2)
  c1 <- var(xs)
  chol1 <- solve(chol(c1))
  newx <- xs
  newc <- matrix(c(1, 0.987, 0.987, 1), ncol = 2)
  eigen(newc)
  chol2 <- chol(newc)
  xm2 <- newx %*% chol2 * sd(x1) + mean(x1)
  x2 <- xm2[, 2]
  x3 = runif(100, min = 0, max = 100)
  u = rnorm(100, mean = 0, sd = sqrt(1400))
  y = 300 + x1 + x2 - x3 + u
  
  bdatostp2 <- paste("base_datostp2", i, sep = "")
  base_datostp2 <- data.frame(x1, x2, x3, u, y)
  list2[[bdatostp2]] <- base_datostp2
}

names(list2)
View(list2)

# F) Estimate B0, B1, B2, and B3 for the 50 new datasets and save the data in an Excel file called segunda_estimacion.xlsx.

coefficients_matrix2 <- matrix(nrow = length(list2), ncol = 4)

for (i in 1:length(list2)) {
  regression_model2 <- lm(y ~ x1 + x2 + x3, data = list2[[i]])
  coefficients_matrix2[i, ] <- coefficients(regression_model2)
  coefficients_df2 <- data.frame(coefficients_matrix2)
  colnames(coefficients_df2) <- c("B0", "B1", "B2", "B3")
  
  write.xlsx(coefficients_df2, file = "segunda_estimacion.xlsx", row.names = FALSE)
}

# G) Load the segunda_estimacion.xlsx file and plot ??1 against ??2.

# Import the dataset
estim2 <- read_excel("segunda_estimacion.xlsx")
View(estim2)

# Create the graphs
ggplot(data = estim2, aes(x = B1, y = B2)) + 
  geom_point(size = 2, shape = 1, color = 'black') + 
  xlab('B1') + 
  ylab('B2') + 
  theme_minimal()

# EXERCISE 2

# A) Using the 50 datasets where x1 and x2 are not highly correlated, estimate the following model:
# yi = B0 + B1x1i + B3x3i + ui
# Save the 50 estimations in an Excel file called tercera_estimacion.xlsx. Plot ??1 against ??3.

coefficients_matrix3 <- matrix(nrow = length(listabd), ncol = 3)

for (i in 1:length(listabd)) {
  regression_model3 <- lm(Y ~ X1 + X3, data = listabd[[i]])
  coefficients_matrix3[i, ] <- coefficients(regression_model3)
  coefficients_df3 <- data.frame(coefficients_matrix3)
  colnames(coefficients_df3) <- c("B0", "B1", "B3")
  
  write.xlsx(coefficients_df3, file = "tercera_estimacion.xlsx", row.names = FALSE)
}

estim3 <- read_excel("tercera_estimacion.xlsx")
View(estim3)

ggplot(data = estim3, aes(x = B1, y = B3)) + 
  geom_point(size = 2, shape = 1, color = 'black') + 
  xlab('B1') + 
  ylab('B3') + 
  theme_minimal()

# B) Using the 50 datasets where x1 and x2 are highly correlated, estimate the new model.
# Save the 50 estimations in an Excel file called cuarta_estimacion.xlsx. Plot ??1 against ??3.

coefficients_matrix4 <- matrix(nrow = length(list2), ncol = 3)

for (i in 1:length(list2)) {
  regression_model4 <- lm(y ~ x1 + x3, data = list2[[i]])
  coefficients_matrix4[i, ] <- coefficients(regression_model4)
  coefficients_df4 <- data.frame(coefficients_matrix4)
  colnames(coefficients_df4) <- c("B0", "B1", "B3")
  
  write.xlsx(coefficients_df4, file = "cuarta_estimacion.xlsx", row.names = FALSE)
}

estim4 <- read_excel("cuarta_estimacion.xlsx")
View(estim4)

ggplot(data = estim4, aes(x = B1, y = B3)) + 
  geom_point(size = 2, shape = 1, color = 'black') + 
  xlab('B1') + 
  ylab('B3') + 
  theme_minimal()

