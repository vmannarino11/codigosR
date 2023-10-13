# PRACTICAL WORK
# PRODUCTIVITY: EVIDENCE AND PUBLIC POLICY

# Professor: Pablo José Sanguinetti

# Students: Bautista Menta, Estefanía Walker, Valentín Mannarino

rm(list = ls())

setwd("C:/Users/Usuario/OneDrive - Económicas - UBA/Valentin/Códigos/R/Productividad")
getwd()

# Install Packages (if needed)
# install.packages("plotly")
# install.packages("cowplot")
# install.packages("gridExtra")
# install.packages("xtable")

# Load Required Packages
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
library(haven)
library(plotly)
library(tseries)
library(ggplot2)
library(dplyr)
library(cowplot)
library(gridExtra)
library(xtable)

# Load the Database
datos <- read_dta("C:/Users/Usuario/OneDrive - Económicas - UBA/Valentin/Códigos/R/Productividad/endei_ii_anonimizada_junio_2019.dta")

# Create a new data frame without missing values in the specific variable
datos1 <- datos[complete.cases(datos$va_tr16), ]

# (a) Calculate labor productivity by firm level using value added per worker for the year 2016
prod_trab_2016 <- datos1$va_tr16

# (b) Calculate dispersion and plot the distribution of the estimated labor productivity indicator.

# Create a new data frame without missing values in the specific variable
datos$rama <- as.numeric(substr(datos$rama_act, 1, 2))
datos$rama <- ifelse(datos$rama == 99, 9999, datos$rama)
va_sector <- data.frame(datos$ide_endei_ii, datos$rama, datos$va_tr16)

datos_filtered <- va_sector %>%
  filter_all(all_vars(. != 9999))
n_distinct(datos_filtered$datos.rama)

# Assign names to each sector
nombres_rama_act <- data.frame(
  ind2 = c(15, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 33, 34, 35, 36),
  nombre_rama = c("Alimentos", "Textil", "Confecciones", "Cuero", "Madera",
                  "Papel", "Edición", "Químicos", "Caucho y Plástico", "Minerales no metálicos",
                  "Metales comunes", "Otros productos de metal", "Maquinaria y equipo",
                  "Electricidad y televisión", "Instrumentos médicos", "Vehículos y autopartes",
                  "Otros equipos de transporte", "Muebles")
)

# Combine the filtered dataframe with sector names
datos_filtrados_con_nombres <- merge(datos_filtered, nombres_rama_act, by.x = "datos.rama", by.y = "ind2", all.x = TRUE)

va_por_sector <- datos_filtrados_con_nombres

# Filter out outlier values
va_por_sector_filtrado <- va_por_sector %>%
  filter(datos.va_tr16 < quantile(datos.va_tr16, 0.99))

# Calculate dispersion (standard deviation) by subsector
dispersion_por_subsector <- va_por_sector_filtrado %>%
  group_by(nombre_rama) %>%
  summarise(dispersion = sd(datos.va_tr16))

# Order subsectors by their standard deviation in descending order
dispersion_por_subsector <- dispersion_por_subsector %>%
  arrange(desc(dispersion))

# Create two density distribution plots for the labor productivity indicator for each subsector
grafico_distribucion <- ggplot(va_por_sector_filtrado, aes(x = datos.va_tr16, fill = nombre_rama)) +
  geom_density(alpha = 0.6) +
  labs(x = "Labor Productivity Indicator", y = "Density") +
  ggtitle("Distribution of Labor Productivity Indicator by Subsector") +
  theme_minimal() +
  theme(legend.position = "right")

grafico_distribucion2 <- ggplot(va_por_sector_filtrado, aes(x = log(datos.va_tr16), fill = factor(nombre_rama))) +
  geom_density(alpha = 1) +
  xlab("Log (Productivity)") +
  ylab("Density") +
  ggtitle("Distribution of the Labor Productivity Indicator") +
  scale_fill_discrete(name = "Industry") +
  facet_wrap(~ factor(nombre_rama))

# Print the subsector dispersion table and the distribution plots
print(dispersion_por_subsector)
print(grafico_distribucion)
print(grafico_distribucion2)

# (c) Decompose productivity within each subsector

# Filter out sectors with a 9999 code
datos1 <- datos1 %>%
  filter(rama_act != 9999)

# Combine the dataframes using the "ide_endeii_ii" column as the link
# Rename the column "datos.ide_endei_ii" to "ide_endei_ii" in the va_por_sector DataFrame
va_por_sector <- va_por_sector %>%
  rename(ide_endei_ii = datos.ide_endei_ii)

datos_combinados <- datos1 %>%
  merge(va_por_sector, by = "ide_endei_ii")

# Prepare the data frame for the formula

# Step 1: Calculate va_ij (Value added by firm i in subsector j)
datos_combinados <- datos_combinados %>%
  group_by(nombre_rama, ide_endei_ii) %>%
  mutate(va_ij = va16)

# Step 2: Calculate va_tr_ij (Total value added of all firms in subsector j)
datos_combinados <- datos_combinados %>%
  group_by(nombre_rama) %>%
  mutate(va_j = sum(va16))

# Step 3: Calculate n_ij (Number of employees of firm i in subsector j)
datos_combinados <- datos_combinados %>%
  mutate(n_ij = va_ij / va_j)

# Step 4: Calculate theta_ij (Participation in employment/value added by firm i in subsector j)
datos_combinados <- datos_combinados %>%
  mutate(theta_ij = n_ij)

# Step 6: Calculate theta_bar (Average participation in employment/value added of all firms in subsector j)
datos_combinados <- datos_combinados %>%
  group_by(nombre_rama) %>%
  mutate(theta_bar = mean(n_ij))

# Step 7: Calculate theta_j (Participation in employment/value added of all firms in subsector j)
datos_combinados <- datos_combinados %>%
  group_by(nombre_rama) %>%
  mutate(theta_j = sum(n_ij))

# Create a data frame containing the necessary variables for decomposition
datos_formula <- datos_combinados %>%
  select(nombre_rama, ide_endei_ii, va_ij, va_j, n_ij, theta_ij, theta_bar, theta_j)

# Now, let's build the rest of the variables

# Step 1: Calculate the logarithm of the productivity of each firm (p_ij)
datos_formula <- datos_formula %>%
  mutate(p_ij = log(va_ij))

# Step 2: Calculate the logarithm of the unweighted average of firm productivity in each subsector (p_j_bar)
datos_formula <- datos_formula %>%
  group_by(nombre_rama) %>%
  mutate(p_j_bar = mean(p_ij))

# Step 3: Calculate the deviation of theta_ij and p_ij from their averages
datos_formula <- datos_formula %>%
  group_by(nombre_rama) %>%
  mutate(
    desviacion_theta_ij = theta_ij - theta_bar,
    desviacion_p_ij = p_ij - p_j_bar
  )

# Step 4: Calculate the covariance between employment participation and firm productivity in each subsector
datos_formula <- datos_formula %>%
  group_by(nombre_rama) %>%
  mutate(
    covarianza_empleo_productividad = sum(desviacion_theta_ij * desviacion_p_ij)
  )

# Step 5: Calculate the weighted firm productivity in each subsector (P_j)
datos_formula <- datos_formula %>%
  group_by(nombre_rama)%>%
  mutate(P_j = log(sum(va_ij) / sum(n_ij)))
         
# Step 6: Calculate the labor productivity of each subsector
datos_formula <- datos_formula %>%
  group_by(nombre_rama)%>%
  mutate(productividad_trabajo_OP = p_j_bar + covarianza_empleo_productividad)
       
# Step 7: Check if the formula is correct
datos_formula <- datos_formula %>%
  group_by(nombre_rama)%>%
  mutate(check = P_j - p_j_bar - covarianza_empleo_productividad)

Prop_malasasig <- datos_formula %>%
  select(nombre_rama, P_j,p_j_bar, covarianza_empleo_productividad)

Prop_malasasig <- Prop_malasasig %>%
  distinct(nombre_rama, .keep_all = TRUE)

Prop_malasasig <- Prop_malasasig%>%
  mutate(Ratio_ProdProm = p_j_bar/P_j)

Prop_malasasig <- Prop_malasasig%>%
  mutate(Ratio_MalaAsign = 1-Ratio_ProdProm)

Prop_malasasig <- Prop_malasasig %>%
  arrange(desc(P_j))

Prop_malasasig_disp <- merge(Prop_malasasig, dispersion_por_subsector, by = "nombre_rama")

print(Prop_malasasig)
         
# Export to LaTeX
tabla_latex <- xtable(Prop_malasasig)
print(tabla_latex, include.rownames = FALSE)
         
# Create a plot showing the relationship between resource allocation and subsector productivity
ggplot(Prop_malasasig, aes(x = P_j, y = covarianza_empleo_productividad, label = nombre_rama)) +
           geom_point() +
           geom_smooth(method = "lm", se = FALSE, color = "blue") +
           geom_text(nudge_y = 0.08, size = 3) +
           labs(
             x = "Productivity (in logs)",
             y = "Allocation Measure (Covariance Term)",
             title = "Relationship between Resource Allocation and Sectoral Productivity"
           ) +
           theme_minimal()
         
# Create a plot showing the relationship between productivity and dispersion
ggplot(Prop_malasasig_disp, aes(x = P_j, y = dispersion, label = nombre_rama)) +
           geom_point() +
           geom_smooth(method = "lm", se = FALSE, color = "blue") +
           geom_text(nudge_y = 0.08, size = 3) +
           labs(
             x = "Productivity (in logs)",
             y = "Sectoral Dispersion",
             title = "Relationship between Sectoral Dispersion and Sectoral Productivity"
           ) +
           theme_minimal()
         
# (d) Decompose the productivity of the entire manufacturing sector using the OP formula
# Prepare the data frame for the formula
# Step 1: Create the variable ct_j (sum of deviations of theta_ij and theta_j; plus the deviation between p_ij and p_j)
# Deviation of theta_ij from theta_j
datos_formula <- datos_formula %>%
       mutate(thetares = (theta_ij - theta_j))
         
# Deviation of p_ij from the mean
datos_formula <- datos_formula %>%
           mutate(pres = (p_ij - p_j_bar))
         
# Calculate ct_j -> It's a value for each subsector
datos_formula <- datos_formula %>%
           mutate(ct_j = sum(thetares * pres))
         
#Step 2: Create P_bar (The unweighted average productivity of all subsectors -> a single number)
promedio_total <- mean(datos_formula$P_j, na.rm = TRUE)
         
datos_formula <- datos_formula %>%
           mutate(P_bar = promedio_total)
         
# Step 3: Create CT (sum of deviations of theta_j and theta_bar; plus the deviation between P_j and P_bar)
       
# Deviation of theta_j from the mean -> It's a value for each subsector
datos_formula <- datos_formula %>%
           mutate(thetares2 = (theta_j - theta_bar))
         
# Deviation of P_j from the mean -> It's a value for each subsector
datos_formula <- datos_formula %>%
           mutate(Pres2 = (P_j - P_bar))
         
# Calculate CT -> It's a value for each subsector
CT <- sum(datos_formula$thetares2 * datos_formula$Pres2)
         datos_formula['CT'] = CT
         
# Step 4: Calculate the average of ct_j
mean_ct_J <- mean(datos_formula$ct_j, na.rm = TRUE)
         
# Add the result to the data frame as a new column called 'mean_ct_J'
datos_formula['mean_ct_J'] = mean_ct_J
         
# Step 5: Calculate the average of p_j_bar
mean_pj <- mean(datos_formula$p_j_bar, na.rm = TRUE)
         
# Add the result to the data frame as a new column called 'mean_pj'
datos_formula['mean_pj'] = mean_pj
         
# Step 6: Calculate the labor productivity of the manufacturing sector. Pm = CT + sum ct_j / j + sum p_j_bar / j
datos_formula <- datos_formula %>%
mutate(P_m = CT + mean_ct_J + mean_pj)
         



