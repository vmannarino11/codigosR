#                                                      TERM PAPER 

#                                                  Advanced Macroeconomics 

# Students: Juan Sebastian Navajas Jauregui, Julian Vergara, and Valentin Mannarino

#Clear the environment 
rm(list = ls())

#EXERCISE 3)

#Part 1

#n represents the population growth rate. We take the 1985-2021 average from the World Bank. Life expectancy in Argentina is 76 years, also from the World Bank. Since we use a two-generation OLG model, we take the Young as the first 38 years.
# alpha is the capital share of income. The data is extracted from the "Income Generation and Labor Input Account" report of INDEC published on January 19, 2023.
#rho is the discount factor. The parameter data is from the paper "A Subtle and Not Always Understood Link in Half a Century: A Note on the Investment Rate and Economic Growth" by Sebastian Katz, Luis Lanteri, Sebastian Vargas (2007).
#y is the per capita product from 1985-2021 at constant prices in dollars. Extracted from the World Bank's website.
#Sections 4 and 5
#We start by setting the variables:
n <- 0.01174
alpha <- 0.461
rho <- 0.037
y <- 14038

#For the discount factor, we'll take the centered value of 0.037 and the two extremes proposed in the paper:
rho2 <- 0.026
rho3 <- 0.048

#Now, we calculate the technology parameter, also known as A.
A <- y^(1-alpha)*(((2+rho)*(1+n))/(1-alpha))^alpha
A2 <- y^(1-alpha)*(((2+rho2)*(1+n))/(1-alpha))^alpha
A3 <- y^(1-alpha)*(((2+rho3)*(1+n))/(1-alpha))^alpha

#We observe that changes in rho do not significantly affect our A. The value of A, taking the centered rho, is 319.09.
#We replace the values of our parameters in the Steady State capital formula:
K <- ((A*(1-alpha))/((2+rho)*(1+n)))^(1/(1-alpha))
K2 <- ((A2*(1-alpha))/((2+rho2)*(1+n)))^(1/(1-alpha))
K3 <- ((A3*(1-alpha))/((2+rho3)*(1+n)))^(1/(1-alpha))

#Again, changes in A due to changes in rho do not result in significant changes. The Steady State per capita capital in constant dollars is $3,671 for our centered values of rho and A.
#We verify that the Steady State per capita product is:
Y <- A*K^alpha
Y2 <- A2*K^alpha
Y3 <- A3*K^alpha

#As expected, the Steady State per capita product in constant dollars, using centered values, is $14,038.
#Part 6
#To check dynamic efficiency or inefficiency, we'll compare n, our population growth rate, with r, the marginal productivity of capital. We can do this by working with an OLG model.
r <- A * alpha * K ^ (alpha - 1)

#r=1.762674 
r > n
#TRUE

#We find that the marginal productivity of capital is greater than the population growth rate. Thus, we are in dynamic efficiency, as expected in Argentina. Dynamic efficiency means that capital is scarce, resulting in a high return.
#Part 7
#We'll work with pension incomes for the period 2019-2021 in millions of Argentine pesos. Data source: ANSeS 2021 Financial Report.
p2019 <- 1878302
p2020 <- 2266628
p2021 <- 3359886
p2022 <- 6021073

#To observe its real decline, we'll create an index based on 1 in 2019 and adjust it for inflation announced by INDEC to deflate the data.
def2020 <- 1 * (1 + 0.361)
def2021 <- def2020 * (1 + 0.509)
def2022 <- def2021 * (1 + 0.9480)

#We create the deflated values:
Ip2019def <- p2019/1
Ip2020def <- p2020/def2020
Ip2021def <- p2021/def2021
Ip2022def <- p2022/def2022

#We present the data in a matrix:
matrix <- matrix(c(Ip2019def, Ip2020def, Ip2021def, Ip2022def), nrow=4, ncol=1)
rows <- c("2019", "2020", "2021", "2022")
rownames(matrix) <- rows
colnames(matrix) <- c("Pension Income")
table <- as.table(matrix)
table

#2019             1878302
#2020             1665414
#2021             1635977
#2022             1505004

#In this table, we can observe how pension incomes decreased in real terms.
#Part 8
#For this part, since we can't solve it directly, we'll use the Newton-Raphson method, which involves choosing an initial guess for K and then iterating until a convergent solution is reached.
#We'll consider A, rho, and alpha as constants. The new value of n depends on population growth from 2019-2021 from the World Bank:
n2 <- 0.009392

#We define pension transfers as:
b0 = (1 + n2) * Ip2019def
b1 = (1 + n2) * Ip2020def
b2 = (1 + n2) * Ip2021def
b3 = (1 + n2) * Ip2022def

#Following the theoretical model for this topic, if contributions fall, as observed during 2019-2022, individual and aggregate savings of the economy decrease. Therefore, since savings is in the numerator of the capital accumulation definition (K = s/1+N), we expect capital accumulation to decrease.
#We formulate the capital equation:
f1 <- function(k_l) {
  (A*(k_l^alpha)*(1-alpha))/((2+rho)*(1+n2)) - (b0*(1+A*(k_l^(alpha-1))*alpha) + (1+rho)*b1)/(((1+n2)*(2+rho))*(1+alpha*A*(k_l^(alpha-1))))
}

#We take the derivative:
f2 <- function(k_l) { 
  ((A*(k_l^(alpha-1))*(1-alpha)*alpha)/(2+rho)*(1+n2)) + (((alpha-1))*(1+rho)*b1*(k_l^(-1)))/((2+rho)*(1+n2)*(1+A*alpha*(k_l^(alpha-1)))) - 1
}

#We set the initial values in Argentine pesos (official average exchange rate buy-sell December 2019) based on the Steady State per capita capital obtained:
k_l <- 3500 * 60.89
tol <- 1e-6

# Newton-Raphson method iterations
while (abs(f1(k_l)) > tol) {
  k_l <- (k_l - f1(k_l)) / f2(k_l)
}


#View the result
cat("The value of k_l that satisfies the equation is:", k_l)

#Since the model doesn't have a closed solution, we attempted to use the Newton-Raphson approximation method, but it wasn't successful. However, being in a PAYG system, we expect that a decrease in contributions (in real terms) should result in an increase in individual and aggregate savings in the economy, which should translate into an increase in the capital stock for the next period.

#References consulted
#Katz, S., Lanteri, L., & Vargas, S. (2007). Un vínculo sutil y no siempre comprendido en medio siglo: una nota sobre la tasa de inversión y el crecimiento económico.
#Sala-i-Martin, X. (2000). Apuntes de crecimiento económico. Antoni Bosch Editor.
#Campante, F., Sturzenegger, F., & Velasco, A. (2021). Advanced macroeconomics: an easy guide (p. 418). LSE Press.
#https://www.anses.gob.ar/estado-financiero
#https://data.worldbank.org/indicator/NY.GDP.PCAP.KN?locations=AR
#https://data.worldbank.org/indicator/sp.pop.grow?locations=AR
#https://www.indec.gob.ar/indec/web/Institucional-Indec-InformesTecnicos
