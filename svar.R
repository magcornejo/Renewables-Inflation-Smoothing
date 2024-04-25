#===============================================================================
# Description: Performs Structural VAR estimations to identify the structural 
# shocks on prices that will be used for the panel VAR.
#===============================================================================

#===============================================================================
# 1). Preliminary ------
#===============================================================================


# Clean up workspace
rm(list=ls())

# Load packages
wants <- c("readxl","zoo","vars","svars","urca","ggplot2","gridExtra")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)
require("parallel") # base package

# Directories
dir <- list()
dir$root <- dirname(getwd())
dir$shocks <- paste0(dir$root,"/Structural shocks identifications")

# Import time series datasets
crude_oil <- read_xlsx(paste0(dir$shocks,"/crude_oil.xlsx"), sheet = "monthly")
crude_oil <- read.zoo(crude_oil, fomat = "%Y-%m-%d")
natural_gas <- read_xlsx(paste0(dir$shocks,"/natural_gas.xlsx"), sheet = "monthly")
natural_gas <- read.zoo(natural_gas, fomat = "%Y-%m-%d")

# Data transformation
crude_oil$lprod <- log(crude_oil$production)
crude_oil$dlprod <- diff(crude_oil$lprod)*100
crude_oil$lprice <- log(crude_oil$wti/crude_oil$uscpi)
crude_oil$dlprice <- diff(crude_oil$lprice)*100
crude_oil$rea <- diff(crude_oil$igrea)
crude_oil$d12lprod <- (crude_oil$lprod-lag(crude_oil$lprod,12))*100
crude_oil$d12lprice <- (crude_oil$lprice-lag(crude_oil$lprice,12))*100
crude_oil$d12igrea <- (crude_oil$igrea-lag(crude_oil$igrea,12))*100
natural_gas$lprod <- log(natural_gas$usprod)
natural_gas$dlprod <- diff(natural_gas$lprod)*100
natural_gas$lprice <- log(natural_gas$henryhub/natural_gas$uscpi)
natural_gas$dlprice <- diff(natural_gas$lprice)*100
natural_gas$rea <- diff(natural_gas$igrea)
natural_gas$d12lprod <- (natural_gas$lprod-lag(natural_gas$lprod,12))*100
natural_gas$d12lprice <- (natural_gas$lprice-lag(natural_gas$lprice,12))*100
natural_gas$d12igrea <- (natural_gas$igrea-lag(natural_gas$igrea,12))*100

# Starting en 2003M01
crude_oil <- crude_oil[index(crude_oil)>"2002-12-01"]
natural_gas <- natural_gas[index(natural_gas)>"2002-12-01"]

# Plots (in levels / log-levels)
plot.zoo(crude_oil$lprod, xlab = "Month", ylab = "log(production)")
plot.zoo(crude_oil$igrea, xlab = "Month", ylab = "IGREA")
plot.zoo(crude_oil$lprice, xlab = "Month", ylab = "log(price)")
plot.zoo(natural_gas$lprod, xlab = "Month", ylab = "log(production)")
plot.zoo(natural_gas$igrea, xlab = "Month", ylab = "IGREA")
plot.zoo(natural_gas$lprice, xlab = "Month", ylab = "log(price)")


# Plots (in differences / log-differences)
plot.zoo(crude_oil$dlprod, xlab = "Month", ylab = "diff(log(production))")
plot.zoo(crude_oil$rea, xlab = "Month", ylab = "diff(IGREA)")
plot.zoo(crude_oil$dlprice, xlab = "Month", ylab = "diff(log(price))")
plot.zoo(natural_gas$dlprod, xlab = "Month", ylab = "diff(log(production))")
plot.zoo(natural_gas$rea, xlab = "Month", ylab = "diff(IGREA)")
plot.zoo(natural_gas$dlprice, xlab = "Month", ylab = "diff(log(price))")

plot.zoo(crude_oil$d12lprod, xlab = "Month", ylab = "diff(log(production))")
plot.zoo(crude_oil$d12igrea, xlab = "Month", ylab = "diff(IGREA)")
plot.zoo(crude_oil$d12lprice, xlab = "Month", ylab = "diff(log(price))")
plot.zoo(natural_gas$d12lprod, xlab = "Month", ylab = "diff(log(production))")
plot.zoo(natural_gas$d12igrea, xlab = "Month", ylab = "diff(IGREA)")
plot.zoo(natural_gas$d12lprice, xlab = "Month", ylab = "diff(log(price))")

# Unit root tests
summary(ur.df(crude_oil$lprod, type = "trend", selectlags = "BIC")) #I(1) 
summary(ur.df(crude_oil$igrea, type = "trend", selectlags = "BIC")) #I(0) at 5%
summary(ur.df(crude_oil$lprice, type = "trend", selectlags = "BIC")) #I(1) 
summary(ur.df(natural_gas$lprod, type = "trend", selectlags = "BIC")) #I(0) 
summary(ur.df(natural_gas$igrea, type = "trend", selectlags = "BIC")) #I(0) at 5%
summary(ur.df(natural_gas$lprice, type = "trend", selectlags = "BIC")) #I(1)

summary(ur.df(na.omit(crude_oil$dlprod), type = "drift", selectlags = "BIC")) #I(0)
summary(ur.df(na.omit(crude_oil$dlprice), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(crude_oil$rea), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(natural_gas$dlprod), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(natural_gas$dlprice), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(natural_gas$rea), type = "drift", selectlags = "BIC")) #I(0) 

summary(ur.df(na.omit(crude_oil$d12lprod), type = "trend", selectlags = "BIC")) #I(1) 
summary(ur.df(na.omit(crude_oil$d12igrea), type = "trend", selectlags = "BIC")) #I(0) at 5%
summary(ur.df(na.omit(crude_oil$d12lprice), type = "trend", selectlags = "BIC")) #I(1) 
summary(ur.df(na.omit(natural_gas$d12lprod), type = "trend", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(natural_gas$d12igrea), type = "trend", selectlags = "BIC")) #I(0) at 5%
summary(ur.df(na.omit(natural_gas$d12lprice), type = "trend", selectlags = "BIC")) #I(1)


summary(ur.pp(crude_oil$lprod, type = "Z-tau", model = "trend", lags = "short"))  #I(1) 
summary(ur.pp(crude_oil$igrea, type = "Z-tau", model = "trend", lags = "short")) #I(1)
summary(ur.pp(crude_oil$lprice, type = "Z-tau", model = "trend", lags = "short")) #I(1) 
summary(ur.pp(natural_gas$lprod, type = "Z-tau", model = "trend", lags = "short"))  #I(0) 
summary(ur.pp(natural_gas$igrea, type = "Z-tau", model = "trend", lags = "short")) #I(1)
summary(ur.pp(natural_gas$lprice, type = "Z-tau", model = "trend", lags = "short")) #I(1) at 1%

summary(ur.pp(crude_oil$dlprod, type = "Z-tau", model = "constant", lags = "short"))  #I(0) 
summary(ur.pp(crude_oil$dlprice, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(crude_oil$rea, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(natural_gas$dlprod, type = "Z-tau", model = "constant", lags = "short"))  #I(0)
summary(ur.pp(natural_gas$dlprice, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(natural_gas$rea, type = "Z-tau", model = "constant", lags = "short")) #I(0) 

summary(ur.pp(crude_oil$d12lprod, type = "Z-tau", model = "constant", lags = "short"))  #I(0) 
summary(ur.pp(crude_oil$d12lprice, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(crude_oil$d12igrea, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(natural_gas$d12lprod, type = "Z-tau", model = "constant", lags = "short"))  #I(0)
summary(ur.pp(natural_gas$d12lprice, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(natural_gas$d12igrea, type = "Z-tau", model = "constant", lags = "short")) #I(0) 


#===============================================================================
# 2). SVAR estimation ------
#===============================================================================

# CRUDE OIL

series <- cbind(crude_oil$dlprod,crude_oil$rea,crude_oil$dlprice)
colnames(series) <- c("dlprod","rea","dlprice")
var_est <- VAR(na.omit(series), p = 24, type = "const")

restMat <- matrix(rep(NA, 9), ncol = 3)
restMat[1, c(2, 3)] <- 0
restMat[2, 3] <- 0
restMat
x1 <- id.ngml(var_est, restriction_matrix = restMat)
summary(x1)

irf <- irf(x1, n.ahead = 24)
plot(irf)

cores <- parallel::detectCores() - 1
set.seed(231)
x1.boot <- wild.boot(x1, design = "fixed", distr = "rademacher", nboot = 1000, n.ahead = 24, nc = cores)  #(~10min with 1000 bootstraps)
summary(x1.boot)

cairo_ps(file = "figureA1.eps", onefile = FALSE, fallback_resolution = 600)
plot(x1.boot, lowerq = 0.025, upperq = 0.975)
dev.off() 


# Retrieving structural shocks ------

# Compute reduced form residuals
resid1 <- x1$VAR$varresult$dlprod$residuals 
resid2 <- x1$VAR$varresult$rea$residuals 
resid3 <- x1$VAR$varresult$dlprice$residuals 

residuals <- cbind(resid1,resid2,resid3)

#compute e_t matrix of structural shocks */
e <- solve(x1$B) %*% t(residuals)
e <- t(e)

month <- seq(as.Date("2005/1/1"), as.Date("2021/12/1"), "month")
oil_shocks <- as.data.frame(cbind(month,e))
oil_shocks$month <- as.Date(oil_shocks$month)
colnames(oil_shocks) <- c("month","oilsupply_shock","oildemand_shock","oilprice_shock")
write.csv(oil_shocks, file = "oil_shocks.csv", row.names = F)


# NATURAL GAS

series <- cbind(natural_gas$dlprod,natural_gas$rea,natural_gas$dlprice)
colnames(series) <- c("dlprod","rea","dlprice")
var_est <- VAR(na.omit(series), p = 24, type = "const")

restMat <- matrix(rep(NA, 9), ncol = 3)
restMat[1, c(2, 3)] <- 0
restMat[2, 3] <- 0
restMat
x1 <- id.ngml(var_est, restriction_matrix = restMat)
summary(x1)

irf <- irf(x1, n.ahead = 24)
plot(irf)

cores <- parallel::detectCores() - 1
set.seed(231)
x1.boot <- wild.boot(x1, design = "fixed", distr = "rademacher", nboot = 1000, n.ahead = 24, nc = cores)  #(~10min with 1000 bootstraps)
summary(x1.boot)

cairo_ps(file = "figureA2.eps", onefile = FALSE, fallback_resolution = 600)
plot(x1.boot, lowerq = 0.025, upperq = 0.975)
dev.off() 


# Retrieving structural shocks ------

# Compute reduced form residuals
resid1 <- x1$VAR$varresult$dlprod$residuals 
resid2 <- x1$VAR$varresult$rea$residuals 
resid3 <- x1$VAR$varresult$dlprice$residuals 

residuals <- cbind(resid1,resid2,resid3)

#compute e_t matrix of structural shocks */
e <- solve(x1$B) %*% t(residuals)
e <- t(e)

month <- seq(as.Date("2005/1/1"), as.Date("2021/12/1"), "month")
gas_shocks <- as.data.frame(cbind(month,e))
gas_shocks$month <- as.Date(gas_shocks$month)
colnames(gas_shocks) <- c("month","gassupply_shock","gasdemand_shock","gasprice_shock")
write.csv(gas_shocks, file = "gas_shocks.csv", row.names = F)

# Figure 1

p1 <- ggplot(oil_shocks, aes(x=month, y=oilsupply_shock)) +
  geom_line() +
  labs(title="Supply shock",x="month", y = "crude oil") +
  theme_classic()
p2 <- ggplot(oil_shocks, aes(x=month, y=oildemand_shock)) +
  geom_line() +
  labs(title="Demand shock",x="month", y = "crude oil") +
  theme_classic()
p3 <- ggplot(oil_shocks, aes(x=month, y=oilprice_shock)) +
  geom_line() +
  labs(title="Price shock",x="month", y = "crude oil") +
  theme_classic()
p4 <- ggplot(gas_shocks, aes(x=month, y=gassupply_shock)) +
  geom_line() +
  labs(title="Supply shock",x="month", y = "natural gas") +
  theme_classic()
p5 <- ggplot(gas_shocks, aes(x=month, y=gasdemand_shock)) +
  geom_line() +
  labs(title="Demand shock",x="month", y = "natural gas") +
  theme_classic()
p6 <- ggplot(gas_shocks, aes(x=month, y=gasprice_shock)) +
  geom_line() +
  labs(title="Price shock",x="month", y = "natural gas") +
  theme_classic()


cairo_ps(file = "figure1.eps", onefile = FALSE, fallback_resolution = 600)
grid.arrange(p1,p2,p3,p4,p5,p6, ncol = 3)
dev.off() 