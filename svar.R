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
dir$data1 <- paste0(dir$root,"/data1")
dir$data2 <- paste0(dir$root,"/data2")
dir$figures <- paste0(dir$root,"/figures")

# Import time series datasets
crude_oil <- read_xlsx(paste0(dir$data1,"/crude_oil.xlsx"), sheet = "monthly")
crude_oil <- read.zoo(crude_oil, fomat = "%Y-%m-%d")
natural_gas <- read_xlsx(paste0(dir$data1,"/natural_gas.xlsx"), sheet = "monthly")
natural_gas <- read.zoo(natural_gas, fomat = "%Y-%m-%d")

# Plots since 2005
crude_oil05 <- crude_oil[index(crude_oil)>"2004-12-01"]
natural_gas05 <- natural_gas[index(natural_gas)>"2004-12-01"]

crude_oil05$price <- crude_oil05$wti/crude_oil05$uscpi*100
natural_gas05$price <- natural_gas05$henryhub/natural_gas05$uscpi*100

# Figure 2
setwd(dir$figures)

cairo_ps(file = "figure2.eps", onefile = FALSE, fallback_resolution = 600, width = 8, height = 4)
par(mar = c(4, 4, 2, 5) )
plot.zoo(crude_oil05$price, xlab = "", ylab = "Crude oil price (USD/bbl)", col = "blue", main = "", lty = 1)
par(new = TRUE)
plot.zoo(natural_gas05$price, xlab = "", ylab = "", col = "red", axes = FALSE, lty = 2) # suppress axes and labels
axis(4)
mtext("Natural gas price (USD/million btu)", side = 4, line = 3)
legend("topright", legend = c("Crude Oil (left axis)", "Natural Gas (right axis)"), col = c("blue", "red"), lty = c(1,2), bty = "n")
dev.off()


# Data transformation
crude_oil$lprod <- log(crude_oil$production)
crude_oil$dlprod <- diff(crude_oil$lprod)*100
crude_oil$lprice <- log(crude_oil$wti/crude_oil$uscpi)
crude_oil$dlprice <- diff(crude_oil$lprice)*100
crude_oil$drea <- diff(crude_oil$igrea)

natural_gas$lprod <- log(natural_gas$usprod)
natural_gas$dlprod <- diff(natural_gas$lprod)*100
natural_gas$lprice <- log(natural_gas$henryhub/natural_gas$uscpi)
natural_gas$dlprice <- diff(natural_gas$lprice)*100
natural_gas$drea <- diff(natural_gas$igrea)

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
plot.zoo(crude_oil$drea, xlab = "Month", ylab = "diff(IGREA)")
plot.zoo(crude_oil$dlprice, xlab = "Month", ylab = "diff(log(price))")
plot.zoo(natural_gas$dlprod, xlab = "Month", ylab = "diff(log(production))")
plot.zoo(natural_gas$drea, xlab = "Month", ylab = "diff(IGREA)")
plot.zoo(natural_gas$dlprice, xlab = "Month", ylab = "diff(log(price))")


# Unit root tests
summary(ur.df(crude_oil$lprod, type = "trend", selectlags = "BIC")) #I(1) 
summary(ur.df(crude_oil$igrea, type = "trend", selectlags = "BIC")) #I(0) at 5%
summary(ur.df(crude_oil$lprice, type = "trend", selectlags = "BIC")) #I(1) 
summary(ur.df(natural_gas$lprod, type = "trend", selectlags = "BIC")) #I(0) 
summary(ur.df(natural_gas$igrea, type = "trend", selectlags = "BIC")) #I(0) at 5%
summary(ur.df(natural_gas$lprice, type = "trend", selectlags = "BIC")) #I(1)

summary(ur.df(na.omit(crude_oil$dlprod), type = "drift", selectlags = "BIC")) #I(0)
summary(ur.df(na.omit(crude_oil$dlprice), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(crude_oil$drea), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(natural_gas$dlprod), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(natural_gas$dlprice), type = "drift", selectlags = "BIC")) #I(0) 
summary(ur.df(na.omit(natural_gas$drea), type = "drift", selectlags = "BIC")) #I(0) 

summary(ur.pp(crude_oil$lprod, type = "Z-tau", model = "trend", lags = "short"))  #I(1) 
summary(ur.pp(crude_oil$igrea, type = "Z-tau", model = "trend", lags = "short")) #I(0) at 10%
summary(ur.pp(crude_oil$lprice, type = "Z-tau", model = "trend", lags = "short")) #I(1) 
summary(ur.pp(natural_gas$lprod, type = "Z-tau", model = "trend", lags = "short"))  #I(0) 
summary(ur.pp(natural_gas$igrea, type = "Z-tau", model = "trend", lags = "short")) #I(0) at 10%
summary(ur.pp(natural_gas$lprice, type = "Z-tau", model = "trend", lags = "short")) #I(1) at 1%

summary(ur.pp(crude_oil$dlprod, type = "Z-tau", model = "constant", lags = "short"))  #I(0) 
summary(ur.pp(crude_oil$dlprice, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(crude_oil$drea, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(natural_gas$dlprod, type = "Z-tau", model = "constant", lags = "short"))  #I(0)
summary(ur.pp(natural_gas$dlprice, type = "Z-tau", model = "constant", lags = "short")) #I(0) 
summary(ur.pp(natural_gas$drea, type = "Z-tau", model = "constant", lags = "short")) #I(0) 



#===============================================================================
# 2). SVAR estimation ------
#===============================================================================

# CRUDE OIL

series <- cbind(crude_oil$dlprod,crude_oil$drea,crude_oil$dlprice)
colnames(series) <- c("dlprod","drea","dlprice")
VARselect(na.omit(series), lag.max = 24, type = "const")
var_est <- VAR(na.omit(series), p = 2, type = "const")

restMat <- matrix(rep(NA, 9), ncol = 3)
restMat[1, c(2, 3)] <- 0
restMat[2, 3] <- 0
restMat
x1 <- id.ngml(var_est, restriction_matrix = restMat)
summary(x1)

irf <- irf(x1, n.ahead = 12)

cores <- parallel::detectCores() - 1
set.seed(231)
x1.boot <- wild.boot(x1, design = "fixed", distr = "rademacher", nboot = 1000, n.ahead = 12, nc = cores)  #(~10min with 1000 bootstraps)
summary(x1.boot)

# Figure A1
cairo_ps(file = "figureA1.eps", onefile = FALSE, fallback_resolution = 600)
plot(x1.boot, lowerq = 0.025, upperq = 0.975)
dev.off() 


# Retrieving structural shocks ------

# Compute reduced form residuals
resid1 <- x1$VAR$varresult$dlprod$residuals 
resid2 <- x1$VAR$varresult$drea$residuals 
resid3 <- x1$VAR$varresult$dlprice$residuals 

residuals <- cbind(resid1,resid2,resid3)

#compute e_t matrix of structural shocks */
e <- solve(x1$B) %*% t(residuals)
e <- t(e)

month <- seq(as.Date("2003/3/1"), as.Date("2021/12/1"), "month")
oil_shocks <- as.data.frame(cbind(month,e))
oil_shocks$month <- as.Date(oil_shocks$month)
colnames(oil_shocks) <- c("month","oilsupply_shock","oildemand_shock","oilprice_shock")
oil_shocks <- oil_shocks %>% filter(month>="2005-01-01")
setwd(dir$data2)
write.csv(oil_shocks, file = "oil_shocks.csv", row.names = F)

# NATURAL GAS

series <- cbind(natural_gas$dlprod,natural_gas$drea,natural_gas$dlprice)
colnames(series) <- c("dlprod","drea","dlprice")
VARselect(na.omit(series), lag.max = 24, type = "const")
var_est <- VAR(na.omit(series), p = 12, type = "const")

restMat <- matrix(rep(NA, 9), ncol = 3)
restMat[1, c(2, 3)] <- 0
restMat[2, 3] <- 0
restMat
x1 <- id.ngml(var_est, restriction_matrix = restMat)
summary(x1)

irf <- irf(x1, n.ahead = 12)

cores <- parallel::detectCores() - 1
set.seed(231)
x1.boot <- wild.boot(x1, design = "fixed", distr = "rademacher", nboot = 1000, n.ahead = 12, nc = cores)  #(~10min with 1000 bootstraps)
summary(x1.boot)

# Figure A2
setwd(dir$figures)
cairo_ps(file = "figureA2.eps", onefile = FALSE, fallback_resolution = 600)
plot(x1.boot, lowerq = 0.025, upperq = 0.975)
dev.off() 



# Retrieving structural shocks ------

# Compute reduced form residuals
resid1 <- x1$VAR$varresult$dlprod$residuals 
resid2 <- x1$VAR$varresult$drea$residuals 
resid3 <- x1$VAR$varresult$dlprice$residuals 

residuals <- cbind(resid1,resid2,resid3)

#compute e_t matrix of structural shocks */
e <- solve(x1$B) %*% t(residuals)
e <- t(e)

month <- seq(as.Date("2004/1/1"), as.Date("2021/12/1"), "month")
gas_shocks <- as.data.frame(cbind(month,e))
gas_shocks$month <- as.Date(gas_shocks$month)
colnames(gas_shocks) <- c("month","gassupply_shock","gasdemand_shock","gasprice_shock")
gas_shocks <- gas_shocks %>% filter(month>="2005-01-01")
setwd(dir$data2)
write.csv(gas_shocks, file = "gas_shocks.csv", row.names = F)


# Figure 1
setwd(dir$figures)

p1 <- ggplot(oil_shocks, aes(x=month, y=oilsupply_shock)) +
  geom_line() +
  labs(title="Supply shock",x="month", y = "crude oil") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size = 12, hjust = 0)
  )
p2 <- ggplot(oil_shocks, aes(x=month, y=oildemand_shock)) +
  geom_line() +
  labs(title="Demand shock",x="month", y = "crude oil") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size = 12, hjust = 0)
  )
p3 <- ggplot(oil_shocks, aes(x=month, y=oilprice_shock)) +
  geom_line() +
  labs(title="Price shock",x="month", y = "crude oil") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size = 12, hjust = 0)
  )
p4 <- ggplot(gas_shocks, aes(x=month, y=gassupply_shock)) +
  geom_line() +
  labs(title="Supply shock",x="month", y = "natural gas") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size = 12, hjust = 0)
  )
p5 <- ggplot(gas_shocks, aes(x=month, y=gasdemand_shock)) +
  geom_line() +
  labs(title="Demand shock",x="month", y = "natural gas") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size = 12, hjust = 0)
  )
p6 <- ggplot(gas_shocks, aes(x=month, y=gasprice_shock)) +
  geom_line() +
  labs(title="Price shock",x="month", y = "natural gas") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size = 12, hjust = 0)
  )


cairo_ps(file = "figure1.eps", onefile = FALSE, fallback_resolution = 600, width = 7, height = 4)
grid.arrange(p1,p2,p3,p4,p5,p6, ncol = 3)
dev.off() 
