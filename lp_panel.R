#===============================================================================
# Description: Calculates and plots the local projections 
#===============================================================================

#===============================================================================
# 1). Preliminary ------
#===============================================================================


# Clean up workspace
rm(list=ls())

# Load packages
wants <- c("panelvar","dplyr","plm","lpirfs","openxlsx","readxl","ggplot2","gridExtra","grid")
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

# Import data
paneldata <- read_xlsx(paste0(dir$data1,"/panel_data.xlsx"))
oil_shocks <- read.csv(paste0(dir$data2,"/oil_shocks.csv"))
gas_shocks <- read.csv(paste0(dir$data2,"/gas_shocks.csv"))
exp <- read_xlsx(paste0(dir$data1,"/expenditure.xlsx"))

# Data transformation
country_mapping <- c(
  "Argentina" = 1,
  "Barbados" = 2,
  "Bolivia" = 3,
  "Brazil" = 4,
  "Chile" = 5,
  "Colombia" = 6,
  "Costa Rica" = 7,
  "Ecuador" = 8,
  "El Salvador" = 9,
  "Guatemala" = 10,
  "Honduras" = 11,
  "Jamaica" = 12,
  "Mexico" = 13,
  "Nicaragua" = 14,
  "Panama" = 15,
  "Paraguay" = 16,
  "Peru" = 17,
  "Trinidad and Tobago" = 18,
  "Uruguay" = 19
)
paneldata$id <- country_mapping[paneldata$country]
paneldata$id <- as.numeric(paneldata$id)

paneldata <- paneldata %>% filter(id!=1) # Remove Argentina

# generate z variable (standardized renewable share)
paneldata <- paneldata %>% group_by(id)  %>% mutate(infla_energy=log(cpi_energy/dplyr::lag(cpi_energy,1))*100)
paneldata <- paneldata %>% group_by(id)  %>% mutate(infla_overall=log(cpi_overall/dplyr::lag(cpi_overall,1))*100)
paneldata <- paneldata %>% mutate(z=(renew_lastyear-mean(renew_lastyear, na.rm=T))/sd(renew_lastyear, na.rm = T))

# average share oil import (delta average)
meanshare <- mean(paneldata$fuel_imports, na.rm=T)/100 

paneldata$month <- rep(seq(as.Date("2005/1/1"), as.Date("2021/12/1"), "month"),18)

# Merge shocks data
paneldata <- merge(paneldata, oil_shocks, by = "month", all.x = TRUE)
paneldata <- merge(paneldata, gas_shocks, by = "month", all.x = TRUE)

# Merge government expenditure data
paneldata$year <- substring(paneldata$month,1,4)
paneldata$year <- as.numeric(paneldata$year)
exp$year <- exp$año
exp$country <- exp$pais
paneldata <- left_join(paneldata,exp, by = c("country","year"))


data <- paneldata %>% dplyr::select(id,month,infla_energy,infla_overall,z,it,diff_fx,oilprice_shock,gasprice_shock,exp_to_gdp)
data <- pdata.frame(data, index = c("id", "month"))


# Plotting renewable electricity generation

annual <- paneldata %>% group_by(year,country) %>% summarise(mean_renew=mean(renewables, na.rm = T))


cairo_ps(file = "renewables.eps", onefile = FALSE, fallback_resolution = 600, width = 12, height = 6)
ggplot(data = annual, aes(x = year, y = mean_renew, group = country)) +
  geom_line() +
  scale_x_discrete(breaks = seq(2005, 2021, by = 5)) +
  facet_wrap(~ country, ncol = 6) +
  theme(panel.spacing.x = unit(10, "mm")) +
  theme_minimal() +  
  theme(legend.position = "none", axis.title.x = element_blank(),
        axis.title.y = element_blank(), strip.text = element_text(size = 11))
dev.off() 

data <- data %>% rename(cross_id = id)
data <- data %>% rename(date_id = month)
data <- pdata.frame(data, index = c("cross_id", "date_id"))
       
#===============================================================================
# 2). Local projections (with gamma=5) ------
#===============================================================================

#WTI 
shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare


shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "low", time = seq(1,18))
)


plot1 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.3,0.3) +
  geom_hline(yintercept=0)

plot2 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.3,0.3) +
  geom_hline(yintercept=0)


#GAS 
shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare

shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "low", time = seq(1,18))
)
     

plot3 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.3,0.3) +
  geom_hline(yintercept=0)

plot4 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.3,0.3) +
  geom_hline(yintercept=0)
                      



cairo_ps(file = "CIRF.eps", onefile = FALSE, fallback_resolution = 600, width = 8, height = 7)

g <- ggplotGrob(plot1)
legend <- g$grobs[[which(g$layout$name == "guide-box-bottom")]]

grid.arrange(plot1+theme(legend.position='hidden'), plot2+theme(legend.position='hidden'),
             plot3+theme(legend.position='hidden'), plot4+theme(legend.position='hidden'), bottom=legend$grobs[[1]],
             ncol=2)
dev.off()




#

#===============================================================================
# 3). Asymmetric local projections (with gamma=5) ------
#===============================================================================

#WTI 
data$oilshock_pos <- ifelse(data$oilprice_shock>0,data$oilprice_shock,0)
data$oilshock_neg <- ifelse(data$oilprice_shock<0,abs(data$oilprice_shock),0)

shock_energy_pos <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "oilshock_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_energy_pos$irf_s1_mean <- shock_energy_pos$irf_s1_mean*meanshare
shock_energy_pos$irf_s1_low <- shock_energy_pos$irf_s1_low*meanshare
shock_energy_pos$irf_s1_up <- shock_energy_pos$irf_s1_up*meanshare
shock_energy_pos$irf_s2_mean <- shock_energy_pos$irf_s2_mean*meanshare
shock_energy_pos$irf_s2_low <- shock_energy_pos$irf_s2_low*meanshare
shock_energy_pos$irf_s2_up <- shock_energy_pos$irf_s2_up*meanshare


shock_energy_neg <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "oilshock_neg",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_energy_neg$irf_s1_mean <- shock_energy_neg$irf_s1_mean*meanshare
shock_energy_neg$irf_s1_low <- shock_energy_neg$irf_s1_low*meanshare
shock_energy_neg$irf_s1_up <- shock_energy_neg$irf_s1_up*meanshare
shock_energy_neg$irf_s2_mean <- shock_energy_neg$irf_s2_mean*meanshare
shock_energy_neg$irf_s2_low <- shock_energy_neg$irf_s2_low*meanshare
shock_energy_neg$irf_s2_up <- shock_energy_neg$irf_s2_up*meanshare



shock_overall_pos <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "oilshock_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_overall_pos$irf_s1_mean <- shock_overall_pos$irf_s1_mean*meanshare
shock_overall_pos$irf_s1_low <- shock_overall_pos$irf_s1_low*meanshare
shock_overall_pos$irf_s1_up <- shock_overall_pos$irf_s1_up*meanshare
shock_overall_pos$irf_s2_mean <- shock_overall_pos$irf_s2_mean*meanshare
shock_overall_pos$irf_s2_low <- shock_overall_pos$irf_s2_low*meanshare
shock_overall_pos$irf_s2_up <- shock_overall_pos$irf_s2_up*meanshare


shock_overall_neg <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "oilshock_neg",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_overall_neg$irf_s1_mean <- shock_overall_neg$irf_s1_mean*meanshare
shock_overall_neg$irf_s1_low <- shock_overall_neg$irf_s1_low*meanshare
shock_overall_neg$irf_s1_up <- shock_overall_neg$irf_s1_up*meanshare
shock_overall_neg$irf_s2_mean <- shock_overall_neg$irf_s2_mean*meanshare
shock_overall_neg$irf_s2_low <- shock_overall_neg$irf_s2_low*meanshare
shock_overall_neg$irf_s2_up <- shock_overall_neg$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy_pos <- rbind(cumsum(shock_energy_pos$irf_s1_mean),cumsum(shock_energy_pos$irf_s1_low),cumsum(shock_energy_pos$irf_s1_up),cumsum(shock_energy_pos$irf_s2_mean),cumsum(shock_energy_pos$irf_s2_low),cumsum(shock_energy_pos$irf_s2_up))
cshock_energy_pos <- t(cshock_energy_pos)
cshock_energy_pos <- as.data.frame(cshock_energy_pos)
cshock_energy_pos$Time <- seq(1,18)
colnames(cshock_energy_pos) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

cshock_energy_neg <- rbind(cumsum(shock_energy_neg$irf_s1_mean),cumsum(shock_energy_neg$irf_s1_low),cumsum(shock_energy_neg$irf_s1_up),cumsum(shock_energy_neg$irf_s2_mean),cumsum(shock_energy_neg$irf_s2_low),cumsum(shock_energy_neg$irf_s2_up))
cshock_energy_neg <- t(cshock_energy_neg)
cshock_energy_neg <- as.data.frame(cshock_energy_neg)
cshock_energy_neg$Time <- seq(1,18)
colnames(cshock_energy_neg) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

cshock_overall_pos <- rbind(cumsum(shock_overall_pos$irf_s1_mean),cumsum(shock_overall_pos$irf_s1_low),cumsum(shock_overall_pos$irf_s1_up),cumsum(shock_overall_pos$irf_s2_mean),cumsum(shock_overall_pos$irf_s2_low),cumsum(shock_overall_pos$irf_s2_up))
cshock_overall_pos <- t(cshock_overall_pos)
cshock_overall_pos <- as.data.frame(cshock_overall_pos)
cshock_overall_pos$Time <- seq(1,18)
colnames(cshock_overall_pos) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

cshock_overall_neg <- rbind(cumsum(shock_overall_neg$irf_s1_mean),cumsum(shock_overall_neg$irf_s1_low),cumsum(shock_overall_neg$irf_s1_up),cumsum(shock_overall_neg$irf_s2_mean),cumsum(shock_overall_neg$irf_s2_low),cumsum(shock_overall_neg$irf_s2_up))
cshock_overall_neg <- t(cshock_overall_neg)
cshock_overall_neg <- as.data.frame(cshock_overall_neg)
cshock_overall_neg$Time <- seq(1,18)
colnames(cshock_overall_neg) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


colnames(cshock_energy_pos) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_pos_long <- rbind(
  cbind(cshock_energy_pos[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy_pos[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_energy_neg) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_neg_long <- rbind(
  cbind(cshock_energy_neg[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy_neg[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall_pos) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_pos_long <- rbind(
  cbind(cshock_overall_pos[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall_pos[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall_neg) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_neg_long <- rbind(
  cbind(cshock_overall_neg[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall_neg[,4:6], regime = "low", time = seq(1,18))
)

plot1 <- ggplot(cshock_energy_pos_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Positive crude oil shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center", panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)


plot2 <- ggplot(cshock_energy_neg_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Negative crude oil shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)


plot3 <- ggplot(cshock_overall_pos_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Positive crude oil shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)


plot4 <- ggplot(cshock_overall_neg_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Negative crude oil shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)


cairo_ps(file = "CIRF_oil_asym.eps", onefile = FALSE, fallback_resolution = 600)
g <- ggplotGrob(plot1)
legend <- g$grobs[[which(g$layout$name == "guide-box-bottom")]]

grid.arrange(plot1+theme(legend.position='hidden'), plot2+theme(legend.position='hidden'),
             plot3+theme(legend.position='hidden'), plot4+theme(legend.position='hidden'), bottom=legend$grobs[[1]],
             ncol=2)
dev.off()


# GAS 
data$gasshock_pos <- ifelse(data$gasprice_shock>0,data$gasprice_shock,0)
data$gasshock_neg <- ifelse(data$gasprice_shock<0,abs(data$gasprice_shock),0)

shock_energy_pos <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "gasshock_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_energy_pos$irf_s1_mean <- shock_energy_pos$irf_s1_mean*meanshare
shock_energy_pos$irf_s1_low <- shock_energy_pos$irf_s1_low*meanshare
shock_energy_pos$irf_s1_up <- shock_energy_pos$irf_s1_up*meanshare
shock_energy_pos$irf_s2_mean <- shock_energy_pos$irf_s2_mean*meanshare
shock_energy_pos$irf_s2_low <- shock_energy_pos$irf_s2_low*meanshare
shock_energy_pos$irf_s2_up <- shock_energy_pos$irf_s2_up*meanshare


shock_energy_neg <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "gasshock_neg",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_energy_neg$irf_s1_mean <- shock_energy_neg$irf_s1_mean*meanshare
shock_energy_neg$irf_s1_low <- shock_energy_neg$irf_s1_low*meanshare
shock_energy_neg$irf_s1_up <- shock_energy_neg$irf_s1_up*meanshare
shock_energy_neg$irf_s2_mean <- shock_energy_neg$irf_s2_mean*meanshare
shock_energy_neg$irf_s2_low <- shock_energy_neg$irf_s2_low*meanshare
shock_energy_neg$irf_s2_up <- shock_energy_neg$irf_s2_up*meanshare



shock_overall_pos <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "gasshock_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_overall_pos$irf_s1_mean <- shock_overall_pos$irf_s1_mean*meanshare
shock_overall_pos$irf_s1_low <- shock_overall_pos$irf_s1_low*meanshare
shock_overall_pos$irf_s1_up <- shock_overall_pos$irf_s1_up*meanshare
shock_overall_pos$irf_s2_mean <- shock_overall_pos$irf_s2_mean*meanshare
shock_overall_pos$irf_s2_low <- shock_overall_pos$irf_s2_low*meanshare
shock_overall_pos$irf_s2_up <- shock_overall_pos$irf_s2_up*meanshare


shock_overall_neg <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "gasshock_neg",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  lag_switching = TRUE,
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_overall_neg$irf_s1_mean <- shock_overall_neg$irf_s1_mean*meanshare
shock_overall_neg$irf_s1_low <- shock_overall_neg$irf_s1_low*meanshare
shock_overall_neg$irf_s1_up <- shock_overall_neg$irf_s1_up*meanshare
shock_overall_neg$irf_s2_mean <- shock_overall_neg$irf_s2_mean*meanshare
shock_overall_neg$irf_s2_low <- shock_overall_neg$irf_s2_low*meanshare
shock_overall_neg$irf_s2_up <- shock_overall_neg$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy_pos <- rbind(cumsum(shock_energy_pos$irf_s1_mean),cumsum(shock_energy_pos$irf_s1_low),cumsum(shock_energy_pos$irf_s1_up),cumsum(shock_energy_pos$irf_s2_mean),cumsum(shock_energy_pos$irf_s2_low),cumsum(shock_energy_pos$irf_s2_up))
cshock_energy_pos <- t(cshock_energy_pos)
cshock_energy_pos <- as.data.frame(cshock_energy_pos)
cshock_energy_pos$Time <- seq(1,18)
colnames(cshock_energy_pos) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

cshock_energy_neg <- rbind(cumsum(shock_energy_neg$irf_s1_mean),cumsum(shock_energy_neg$irf_s1_low),cumsum(shock_energy_neg$irf_s1_up),cumsum(shock_energy_neg$irf_s2_mean),cumsum(shock_energy_neg$irf_s2_low),cumsum(shock_energy_neg$irf_s2_up))
cshock_energy_neg <- t(cshock_energy_neg)
cshock_energy_neg <- as.data.frame(cshock_energy_neg)
cshock_energy_neg$Time <- seq(1,18)
colnames(cshock_energy_neg) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

cshock_overall_pos <- rbind(cumsum(shock_overall_pos$irf_s1_mean),cumsum(shock_overall_pos$irf_s1_low),cumsum(shock_overall_pos$irf_s1_up),cumsum(shock_overall_pos$irf_s2_mean),cumsum(shock_overall_pos$irf_s2_low),cumsum(shock_overall_pos$irf_s2_up))
cshock_overall_pos <- t(cshock_overall_pos)
cshock_overall_pos <- as.data.frame(cshock_overall_pos)
cshock_overall_pos$Time <- seq(1,18)
colnames(cshock_overall_pos) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

cshock_overall_neg <- rbind(cumsum(shock_overall_neg$irf_s1_mean),cumsum(shock_overall_neg$irf_s1_low),cumsum(shock_overall_neg$irf_s1_up),cumsum(shock_overall_neg$irf_s2_mean),cumsum(shock_overall_neg$irf_s2_low),cumsum(shock_overall_neg$irf_s2_up))
cshock_overall_neg <- t(cshock_overall_neg)
cshock_overall_neg <- as.data.frame(cshock_overall_neg)
cshock_overall_neg$Time <- seq(1,18)
colnames(cshock_overall_neg) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


colnames(cshock_energy_pos) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_pos_long <- rbind(
  cbind(cshock_energy_pos[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy_pos[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_energy_neg) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_neg_long <- rbind(
  cbind(cshock_energy_neg[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy_neg[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall_pos) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_pos_long <- rbind(
  cbind(cshock_overall_pos[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall_pos[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall_neg) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_neg_long <- rbind(
  cbind(cshock_overall_neg[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall_neg[,4:6], regime = "low", time = seq(1,18))
)

plot1 <- ggplot(cshock_energy_pos_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Positive natural gas shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)


plot2 <- ggplot(cshock_energy_neg_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Negative natural gas shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)


plot3 <- ggplot(cshock_overall_pos_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Positive natural gas shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)


plot4 <- ggplot(cshock_overall_neg_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Negative natural gas shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  ylim(-0.5,0.6) +
  geom_hline(yintercept=0)

cairo_ps(file = "CIRF_gas_asym.eps", onefile = FALSE, fallback_resolution = 600)
g <- ggplotGrob(plot1)
legend <- g$grobs[[which(g$layout$name == "guide-box-bottom")]]

grid.arrange(plot1+theme(legend.position='hidden'), plot2+theme(legend.position='hidden'),
             plot3+theme(legend.position='hidden'), plot4+theme(legend.position='hidden'), bottom=legend$grobs[[1]],
             ncol=2)
dev.off()



#===============================================================================
# 4). Robustness ------
#===============================================================================


# a) Sensitivity to gamma

#gamma=1

#WTI 
shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 1,
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare


shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 1,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "low", time = seq(1,18))
)


plot1 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)

plot2 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)


#GAS 
shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 1,
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare

shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 1,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "low", time = seq(1,18))
)


plot3 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)

plot4 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)



cairo_ps(file = "CIRF_gamma1.eps", onefile = FALSE, fallback_resolution = 600, width = 8, height = 7)
g <- ggplotGrob(plot1)
legend <- g$grobs[[which(g$layout$name == "guide-box-bottom")]]

grid.arrange(plot1+theme(legend.position='hidden'), plot2+theme(legend.position='hidden'),
             plot3+theme(legend.position='hidden'), plot4+theme(legend.position='hidden'), bottom=legend$grobs[[1]],
             ncol=2)
dev.off()

# gamma=10

#WTI 
shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 10,
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare


shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 10,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "low", time = seq(1,18))
)


plot1 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)

plot2 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)


#GAS 
shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 10,
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare

shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "z",
  use_logistic = T,
  lag_switching = F, #it's already lagged
  gamma = 10,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "low", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "high", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "low", time = seq(1,18))
)


plot3 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)

plot4 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)



cairo_ps(file = "CIRF_gamma10.eps", onefile = FALSE, fallback_resolution = 600, width = 8, height = 7)
g <- ggplotGrob(plot1)
legend <- g$grobs[[which(g$layout$name == "guide-box-bottom")]]

grid.arrange(plot1+theme(legend.position='hidden'), plot2+theme(legend.position='hidden'),
             plot3+theme(legend.position='hidden'), plot4+theme(legend.position='hidden'), bottom=legend$grobs[[1]],
             ncol=2)
dev.off()



# b) Defining a dummy variable to determine each country's current renewable environment
#d if country mean >50
#d1 if i,t >50
#d2 if i,t >55 (median)

data <- paneldata %>% dplyr::select(id,month,infla_energy,infla_overall,d,d1,d2,it,diff_fx,oilprice_shock,gasprice_shock,exp_to_gdp)
data <- pdata.frame(data, index = c("id", "month"))
data <- data %>% rename(cross_id = id)
data <- data %>% rename(date_id = month)
data <- pdata.frame(data, index = c("cross_id", "date_id"))
       
#WTI 

shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "d",
  use_logistic = F,
  lag_switching = F, #it's already lagged
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare


shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "oilprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "d",
  lag_switching = FALSE,
  use_logistic = FALSE,
  use_hp = FALSE,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "low", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "high", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "low", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "high", time = seq(1,18))
)


plot1 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)

plot2 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Crude oil shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)


#GAS 
shock_energy <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_energy",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "d",
  use_logistic = F,
  lag_switching = F, #it's already lagged
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_energy$irf_s1_mean <- shock_energy$irf_s1_mean*meanshare
shock_energy$irf_s1_low <- shock_energy$irf_s1_low*meanshare
shock_energy$irf_s1_up <- shock_energy$irf_s1_up*meanshare
shock_energy$irf_s2_mean <- shock_energy$irf_s2_mean*meanshare
shock_energy$irf_s2_low <- shock_energy$irf_s2_low*meanshare
shock_energy$irf_s2_up <- shock_energy$irf_s2_up*meanshare

shock_overall <- lp_nl_panel(
  data_set = data,
  data_sample = "Full",
  endog_data = "infla_overall",
  cumul_mult = FALSE,
  shock = "gasprice_shock",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovNW",
  c_exog_data = c("it","diff_fx","exp_to_gdp"),
  l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
  lags_exog_data = 12,
  switching = "d",
  use_logistic = F,
  lag_switching = F, #it's already lagged
  gamma = 5,
  confint = 1.64,
  hor = 18
)

shock_overall$irf_s1_mean <- shock_overall$irf_s1_mean*meanshare
shock_overall$irf_s1_low <- shock_overall$irf_s1_low*meanshare
shock_overall$irf_s1_up <- shock_overall$irf_s1_up*meanshare
shock_overall$irf_s2_mean <- shock_overall$irf_s2_mean*meanshare
shock_overall$irf_s2_low <- shock_overall$irf_s2_low*meanshare
shock_overall$irf_s2_up <- shock_overall$irf_s2_up*meanshare


# Plot cumulative IRF
cshock_energy <- rbind(cumsum(shock_energy$irf_s1_mean),cumsum(shock_energy$irf_s1_low),cumsum(shock_energy$irf_s1_up),cumsum(shock_energy$irf_s2_mean),cumsum(shock_energy$irf_s2_low),cumsum(shock_energy$irf_s2_up))
cshock_energy <- t(cshock_energy)
cshock_energy <- as.data.frame(cshock_energy)
cshock_energy$Time <- seq(1,18)
colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")


cshock_overall <- rbind(cumsum(shock_overall$irf_s1_mean),cumsum(shock_overall$irf_s1_low),cumsum(shock_overall$irf_s1_up),cumsum(shock_overall$irf_s2_mean),cumsum(shock_overall$irf_s2_low),cumsum(shock_overall$irf_s2_up))
cshock_overall <- t(cshock_overall)
cshock_overall <- as.data.frame(cshock_overall)
cshock_overall$Time <- seq(1,18)
colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")

colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_energy_long <- rbind(
  cbind(cshock_energy[,1:3], regime = "low", time = seq(1,18)),
  cbind(cshock_energy[,4:6], regime = "high", time = seq(1,18))
)

colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
cshock_overall_long <- rbind(
  cbind(cshock_overall[,1:3], regime = "low", time = seq(1,18)),
  cbind(cshock_overall[,4:6], regime = "high", time = seq(1,18))
)


plot3 <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Energy inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)

plot4 <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
  geom_line(aes(y = mean, color = regime), size = 1) +
  labs(
    title = "Natural gas shock",
    x = "Horizon",
    y = "Headline inflation (%)"
  ) +
  scale_fill_manual(values = c("darkgreen", "red")) +
  scale_color_manual(values = c("darkgreen", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom",  legend.justification = "center",panel.background = element_blank(),  # Remove background
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  xlim(1,18) +
  geom_hline(yintercept=0)



cairo_ps(file = "CIRF_dummy.eps", onefile = FALSE, fallback_resolution = 600, width = 8, height = 7)
g <- ggplotGrob(plot1)
legend <- g$grobs[[which(g$layout$name == "guide-box-bottom")]] 

grid.arrange(plot1+theme(legend.position='hidden'), plot2+theme(legend.position='hidden'),
             plot3+theme(legend.position='hidden'), plot4+theme(legend.position='hidden'), bottom=legend$grobs[[1]],
             ncol=2)
dev.off()

