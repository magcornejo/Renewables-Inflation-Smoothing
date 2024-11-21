#===============================================================================
# Description: Panel unit root tests and summary statistics
#===============================================================================


rm(list = ls())


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
dir$data1 <- paste0(dir$root, "/data1")
dir$data2 <- paste0(dir$root, "/data2")
dir$figures <- paste0(dir$root,"/figures")


# Import data
paneldata <- read_xlsx(paste0(dir$data1, "/panel_data.xlsx"))
oil_shocks <- read.csv(paste0(dir$data2, "/oil_shocks.csv"))
gas_shocks <- read.csv(paste0(dir$data2, "/gas_shocks.csv"))
exp <- read_xlsx(paste0(dir$data1, "/expenditure.xlsx"))

# Data transformation
country_mapping <- c(
  "Argentina" = 1, "Barbados" = 2, "Bolivia" = 3, "Brazil" = 4, "Chile" = 5,
  "Colombia" = 6, "Costa Rica" = 7, "Ecuador" = 8, "El Salvador" = 9,
  "Guatemala" = 10, "Honduras" = 11, "Jamaica" = 12, "Mexico" = 13,
  "Nicaragua" = 14, "Panama" = 15, "Paraguay" = 16, "Peru" = 17,
  "Trinidad and Tobago" = 18, "Uruguay" = 19
)
paneldata$id <- country_mapping[paneldata$country]
paneldata$id <- as.numeric(paneldata$id)
paneldata <- paneldata %>% filter(id != 1)

paneldata <- paneldata %>% group_by(id)  %>% mutate(infla_energy = log(cpi_energy/dplyr::lag(cpi_energy, 1)) * 100)
paneldata <- paneldata %>% group_by(id)  %>% mutate(infla_overall = log(cpi_overall/dplyr::lag(cpi_overall, 1)) * 100)
paneldata <- paneldata %>% mutate(z = (renew_lastyear - mean(renew_lastyear, na.rm = TRUE)) / sd(renew_lastyear, na.rm = TRUE))

paneldata$month <- rep(seq(as.Date("2005/1/1"), as.Date("2021/12/1"), "month"), 18)

# Merge shocks data
oil_shocks$month <- as.Date(oil_shocks$month)
gas_shocks$month <- as.Date(gas_shocks$month)
paneldata <- left_join(paneldata, oil_shocks, by = "month", relationship = "many-to-one")
paneldata <- left_join(paneldata, gas_shocks, by = "month", relationship = "many-to-one")

# Merge government expenditure data
paneldata$year <- as.numeric(substring(paneldata$month, 1, 4))
exp$year <- exp$año
exp$country <- exp$pais
paneldata <- left_join(paneldata, exp, by = c("country", "year"))

data <- paneldata %>% dplyr::select(id, month, infla_energy, infla_overall, z, it, diff_fx, oilsupply_shock, gassupply_shock, exp_to_gdp, oildemand_shock, gasdemand_shock, oilprice_shock, gasprice_shock, fuel_imports)
data <- pdata.frame(data, index = c("id", "month"))

# Panel unit root tests

purtest(data$infla_energy, test = "ips", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(data$infla_energy, test = "madwu", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(data$infla_overall, test = "ips", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(data$infla_overall, test = "madwu", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(data$exp_to_gdp, test = "ips", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(data$exp_to_gdp, test = "madwu", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(data$z, test = "ips", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(data$z, test = "madwu", lags = "SIC", exo = "intercept", index = c("id", "month"))

data <- data %>% group_by(id) %>% mutate(constant_FX = ifelse(all(diff_fx == diff_fx[1]), 1, 0)) %>%
  ungroup()

newdata <- data %>% filter(constant_FX==0)
newdata <- pdata.frame(newdata, index = c("id", "month"))
purtest(newdata$diff_fx, test = "ips", lags = "SIC", exo = "intercept", index = c("id", "month"))
purtest(newdata$diff_fx, test = "madwu", lags = "SIC", exo = "intercept", index = c("id", "month"))


# Summary statistics

summary_stats <- paneldata %>%
  group_by(country) %>%
  summarise(
    mean_infla_energy = mean(infla_energy, na.rm = TRUE),
    sd_infla_energy = sd(infla_energy, na.rm = TRUE),
    n_infla_energy = sum(!is.na(infla_energy)),
    mean_infla_overall = mean(infla_overall, na.rm = TRUE),
    sd_infla_overall = sd(infla_overall, na.rm = TRUE),
    n_infla_overall = sum(!is.na(infla_overall)),
    mean_renew_lastyear = mean(renew_lastyear, na.rm = TRUE),
    sd_renew_lastyear = sd(renew_lastyear, na.rm = TRUE),
    n_renew_lastyear = sum(!is.na(renew_lastyear))
  )

summary_table <- summary_stats %>%
  pivot_longer(
    cols = -country,
    names_to = c("variable", ".value"),
    names_sep = "_"
  )

# Table A3
xtable(summary_stats, caption = "Summary statistics on selected variables by country, 2005:1-2021:12", digits = 2)

