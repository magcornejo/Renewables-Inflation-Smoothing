#===============================================================================
# Description: Calculates and plots the local projections 
#===============================================================================

#===============================================================================
# 0). Preliminary ------
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


#===============================================================================
# 1). Electricity generation by source in LAC ------
#===============================================================================

electricity <- read_xlsx(paste0(dir$data1, "/electricity_generation.xlsx"), sheet = "Sheet2")
electricity_bycountry <- read_xlsx(paste0(dir$data1, "/electricity_generation_by_country.xlsx"), sheet = "Hoja2")

elec_long <- t(electricity)
elec_long <- elec_long[2:23,]
data <- rbind(
  cbind(elec_long[,1], type = "Hydro", time = seq(2000,2021)),
  cbind(elec_long[,2], type = "Wind", time = seq(2000,2021)),
  cbind(elec_long[,3], type = "Solar", time = seq(2000,2021)),
  cbind(elec_long[,4], type = "Geothermal", time = seq(2000,2021)),
  cbind(elec_long[,5], type = "Renewable thermal", time = seq(2000,2021)),
  cbind(elec_long[,6], type = "Non-renewable thermal", time = seq(2000,2021)),
  cbind(elec_long[,7], type = "Nuclear", time = seq(2000,2021)),
  cbind(elec_long[,8], type = "Other", time = seq(2000,2021))
)


data <- as.data.frame(data)
data$type <- factor(data$type, levels = c("Hydro", "Wind", "Solar", "Geothermal", "Renewable thermal", "Non-renewable thermal", "Nuclear", "Other")) 
custom_colors <- c("Hydro" = "#3399FF", "Wind" = "#22BB33", "Solar" = "#FFFF00", "Geothermal" = "#996633", "Renewable thermal" = "#008830", "Non-renewable thermal" = "#999999", "Nuclear" = "#FF99CC", "Other" = "#003300" )
plot1 <- ggplot(data, aes(x=as.numeric(time), y=as.numeric(V1), fill=type)) + 
  geom_area() +
  theme_classic() +
  labs(x = "", y = "%", title = "(b) LAC (2005-2021)") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title=element_blank()) +
  scale_fill_manual(values = custom_colors)

electricity_long <- pivot_longer(electricity_bycountry, 
                                 cols = -Country, 
                                 names_to = "Source", 
                                 values_to = "Proportion")

electricity_long$Proportion <- electricity_long$Proportion * 100

electricity_long$Source <- factor(electricity_long$Source, 
                                  levels = rev(c("Hydro", "Wind", "Solar", "Geothermal", "Renewable thermal", "Non-renewable thermal", "Nuclear")))

custom_colors <- c("Hydro" = "#3399FF", "Wind" = "#22BB33", "Solar" = "#FFFF00", 
                   "Geothermal" = "#996633", "Renewable thermal" = "#008830", 
                   "Non-renewable thermal" = "#999999", "Nuclear" = "#FF99CC")

electricity_long$Country <- factor(electricity_long$Country, levels = rev(electricity_bycountry$Country))

plot2 <- ggplot(electricity_long, aes(x = Proportion, y = Country, fill = Source)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "", y = "", title = "(a) By country (2005-2021)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(labels = scales::percent) # Optional: format x-axis as percentage


setwd(dir$figures)
cairo_ps(file = "figure3.eps", onefile = FALSE, fallback_resolution = 600, width = 11, height = 4)
plot_grid(plot2,plot1, align = 'h', ncol = 2, rel_widths = c(0.4,0.6))
dev.off()

#===============================================================================
# 2). Local projections (with gamma=3) ------ a quite smooth transition
#==============================================================================

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

data <- paneldata %>% dplyr::select(id, month, infla_energy, infla_overall, z, it, diff_fx, oilsupply_shock, gassupply_shock, exp_to_gdp, oildemand_shock, gasdemand_shock, oilprice_shock, gasprice_shock, fuel_imports, d)
data <- pdata.frame(data, index = c("id", "month"))

# Define the different shocks for the three iterations

# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


####################################################################################
#################################ENERGY INFLATION ##################################
###################################################################################


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Energy Inflation)
  shock_energy <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_energy",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 3,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for energy inflation
  cshock_energy <- rbind(
    cumsum(shock_energy$irf_s1_mean),
    cumsum(shock_energy$irf_s1_low),
    cumsum(shock_energy$irf_s1_up),
    cumsum(shock_energy$irf_s2_mean),
    cumsum(shock_energy$irf_s2_low),
    cumsum(shock_energy$irf_s2_up)
  )
  cshock_energy <- t(cshock_energy)
  cshock_energy <- as.data.frame(cshock_energy)
  cshock_energy$Time <- seq(1, 12)
  
  colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_energy_long <- rbind(
    cbind(cshock_energy[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_energy[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for energy inflation without a legend
  plot_no_legend <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1.6, 1.6) +
    geom_hline(yintercept = 0)
  
  plot_list[[shock_type]] <- plot_no_legend
  
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18) 
      )
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign title
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 18),    # Larger title size
      legend.text = element_text(size = 18)    # Larger legend text size
    )
})

# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Energy Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure 4
ggsave(
  filename = "Energy_shocks.png",   # Output filename
  plot = combined_plot,             # Combined plot object
  width = 14,                       # Width in inches
  height = 12,                      # Height in inches
  dpi = 300                         # Resolution at 300 DPI for high quality
)





####################################################################################
#################################HEADLINE INFLATION ##################################
###################################################################################

# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Headline Inflation)
  shock_overall <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_overall",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_overall","infla_energy","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 3,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for headline inflation
  cshock_overall <- rbind(
    cumsum(shock_overall$irf_s1_mean),
    cumsum(shock_overall$irf_s1_low),
    cumsum(shock_overall$irf_s1_up),
    cumsum(shock_overall$irf_s2_mean),
    cumsum(shock_overall$irf_s2_low),
    cumsum(shock_overall$irf_s2_up)
  )
  cshock_overall <- t(cshock_overall)
  cshock_overall <- as.data.frame(cshock_overall)
  cshock_overall$Time <- seq(1, 12)

  colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_overall_long <- rbind(
    cbind(cshock_overall[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_overall[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for overall inflation without a legend
  plot_no_legend <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1, 1) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18) 
      )
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})

# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Headline Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure 5
ggsave(
  filename = "Headline_shocks.png",   # Output filename
  plot = combined_plot,               # Combined plot object
  width = 16,                         # Width in inches
  height = 12,                        # Height in inches
  dpi = 300                           # Resolution at 300 DPI for high quality
)



#===============================================================================
# 3). Local projections - Asymmetries
#===============================================================================

####################################################################################
#################################ENERGY INFLATION  positive shock ##################################
###################################################################################

data$price_OIL_pos <- ifelse(data$oilprice_shock>0,data$oilprice_shock,0)
data$supply_OIL_pos <- ifelse(data$oilsupply_shock>0,data$oilsupply_shock,0)
data$demand_OIL_pos <- ifelse(data$oildemand_shock>0,data$oildemand_shock,0)
data$price_GAS_pos <- ifelse(data$gasprice_shock>0,data$gasprice_shock,0)
data$supply_GAS_pos <- ifelse(data$gassupply_shock>0,data$gassupply_shock,0)
data$demand_GAS_pos <- ifelse(data$gasdemand_shock>0,data$gasdemand_shock,0)


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL_pos = list(shock = "price_OIL_pos", file_name = "CIRF_price_oil"),
  supply_OIL_pos = list(shock = "supply_OIL_pos",  file_name = "CIRF_supply_oil"),
  demand_OIL_pos = list(shock = "demand_OIL_pos", file_name = "CIRF_demand_oil"),
  price_GAS_pos = list(shock = "price_GAS_pos", file_name = "CIRF_price_gas") ,
  supply_GAS_pos = list(shock = "supply_GAS_pos", file_name = "CIRF_supply_gas"),
  demand_GAS_pos = list(shock = "demand_GAS_pos", file_name = "CIRF_demand_gas")
)




# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend

# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Energy Inflation)
  shock_energy <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_energy",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 3,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for energy inflation
  cshock_energy <- rbind(
    cumsum(shock_energy$irf_s1_mean),
    cumsum(shock_energy$irf_s1_low),
    cumsum(shock_energy$irf_s1_up),
    cumsum(shock_energy$irf_s2_mean),
    cumsum(shock_energy$irf_s2_low),
    cumsum(shock_energy$irf_s2_up)
  )
  cshock_energy <- t(cshock_energy)
  cshock_energy <- as.data.frame(cshock_energy)
  cshock_energy$Time <- seq(1, 12)
  
  colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_energy_long <- rbind(
    cbind(cshock_energy[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_energy[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for energy inflation without a legend
  plot_no_legend <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-3, 3) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), # Ajusta el tamaño del texto de la leyenda a 18
            legend.title = element_text(size = 18)
      ) +# Ajusta el tamaño del título de la leyenda a 18 (si hay título)
      scale_x_continuous(breaks = 1:12) +
      ylim(-2.5, 3) +
      geom_hline(yintercept = 0)  
  }
}



# Define improved titles for the plots
plot_titles <- c(
  supply_OIL_pos = "Oil supply shock",
  demand_OIL_pos = "Oil demand shock",
  price_OIL_pos = "Oil price shock",
  supply_GAS_pos = "Gas supply shock",
  demand_GAS_pos = "Gas demand shock",
  price_GAS_pos = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the legend from the first plot (for shared legend across all plots)
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Arrange the combined plot with shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Energy Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure 6
ggsave(
  filename = "Energy_shocks_pos.png",  # Output filename with .png extension
  plot = combined_plot,                # Combined plot object
  width = 16,                          # Width of the image (in inches)
  height = 12,                         # Height of the image (in inches)
  dpi = 300
)

####################################################################################
#################################ENERGY INFLATION  NEGATIVE shock ##################################
###################################################################################

data$price_OIL_neg <- ifelse(data$oilprice_shock<0,abs(data$oilprice_shock),0)
data$supply_OIL_neg <- ifelse(data$oilsupply_shock<0,abs(data$oilsupply_shock),0)
data$demand_OIL_neg <- ifelse(data$oildemand_shock<0,abs(data$oildemand_shock),0)
data$price_GAS_neg <- ifelse(data$gasprice_shock<0,abs(data$gasprice_shock),0)
data$supply_GAS_neg <- ifelse(data$gassupply_shock<0,abs(data$gassupply_shock),0)
data$demand_GAS_neg <- ifelse(data$gasdemand_shock<0,abs(data$gasdemand_shock),0)


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL_neg = list(shock = "price_OIL_neg", file_name = "CIRF_price_oil"),
  supply_OIL_neg = list(shock = "supply_OIL_neg",  file_name = "CIRF_supply_oil"),
  demand_OIL_neg = list(shock = "demand_OIL_neg", file_name = "CIRF_demand_oil"),
  price_GAS_neg = list(shock = "price_GAS_neg", file_name = "CIRF_price_gas") ,
  supply_GAS_neg = list(shock = "supply_GAS_neg", file_name = "CIRF_supply_gas"),
  demand_GAS_neg = list(shock = "demand_GAS_neg", file_name = "CIRF_demand_gas")
)




# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend

# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Energy Inflation)
  shock_energy <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_energy",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 3,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for energy inflation
  cshock_energy <- rbind(
    cumsum(shock_energy$irf_s1_mean),
    cumsum(shock_energy$irf_s1_low),
    cumsum(shock_energy$irf_s1_up),
    cumsum(shock_energy$irf_s2_mean),
    cumsum(shock_energy$irf_s2_low),
    cumsum(shock_energy$irf_s2_up)
  )
  cshock_energy <- t(cshock_energy)
  cshock_energy <- as.data.frame(cshock_energy)
  cshock_energy$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_energy_long <- rbind(
    cbind(cshock_energy[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_energy[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for energy inflation without a legend
  plot_no_legend <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-3, 3) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18)
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-2.5, 3) +
      geom_hline(yintercept = 0)  
  }
}



# Define improved titles for the plots
plot_titles <- c(
  supply_OIL_neg = "Oil supply shock",
  demand_OIL_neg = "Oil demand shock",
  price_OIL_neg = "Oil price shock",
  supply_GAS_neg = "Gas supply shock",
  demand_GAS_neg = "Gas demand shock",
  price_GAS_neg = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the legend from the first plot (for shared legend across all plots)
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into a single figure without individual legends, using the shared legend at the bottom
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Energy Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure 7
ggsave(
  filename = "Energy_shocks_neg.png",  # Output filename
  plot = combined_plot,                # Combined plot object
  width = 16,                          # Width of the image (in inches)
  height = 12,                         # Height of the image (in inches)
  dpi = 300                            # Resolution of 300 DPI (high quality)
)

####################################################################################
#################################HEADLINE INFLATION positive shock##################
###################################################################################


data$price_OIL_pos <- ifelse(data$oilprice_shock>0,data$oilprice_shock,0)
data$supply_OIL_pos <- ifelse(data$oilsupply_shock>0,data$oilsupply_shock,0)
data$demand_OIL_pos <- ifelse(data$oildemand_shock>0,data$oildemand_shock,0)
data$price_GAS_pos <- ifelse(data$gasprice_shock>0,data$gasprice_shock,0)
data$supply_GAS_pos <- ifelse(data$gassupply_shock>0,data$gassupply_shock,0)
data$demand_GAS_pos <- ifelse(data$gasdemand_shock>0,data$gasdemand_shock,0)


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL_pos = list(shock = "price_OIL_pos", file_name = "CIRF_price_oil"),
  supply_OIL_pos = list(shock = "supply_OIL_pos",  file_name = "CIRF_supply_oil"),
  demand_OIL_pos = list(shock = "demand_OIL_pos", file_name = "CIRF_demand_oil"),
  price_GAS_pos = list(shock = "price_GAS_pos", file_name = "CIRF_price_gas") ,
  supply_GAS_pos = list(shock = "supply_GAS_pos", file_name = "CIRF_supply_gas"),
  demand_GAS_pos = list(shock = "demand_GAS_pos", file_name = "CIRF_demand_gas")
)




# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (overall Inflation)
  shock_overall <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_overall",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 3,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for overall inflation
  cshock_overall <- rbind(
    cumsum(shock_overall$irf_s1_mean),
    cumsum(shock_overall$irf_s1_low),
    cumsum(shock_overall$irf_s1_up),
    cumsum(shock_overall$irf_s2_mean),
    cumsum(shock_overall$irf_s2_low),
    cumsum(shock_overall$irf_s2_up)
  )
  cshock_overall <- t(cshock_overall)
  cshock_overall <- as.data.frame(cshock_overall)
  cshock_overall$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_overall_long <- rbind(
    cbind(cshock_overall[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_overall[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for overall inflation without a legend
  plot_no_legend <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-2, 2) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18)
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-2, 2) +
      geom_hline(yintercept = 0)  
  }
}



# Define improved titles for the plots
plot_titles <- c(
  supply_OIL_pos = "Oil supply shock",
  demand_OIL_pos = "Oil demand shock",
  price_OIL_pos = "Oil price shock",
  supply_GAS_pos = "Gas supply shock",
  demand_GAS_pos = "Gas demand shock",
  price_GAS_pos = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the legend from the first plot (for shared legend across all plots)
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into a single figure without individual legends, using the shared legend at the bottom
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Headline Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure 8
ggsave(
  filename = "Headline_shocks_pos.png",  # Output filename
  plot = combined_plot,                # Combined plot object
  width = 16,                          # Width of the image (in inches)
  height = 12,                         # Height of the image (in inches)
  dpi = 300                            # Resolution of 300 DPI (high quality)
)

####################################################################################
#################################HEADLINE INFLATION negative shock #################
###################################################################################


data$price_OIL_neg <- ifelse(data$oilprice_shock<0,abs(data$oilprice_shock),0)
data$supply_OIL_neg <- ifelse(data$oilsupply_shock<0,abs(data$oilsupply_shock),0)
data$demand_OIL_neg <- ifelse(data$oildemand_shock<0,abs(data$oildemand_shock),0)
data$price_GAS_neg <- ifelse(data$gasprice_shock<0,abs(data$gasprice_shock),0)
data$supply_GAS_neg <- ifelse(data$gassupply_shock<0,abs(data$gassupply_shock),0)
data$demand_GAS_neg <- ifelse(data$gasdemand_shock<0,abs(data$gasdemand_shock),0)


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL_neg = list(shock = "price_OIL_neg", file_name = "CIRF_price_oil"),
  supply_OIL_neg = list(shock = "supply_OIL_neg",  file_name = "CIRF_supply_oil"),
  demand_OIL_neg = list(shock = "demand_OIL_neg", file_name = "CIRF_demand_oil"),
  price_GAS_neg = list(shock = "price_GAS_neg", file_name = "CIRF_price_gas") ,
  supply_GAS_neg = list(shock = "supply_GAS_neg", file_name = "CIRF_supply_gas"),
  demand_GAS_neg = list(shock = "demand_GAS_neg", file_name = "CIRF_demand_gas")
)




# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (overall Inflation)
  shock_overall <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_overall",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 3,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for overall inflation
  cshock_overall <- rbind(
    cumsum(shock_overall$irf_s1_mean),
    cumsum(shock_overall$irf_s1_low),
    cumsum(shock_overall$irf_s1_up),
    cumsum(shock_overall$irf_s2_mean),
    cumsum(shock_overall$irf_s2_low),
    cumsum(shock_overall$irf_s2_up)
  )
  cshock_overall <- t(cshock_overall)
  cshock_overall <- as.data.frame(cshock_overall)
  cshock_overall$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_overall_long <- rbind(
    cbind(cshock_overall[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_overall[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for overall inflation without a legend
  plot_no_legend <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1.5, 1.5) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18)
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-1.5, 1.5) +
      geom_hline(yintercept = 0)  
  }
}



# Define improved titles for the plots
plot_titles <- c(
  supply_OIL_neg = "Oil supply shock",
  demand_OIL_neg = "Oil demand shock",
  price_OIL_neg = "Oil price shock",
  supply_GAS_neg = "Gas supply shock",
  demand_GAS_neg = "Gas demand shock",
  price_GAS_neg = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})

# Extract the legend from the first plot (for shared legend across all plots)
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into a single figure without individual legends, using the shared legend at the bottom
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Headline Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure 9
ggsave(
  filename = "Headline_shocks_neg.png",  # Output filename
  plot = combined_plot,                # Combined plot object
  width = 16,                          # Width of the image (in inches)
  height = 12,                         # Height of the image (in inches)
  dpi = 300                            # Resolution of 300 DPI (high quality)
)


#===============================================================================
# 4). Local projections (gamma = 1)
#===============================================================================

# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Energy Inflation)
  shock_energy <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_energy",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 1,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for energy inflation
  cshock_energy <- rbind(
    cumsum(shock_energy$irf_s1_mean),
    cumsum(shock_energy$irf_s1_low),
    cumsum(shock_energy$irf_s1_up),
    cumsum(shock_energy$irf_s2_mean),
    cumsum(shock_energy$irf_s2_low),
    cumsum(shock_energy$irf_s2_up)
  )
  cshock_energy <- t(cshock_energy)
  cshock_energy <- as.data.frame(cshock_energy)
  cshock_energy$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_energy_long <- rbind(
    cbind(cshock_energy[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_energy[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for energy inflation without a legend
  plot_no_legend <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-2, 2) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18) 
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-2, 2) +
      geom_hline(yintercept = 0)
    
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Energy Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure A3
ggsave(
  filename = "Energy_shocks_gamma1.png",   # Output filename
  plot = combined_plot,             # Combined plot object
  width = 16,                       # Width in inches
  height = 12,                      # Height in inches
  dpi = 300                         # Resolution at 300 DPI for high quality
)


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Headline Inflation)
  shock_overall <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_overall",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_overall","infla_energy","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 1,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for headline inflation
  cshock_overall <- rbind(
    cumsum(shock_overall$irf_s1_mean),
    cumsum(shock_overall$irf_s1_low),
    cumsum(shock_overall$irf_s1_up),
    cumsum(shock_overall$irf_s2_mean),
    cumsum(shock_overall$irf_s2_low),
    cumsum(shock_overall$irf_s2_up)
  )
  cshock_overall <- t(cshock_overall)
  cshock_overall <- as.data.frame(cshock_overall)
  cshock_overall$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_overall_long <- rbind(
    cbind(cshock_overall[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_overall[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for overall inflation without a legend
  plot_no_legend <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1.5, 1.5) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18) 
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-1.5, 1.5) +
      geom_hline(yintercept = 0) 
    
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Headline Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure A4
ggsave(
  filename = "Headline_shocks_gamma1.png",   # Output filename
  plot = combined_plot,               # Combined plot object
  width = 16,                         # Width in inches
  height = 12,                        # Height in inches
  dpi = 300                           # Resolution at 300 DPI for high quality
)

#===============================================================================
# 5). Local projections (gamma = 10)
#===============================================================================


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Energy Inflation)
  shock_energy <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_energy",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 10,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for energy inflation
  cshock_energy <- rbind(
    cumsum(shock_energy$irf_s1_mean),
    cumsum(shock_energy$irf_s1_low),
    cumsum(shock_energy$irf_s1_up),
    cumsum(shock_energy$irf_s2_mean),
    cumsum(shock_energy$irf_s2_low),
    cumsum(shock_energy$irf_s2_up)
  )
  cshock_energy <- t(cshock_energy)
  cshock_energy <- as.data.frame(cshock_energy)
  cshock_energy$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_energy_long <- rbind(
    cbind(cshock_energy[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_energy[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for energy inflation without a legend
  plot_no_legend <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1.5, 1.5) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18) 
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-1.5, 1.5) +
      geom_hline(yintercept = 0)
    
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Energy Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure A5
ggsave(
  filename = "Energy_shocks_gamma10.png",   # Output filename
  plot = combined_plot,             # Combined plot object
  width = 16,                       # Width in inches
  height = 12,                      # Height in inches
  dpi = 300                         # Resolution at 300 DPI for high quality
)



# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Headline Inflation)
  shock_overall <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_overall",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_overall","infla_energy","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "z",
    use_logistic = TRUE,
    lag_switching = FALSE,
    gamma = 10,
    confint = 1.645,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for headline inflation
  cshock_overall <- rbind(
    cumsum(shock_overall$irf_s1_mean),
    cumsum(shock_overall$irf_s1_low),
    cumsum(shock_overall$irf_s1_up),
    cumsum(shock_overall$irf_s2_mean),
    cumsum(shock_overall$irf_s2_low),
    cumsum(shock_overall$irf_s2_up)
  )
  cshock_overall <- t(cshock_overall)
  cshock_overall <- as.data.frame(cshock_overall)
  cshock_overall$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_overall_long <- rbind(
    cbind(cshock_overall[,1:3], regime = "high", time = seq(1,12)),
    cbind(cshock_overall[,4:6], regime = "low", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for overall inflation without a legend
  plot_no_legend <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1, 1) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18) 
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-1, 1) +
      geom_hline(yintercept = 0) 
    
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Headline Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure A6
ggsave(
  filename = "Headline_shocks_gamma10.png",   # Output filename
  plot = combined_plot,               # Combined plot object
  width = 16,                         # Width in inches
  height = 12,                        # Height in inches
  dpi = 300                           # Resolution at 300 DPI for high quality
)


#===============================================================================
# 6). Local projections (Alternative definition of electricity transition)
#===============================================================================


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Energy Inflation)
  shock_energy <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_energy",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "d",
    use_logistic = F,
    lag_switching = FALSE,
    confint = 1.64,
    hor = 12
  )
  
  
  
  # 2. Calculate cumulative IRF for energy inflation
  cshock_energy <- rbind(
    cumsum(shock_energy$irf_s1_mean),
    cumsum(shock_energy$irf_s1_low),
    cumsum(shock_energy$irf_s1_up),
    cumsum(shock_energy$irf_s2_mean),
    cumsum(shock_energy$irf_s2_low),
    cumsum(shock_energy$irf_s2_up)
  )
  cshock_energy <- t(cshock_energy)
  cshock_energy <- as.data.frame(cshock_energy)
  cshock_energy$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_energy) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_energy) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_energy_long <- rbind(
    cbind(cshock_energy[,1:3], regime = "low", time = seq(1,12)),
    cbind(cshock_energy[,4:6], regime = "high", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for energy inflation without a legend
  plot_no_legend <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1.5, 1.5) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_energy_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18)
      ) +
      scale_x_continuous(breaks = 1:12) +
      ylim(-1.5, 1.5) +
      geom_hline(yintercept = 0)
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Energy Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure A7
ggsave(
  filename = "Energy_shocks_r_2.png",   # Output filename
  plot = combined_plot,             # Combined plot object
  width = 16,                       # Width in inches
  height = 12,                      # Height in inches
  dpi = 300                         # Resolution at 300 DPI for high quality
)


# List of shocks to analyze: oil shocks and gas shocks
shocks_list <- list(
  price_OIL = list(shock = "oilprice_shock", file_name = "CIRF_price_oil"),
  supply_OIL = list(shock = "oilsupply_shock",  file_name = "CIRF_supply_oil"),
  demand_OIL = list(shock = "oildemand_shock", file_name = "CIRF_demand_oil"),
  price_GAS = list(shock = "gasprice_shock", file_name = "CIRF_price_gas") ,
  supply_GAS = list(shock = "gassupply_shock", file_name = "CIRF_supply_gas"),
  demand_GAS = list(shock = "gasdemand_shock", file_name = "CIRF_demand_gas")
)

# Initialize a list to store the plots
plot_list <- list()
legend_plot <- NULL  # Variable to store the first plot with the legend


# Loop over each type of shock
for (i in seq_along(shocks_list)) {
  shock_type <- names(shocks_list)[i]
  
  # Define the current shock
  shock_vars <- shocks_list[[shock_type]]
  
  # 1. Model for the shock (Headline Inflation)
  
  
  shock_overall <- lp_nl_panel(
    data_set = data,
    data_sample = "Full",
    endog_data = "infla_overall",
    cumul_mult = FALSE,
    shock = shock_vars$shock,
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "individual",
    robust_cov = "vcovNW",
    c_exog_data = c("it","diff_fx","exp_to_gdp"),
    l_exog_data = c("infla_energy","infla_overall","diff_fx","it","exp_to_gdp"),
    lags_exog_data = 12,
    switching = "d",
    use_logistic = F,
    lag_switching = FALSE,
    confint = 1.64,
    hor = 12
  )
  
  # 2. Calculate cumulative IRF for headline inflation
  cshock_overall <- rbind(
    cumsum(shock_overall$irf_s1_mean),
    cumsum(shock_overall$irf_s1_low),
    cumsum(shock_overall$irf_s1_up),
    cumsum(shock_overall$irf_s2_mean),
    cumsum(shock_overall$irf_s2_low),
    cumsum(shock_overall$irf_s2_up)
  )
  cshock_overall <- t(cshock_overall)
  cshock_overall <- as.data.frame(cshock_overall)
  cshock_overall$Time <- seq(1, 12)
  
  # Rename columns to avoid duplicates
  # Rename columns to avoid duplicates
  colnames(cshock_overall) <- c("s1_mean","s1_low","s1_up","s2_mean","s2_low","s2_up","Time")
  
  # Prepare data in long format for ggplot
  colnames(cshock_overall) <- c("mean", "low", "up","mean", "low", "up", "time")
  cshock_overall_long <- rbind(
    cbind(cshock_overall[,1:3], regime = "low", time = seq(1,12)),
    cbind(cshock_overall[,4:6], regime = "high", time = seq(1,12))
  )
  
  # Determine if X-axis label should be shown (only for bottom row)
  show_x_label <- i > 4  # Only for the last two plots (indices 5 and 6)
  
  # 3. Create the plot for overall inflation without a legend
  plot_no_legend <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
    geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
    geom_line(aes(y = mean, color = regime), size = 1) +
    labs(
      title = paste(shock_type, "shock"),
      x = ifelse(show_x_label, "Horizon", "")  # Only show X-axis label for bottom row, else use empty string
    ) +
    scale_fill_manual(values = c("darkgreen", "red")) +
    scale_color_manual(values = c("darkgreen", "red")) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend from individual plots
      panel.background = element_blank(),  
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 13, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title.y = element_blank(),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(breaks = 1:12) +
    ylim(-1, 1) +
    geom_hline(yintercept = 0)
  
  # Add the plot without a legend to the list
  plot_list[[shock_type]] <- plot_no_legend
  
  # Store the first plot with a legend for later use (extract the legend)
  if (is.null(legend_plot)) {
    legend_plot <- ggplot(cshock_overall_long, aes(x = time, group = regime)) +
      geom_ribbon(aes(ymin = low, ymax = up, fill = regime), alpha = 0.2) +
      geom_line(aes(y = mean, color = regime), size = 1) +
      scale_fill_manual(values = c("darkgreen", "red")) +
      scale_color_manual(values = c("darkgreen", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 18), 
            legend.title = element_text(size = 18)
      )
  }
}

# Define improved titles for each plot
plot_titles <- c(
  supply_OIL = "Oil supply shock",
  demand_OIL = "Oil demand shock",
  price_OIL = "Oil price shock",
  supply_GAS = "Gas supply shock",
  demand_GAS = "Gas demand shock",
  price_GAS = "Gas price shock"
)

# Apply titles and adjust font sizes in one step
plot_list <- lapply(names(plot_list), function(shock_type) {
  plot_list[[shock_type]] +
    labs(title = plot_titles[[shock_type]]) +  # Assign improved title
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 18),    # Larger title size
          legend.text = element_text(size = 18)    # Larger legend text size
    )
})


# Extract the shared legend from the first plot
g <- ggplotGrob(legend_plot)
legend <- g$grobs[[which(g$layout$name == "guide-box")]]

# Combine all plots into one figure with a shared legend and Y-axis label
combined_plot <- grid.arrange(
  grobs = c(plot_list[1], plot_list[2], plot_list[3],    # First column: oil shocks
            plot_list[4], plot_list[5], plot_list[6]),   # Second column: gas shocks
  layout_matrix = matrix(c(1, 4,    # First row: oil_shocks_1 in col 1, gas_shocks_1 in col 2
                           2, 5,    # Second row: oil_shocks_2 in col 1, gas_shocks_2 in col 2
                           3, 6),   # Third row: oil_shocks_3 in col 1, gas_shocks_3 in col 2
                         nrow = 3, byrow = TRUE),
  bottom = legend,   # Add the shared legend at the bottom
  left = textGrob("Headline Inflation (%)", rot = 90, gp = gpar(fontsize = 18))  # Shared Y-axis label
)

# Figure A8
ggsave(
  filename = "Headline_shocks_r_2.png",   # Output filename
  plot = combined_plot,               # Combined plot object
  width = 16,                         # Width in inches
  height = 12,                        # Height in inches
  dpi = 300                           # Resolution at 300 DPI for high quality
)
