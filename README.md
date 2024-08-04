# Economic-Complexity-and-Its-Role-in-Shaping-Renewable-Energy-Consumption-Patterns (R Codes)
# Load necessary packages					
install.packages(c("readxl"))					
library(readxl)					
					
# Load the data					
df <- read_excel("C:/Users/ayode/Desktop/RESEARCH PUBLICATIONS/Economic Complexity on Renewable Energy/Extended Data for Impact of Eco Complexity on Ren Energy.xlsx")					
					
# Log-transform the variables					
variables_to_log <- c("RenE", "GDPG", "TFFC", "TO", "FDI") # replace with your variables					
df[paste0("ln_", variables_to_log)] <- log(df[variables_to_log])					
					
					
Descriptive Statistics					
# Load necessary packages					
install.packages("psych")					
library(psych)					
					
# Calculate descriptive statistics					
desc_stats <- describe(df)					
					
# Print the descriptive statistics					
print(desc_stats)					
					
					
Cross-Sectional Dependence					
# Remove rows with NA values					
pdata <- na.omit(pdata)					
					
# Check for infinite values and replace them if necessary					
pdata[pdata == -Inf] <- NA					
pdata[pdata == Inf] <- NA					
					
# Perform the Pesaran CD test					
pesaran_test <- pcdtest(ln_RenE ~ ln_GDPG + ln_TFFC + ln_TO + ln_FDI + ECI + FD, data = pdata, test = "cd")					
					
# Perform the Breusch-Pagan LM test					
bp_test <- pcdtest(ln_RenE ~ ln_GDPG + ln_TFFC + ln_TO + ln_FDI + ECI + FD, data = pdata, test = "lm")					
					
# Print the test results					
print(pesaran_test)					
print(bp_test)					
					
					
CS-ARDL					
# Install necessary packages					
install.packages("dLagM")					
library(dLagM)					
					
# Assuming 'pdata' is your existing panel data frame					
					
# Estimate the CS-ARDL model					
model <- ardlBound(ln_RenE ~ ln_GDPG + ln_TFFC + ln_TO + ln_FDI + ECI + FD, data = pdata, max.p = 2, max.q = 2)					
					
# Print the summary of the model					
summary(model)					
					
					
# Load necessary packages					
library(readxl)					
library(plm)					
library(dplyr)					
					
# Import data					
data <- read_excel("C:/Users/ayode/Desktop/RESEARCH PUBLICATIONS/Economic Complexity on Renewable Energy/Impact of Economic Complexity on Renewable Energy Consumption.xlsx")					
					
# Create log transformations					
data <- data %>%					
  mutate(lRenE = log(RenE),					
         lGDPG = log(GDPG),					
         lTFFC = log(TFFC),					
         lTO = log(TO),					
         lFDI = log(FDI))					
					
# Set up panel data structure					
pdata <- pdata.frame(data, index = c("Country_id", "Year"))					
					
# Initialize lists to store results					
short_run <- list()					
long_run <- list()					
					
# Loop over panels					
for(i in unique(pdata$Country_id)) {					
  					
  # Subset data for panel i					
  panel_data <- pdata[pdata$Country_id == i, ]					
  					
  # Estimate short-run coefficients using OLS					
  short_run_model <- lm(lRenE ~ lGDPG + lTFFC + lTO + lFDI + ECI + FD, data = panel_data)					
  short_run[[i]] <- coef(short_run_model)					
  					
  # Calculate long-run coefficients					
  long_run_model <- dynlm(lRenE ~ L(lGDPG) + L(lTFFC) + L(lTO) + L(lFDI) + L(ECI) + L(FD), data = panel_data)					
  long_run[[i]] <- coef(long_run_model)					
}					
					
# Calculate mean of short-run and long-run coefficients					
short_run_mean <- sapply(short_run, mean)					
long_run_mean <- sapply(long_run, mean)					
					
# Print results					
print(short_run_mean)					
print(long_run_mean)					
					
					
PMG Result (Short run and Long run)					
# Load necessary libraries					
library(readxl)					
library(dplyr)					
library(dynlm)					
					
# Read the data					
data <- read_excel("C:/Users/ayode/Desktop/RESEARCH PUBLICATIONS/Economic Complexity on Renewable Energy/Impact of Economic Complexity on Renewable Energy Consumption.xlsx")					
					
# Log the variables					
data <- data %>%					
  mutate(log_RenE = log(RenE),					
         log_GDPG = log(GDPG),					
         log_TFFC = log(TFFC),					
         log_TO = log(TO),					
         log_FDI = log(FDI))					
					
# Create differenced and lagged variables					
data <- data %>%					
  group_by(Country_id) %>%					
  mutate(D_log_RenE = c(NA, diff(log_RenE)),					
         D_log_GDPG = c(NA, diff(log_GDPG)),					
         D_log_TFFC = c(NA, diff(log_TFFC)),					
         D_log_TO = c(NA, diff(log_TO)),					
         D_log_FDI = c(NA, diff(log_FDI)),					
         L_log_RenE = lag(log_RenE),					
         L_log_GDPG = lag(log_GDPG),					
         L_log_TFFC = lag(log_TFFC),					
         L_log_TO = lag(log_TO),					
         L_log_FDI = lag(log_FDI),					
         L_ECI = lag(ECI),					
         L_FD = lag(FD))					
					
# Define the short-run model					
short_run_model <- dynlm(D_log_RenE ~ L_log_RenE + D_log_GDPG + D_log_TFFC + D_log_TO + D_log_FDI + L_ECI + L_FD, data = data)					
					
# Print the summary of the short-run model					
summary(short_run_model)					
					
					
# Define the long-run model					
long_run_model <- dynlm(log_RenE ~ L(log_GDPG) + L(log_TFFC) + L(log_TO) + L(log_FDI) + L(ECI) + L(FD), data = data)					
					
# Print the summary of the long-run model					
summary(long_run_model)					
![image](https://github.com/user-attachments/assets/90162f4b-40fb-4496-be97-4e5739de6a33)
