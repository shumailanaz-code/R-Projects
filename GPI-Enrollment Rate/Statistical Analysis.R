
### Step 1: Pre Processing of the data


# Load Libraries

install.packages('tidyverse')
library(tidyverse)
library(dplyr)
install.packages('corrplot')
library(corrplot)
install.packages('DescTools')
library(DescTools)
install.packages('forecast')
library(forecast)

# Load Data

data <- read.csv("Education_Data.csv")

# First few rows of the data

head(data)

# Last few rows of the data

tail(data)

# Data Structure

str(data)

# Data Summary

summary(data)

# Removing missing values

education_data <- na.omit(data)

# Find out is there any missing values left

sum(is.na(education_data))

# Outlier Detection

ggplot(education_data) + 
  aes(x = "", y = PER_Female) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = PER_Male) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = PER_GPI) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = LSER_Female) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = LSER_Male) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = LSER_GPI) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = USER_Female) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = USER_Male) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = "", y = USER_GPI) +
  geom_boxplot(fill = "#8eaccd") +
  theme_minimal()

# Visualization of Enrollment levels and Gender Parity Index 

ggplot(education_data) + 
  aes(x = PER_Female) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = PER_Male) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = PER_GPI) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = LSER_Female) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = LSER_Male) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = LSER_GPI) +
  geom_histogram(bins = 15L,  color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = USER_Female) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = USER_Male) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

ggplot(education_data) + 
  aes(x = USER_GPI) +
  geom_histogram(bins = 15L, color = "black", fill = "#8eaccd") +
  theme_minimal()

### Step 2: Descriptive Statistical Analysis


# Calculate country wise Mean and Median of GPI

mean_gpi <- education_data %>%
  group_by(Countries) %>%
  summarise(across(c(4,7,10), list(mean = mean)))

median_gpi <- education_data %>%
  group_by(Countries) %>%
  summarise(across(c(4,7,10), list(median = median)))

# Calculate country wise Mean and Median of Enrollment rate

mean_er <- education_data %>%
  group_by(Countries) %>%
  summarise(across(c(2,3,5,6,8,9), list(mean = mean)))

median_er <- education_data %>%
  group_by(Countries) %>%
  summarise(across(c(2,3,5,6,8,9), list(median = median)))

# Calculate country wise Standard Deviation of Enrollment rate

sd_er <- education_data %>%
  group_by(Countries) %>%
  summarise(across(c(2,3,5,6,8,9), list(sd = sd)))

# Calculate Skewness of Enrollment rate and Gender parity index

sk_gpi <- education_data %>%
  summarise(across(c(5,8,11), list(skew = Skew)))

sk_er <- education_data %>%
  summarise(across(c(3,4,6,7,9,10), list(skew = Skew)))

# Calculate Kurtosis of Enrollment rate and Gender parity index

kt_gpi <- education_data %>%
  summarise(across(c(5,8,11), list(kurt = Kurt)))

kt_er <- education_data %>%
  summarise(across(c(3,4,6,7,9,10), list(kurt = Kurt)))


### Step 3: Correlation Analysis

# Draw Scatter plot to find linear relationship for Pearson Correlation

plot(education_data$PER_Female, education_data$LSER_Female)
plot(education_data$PER_Female, education_data$USER_Female)

# Calculate Pearson Correlation between female enrollment rate

round(cor(education_data$PER_Female, education_data$LSER_Female), digits = 2)
round(cor(education_data$PER_Female, education_data$USER_Female), digits = 2)

# Calculate Pearson Correlation between gender parity index

round(cor(education_data$PER_GPI, education_data$LSER_GPI), digits = 2)
round(cor(education_data$PER_GPI, education_data$USER_GPI), digits = 2)

# Remove 'Countries' and 'Years' columns to create new data set for correlation plot

cor_data <- education_data[, -which(names(education_data) %in% c("Countries", "Years"))]

# Draw Correlation Plot 

corrplot(cor(cor_data), type = "lower", method = "number", tl.srt = 45)


### Step 4: Hypothesis Testing


# Hypothesis Test 1

# Check the normality of female enrollment rate at all levels of education by QQ Plot

qqnorm(education_data$PER_Female, pch = 1, frame = FALSE)
qqline(education_data$PER_Female, col = "steelblue", lwd = 2)

qqnorm(education_data$LSER_Female, pch = 1, frame = FALSE)
qqline(education_data$LSER_Female, col = "steelblue", lwd = 2)

qqnorm(education_data$USER_Female, pch = 1, frame = FALSE)
qqline(education_data$USER_Female, col = "steelblue", lwd = 2)

# Check the normality of female enrollment rate at all levels of education by Shapiro-Wilk Test

shapiro.test(education_data$PER_Female)
shapiro.test(education_data$LSER_Female)
shapiro.test(education_data$USER_Female)

# Handle Non-Normal Data by Log, Square Root, and Cube Root Transformation

log_PF <- log10(education_data$PER_Female)
log_LSF <- log10(education_data$LSER_Female)
log_USF <- log10(education_data$USER_Female)

shapiro.test(log_PF)
shapiro.test(log_LSF)
shapiro.test(log_USF)

sqrt_PF <- sqrt(education_data$PER_Female)
sqrt_LSF <- sqrt(education_data$LSER_Female)
sqrt_USF <- sqrt(education_data$USER_Female)

shapiro.test(sqrt_PF)
shapiro.test(sqrt_LSF)
shapiro.test(sqrt_USF)

cube_PF <- education_data$PER_Female^(1/3)
cube_LSF <- education_data$LSER_Female^(1/3)
cube_USF <- education_data$USER_Female^(1/3)

shapiro.test(cube_PF)
shapiro.test(cube_LSF)
shapiro.test(cube_USF)

# Perform Kruskal-Wallis Test

kruskal.test(education_data$PER_Female, education_data$LSER_Female, education_data$USER_Female)

# Hypothesis Test 2

# Check the normality of GPI data at lower and upper secondary levels by QQ Plot

qqnorm(education_data$LSER_GPI, pch = 1, frame = FALSE)
qqline(education_data$LSER_GPI, col = "steelblue", lwd = 2)

qqnorm(education_data$USER_GPI, pch = 1, frame = FALSE)
qqline(education_data$USER_GPI, col = "steelblue", lwd = 2)

# Check the normality of GPI data at lower and upper secondary levels by Shapiro-Wilk Test

shapiro.test(education_data$LSER_GPI)
shapiro.test(education_data$USER_GPI)

# Handle Non-Normal Data by Log, Square Root, and Cube Root Transformation

log_LSGPI <- log10(education_data$LSER_GPI)
log_USGPI <- log10(education_data$USER_GPI)

shapiro.test(log_LSGPI)
shapiro.test(log_USGPI)

sqrt_LSGPI <- sqrt(education_data$LSER_GPI)
sqrt_USGPI <- sqrt(education_data$USER_GPI)

shapiro.test(sqrt_LSGPI)
shapiro.test(sqrt_USGPI)

cube_LSGPI <- education_data$LSER_GPI^(1/3)
cube_USGPI <- education_data$USER_GPI^(1/3)

shapiro.test(cube_LSGPI)
shapiro.test(cube_USGPI)

# Perform Wilcoxon Rank Sum Test

wilcox.test(education_data$LSER_GPI, education_data$USER_GPI)


### Step 5: Regression Analysis


# Correlation Matrix to check correlation between GPI and enrollment rate

corrplot(cor(cor_data), type = 'lower', tl.srt = 45)

# Multiple Linear Regression
# Find out relationship between primary GPI and primary enrollment rate of both genders

reg_PGPI <- lm(PER_GPI ~ PER_Female + PER_Male, data = education_data)
summary(reg_PGPI)

# Linearity

pairs(education_data[,c(5,3,4)], lower.panel = NULL, pch = 19, cex = 0.2)

# Residuals' Independence

plot(reg_PGPI,1)

# Normality of residuals

plot(reg_PGPI,2)

# Homoscedasticity

plot(reg_PGPI,3)


### Step 5: Time Series Analysis


# Calculate year wise mean for Primary female enrollment rate and Primary GPI

mean_FPER_years <- education_data %>%
  group_by(Years) %>%
  summarise(across(c(2), list(mean = mean)))

mean_PGPI_years <- education_data %>%
  group_by(Years) %>%
  summarise(across(c(4), list(mean = mean)))

# Create time series objects for Primary female enrollment rate and Primary GPI

ts_FPER <- ts(mean_FPER_years$PER_Female_mean, start = c(2001), frequency = 1)
ts_FPER

ts_PGPI <- ts(mean_PGPI_years$PER_GPI_mean, start = c(2001), frequency = 1)
ts_PGPI

# Draw time series object of enrollment rate and GPI

plot.ts(ts_FPER)
plot.ts(ts_PGPI)

# Apply Holt-winters Model on enrollment rate and GPI

enrollmentrate <- HoltWinters(ts_FPER, gamma = FALSE)
enrollmentrate

plot(enrollmentrate)

genderparityindex <- HoltWinters(ts_PGPI, gamma = FALSE)
genderparityindex

plot(genderparityindex)

# Forecast Female Primary enrollment rate and primary GPI for next 5 years

enrollmentrateforecast <- forecast(enrollmentrate, h=5)
enrollmentrateforecast

plot(enrollmentrateforecast)

genderparityindexforecast <- forecast(genderparityindex, h=5)
genderparityindexforecast

plot(genderparityindexforecast)

# Draw Correlogram to check non-zero autocorrelations

acf(enrollmentrateforecast$residuals, lag.max = 4, na.action = na.pass)
acf(genderparityindexforecast$residuals, lag.max = 4, na.action = na.pass)

# Perform Ljung-Box test to check non-zero autocorrelations

Box.test(enrollmentrateforecast$residuals, lag = 4, type = "Ljung-Box")
Box.test(genderparityindexforecast$residuals, lag = 4, type = "Ljung-Box")

# Draw time plot of forecast errors

plot.ts(enrollmentrateforecast$residuals)
plot.ts(genderparityindexforecast$residuals)

