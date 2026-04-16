setwd("D:/Fall 25-26/Data Science/Project")
list.files()
data <- read.csv("data_science_project.csv", stringsAsFactors = FALSE)
head(data)
dim(data)




data[data == ""] <- NA
data[data == "NA"] <- NA

sapply(data, function(x) sum(is.na(x)))

num_cols <- sapply(data, is.numeric)
for(col in names(data)[num_cols]) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data$flood_risk[is.na(data$flood_risk)]     <- get_mode(data$flood_risk)
data$drought_risk[is.na(data$drought_risk)] <- get_mode(data$drought_risk)

data$wildfire_incidents <- as.numeric(data$wildfire_incidents)
data$air_quality_index  <- as.numeric(data$air_quality_index)
data$climate_risk_score <- as.numeric(data$climate_risk_score)

data$wildfire_incidents[is.na(data$wildfire_incidents)] <- mean(data$wildfire_incidents, na.rm = TRUE)
data$air_quality_index[is.na(data$air_quality_index)] <- mean(data$air_quality_index, na.rm = TRUE)
data$climate_risk_score[is.na(data$climate_risk_score)] <- mean(data$climate_risk_score, na.rm = TRUE)

sapply(data, function(x) sum(is.na(x)))




boxplot(data$wildfire_incidents,
        main = "Outliers in Wildfire Incidents",
        col = "lightblue",
        ylab = "Wildfire Incidents")

Q1 <- quantile(data$wildfire_incidents, 0.25, na.rm = TRUE)
Q3 <- quantile(data$wildfire_incidents, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

data_clean <- data[ data$wildfire_incidents >= (Q1 - 1.5 * IQR_val) & data$wildfire_incidents <= (Q3 + 1.5 * IQR_val), ]

nrow(data)        
nrow(data_clean)




data_clean$country      <- as.factor(data_clean$country)
data_clean$region       <- as.factor(data_clean$region)
data_clean$flood_risk   <- as.factor(data_clean$flood_risk)
data_clean$drought_risk <- as.factor(data_clean$drought_risk)

table(data_clean$flood_risk)

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

data_clean$flood_risk <- as.character(data_clean$flood_risk)
data_clean$flood_risk[!data_clean$flood_risk %in% c("High", "Medium", "Low")] <- get_mode(data_clean$flood_risk[data_clean$flood_risk %in% c("High", "Medium", "Low")])
data_clean$flood_risk <- as.factor(data_clean$flood_risk)

str(data_clean)
table(data_clean$flood_risk)
table(data_clean$drought_risk)




min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data_clean$avg_temperature_c_norm  <- min_max_norm(data_clean$avg_temperature_c)
data_clean$co2_emissions_mt_norm   <- min_max_norm(data_clean$co2_emissions_mt)
data_clean$climate_risk_score_norm <- min_max_norm(data_clean$climate_risk_score)

summary(data_clean$avg_temperature_c_norm)
summary(data_clean$co2_emissions_mt_norm)
summary(data_clean$climate_risk_score_norm)




sum(duplicated(data_clean))
data_no_dup <- data_clean[!duplicated(data_clean), ]

nrow(data_clean)   
nrow(data_no_dup)  




table(data_no_dup$flood_risk)
data_filtered <- data_no_dup[data_no_dup$flood_risk %in% c("High", "Medium"), ]

table(data_filtered$flood_risk)
nrow(data_filtered)




sum(data_filtered$co2_emissions_mt < 0)
sum(data_filtered$heatwave_days < 0)
sum(data_filtered$wildfire_incidents < 0)
sum(data_filtered$air_quality_index < 0)
sum(data_filtered$population_affected_m < 0)

data_valid <- data_filtered[
  data_filtered$co2_emissions_mt >= 0 &
    data_filtered$heatwave_days >= 0 &
    data_filtered$wildfire_incidents >= 0 &
    data_filtered$air_quality_index >= 0 &
    data_filtered$population_affected_m >= 0, ]

nrow(data_filtered)
nrow(data_valid)




library(dplyr)
data_valid$flood_risk <- droplevels(data_valid$flood_risk)
table(data_valid$flood_risk)

min_size <- min(table(data_valid$flood_risk))
min_size

set.seed(42)
data_balanced <- data_valid %>%
  group_by(flood_risk) %>%
  sample_n(min_size) %>%
  ungroup()

table(data_balanced$flood_risk)
nrow(data_balanced)




set.seed(123)
train_index <- sample(1:n, size = 0.7 * n)

train_data <- data_balanced[train_index, ]
test_data  <- data_balanced[-train_index, ]

nrow(train_data)
nrow(test_data)




tapply(data_balanced$avg_temperature_c, data_balanced$flood_risk, summary)
tapply(data_balanced$co2_emissions_mt, data_balanced$flood_risk, summary)
tapply(data_balanced$climate_risk_score, data_balanced$flood_risk, summary)
tapply(data_balanced$heatwave_days, data_balanced$flood_risk, summary)




tapply(data_balanced$avg_temperature_c, data_balanced$drought_risk, mean, na.rm = TRUE)
tapply(data_balanced$co2_emissions_mt, data_balanced$drought_risk, mean, na.rm = TRUE)
tapply(data_balanced$climate_risk_score, data_balanced$drought_risk, mean, na.rm = TRUE)




tapply(data_balanced$co2_emissions_mt, data_balanced$region, sd, na.rm = TRUE)
tapply(data_balanced$co2_emissions_mt, data_balanced$region, var, na.rm = TRUE)
tapply(data_balanced$co2_emissions_mt, data_balanced$region, IQR, na.rm = TRUE)
tapply(data_balanced$co2_emissions_mt, data_balanced$region, 
       function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))