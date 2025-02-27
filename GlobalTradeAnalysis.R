# Load necessary libraries
library(dplyr)
library(readxl)

# Load the dataset from Excel
trade <- read_excel("Cleaned_data_with_R_Code.xlsx", sheet = "Cleaned Data")

# View dataset structure
str(trade)
summary(trade)  # Check missing values

# Make a copy and rename columns for clarity
business <- trade
names(business) <- c("Country", "Year", "Commoditycode", "Commodity", "Flow",
                     "Dollars", "Weight", "Quantityname", "Quantity", "Category")

# Approach 1: Eliminate all missing values
trader <- na.omit(business)

# Approach 2: Identify and analyze missing values

# Create an indicator for missing weights and quantities
business$Missing <- ifelse(
  business$Weight < 1 | is.na(business$Weight) | 
  business$Quantity < 1 | is.na(business$Quantity), 1, 0
)

# Count missing values
table(business$Missing)

# Filter data by country with missing values
missedinfo_aus <- filter(business, Country == "Australia" & Missing == 1)
missedinfo_can <- filter(business, Country == "Canada" & Missing == 1)
missedinfo_usa <- filter(business, Country == "USA" & Missing == 1)

# Calculate missing trade value per country
sum(missedinfo_aus$Dollars, na.rm = TRUE)
sum(missedinfo_can$Dollars, na.rm = TRUE)
sum(missedinfo_usa$Dollars, na.rm = TRUE)

# Analyze missing info by trade flow
aus_export <- filter(missedinfo_aus, Flow == "Export")
sum(aus_export$Dollars, na.rm = TRUE)

can_export <- filter(missedinfo_can, Flow == "Export")
sum(can_export$Dollars, na.rm = TRUE)

usa_export <- filter(missedinfo_usa, Flow == "Export")
sum(usa_export$Dollars, na.rm = TRUE)

# Exclude rows with missing values
business_clean <- filter(business, Missing == 0)

# Aggregate data for Tableau visualization
tableau <- business_clean %>% select(-Missing)
write.csv(tableau, "tableau.csv", row.names = FALSE)

# Trade balance calculation (Export - Import)
tradebalance_aus <- sum(filter(business_clean, Country == "Australia" & Flow == "Export")$Dollars, na.rm = TRUE) - 
                     sum(filter(business_clean, Country == "Australia" & Flow == "Import")$Dollars, na.rm = TRUE)

tradebalance_can <- sum(filter(business_clean, Country == "Canada" & Flow == "Export")$Dollars, na.rm = TRUE) - 
                     sum(filter(business_clean, Country == "Canada" & Flow == "Import")$Dollars, na.rm = TRUE)

tradebalance_usa <- sum(filter(business_clean, Country == "USA" & Flow == "Export")$Dollars, na.rm = TRUE) - 
                     sum(filter(business_clean, Country == "USA" & Flow == "Import")$Dollars, na.rm = TRUE)

# Print trade balances
print(tradebalance_aus)
print(tradebalance_can)
print(tradebalance_usa)
