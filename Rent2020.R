# Load the libraries
library(tidyverse)
library(lubridate)

# Create data frame "rent" from CSV file w/headers
rent <- read_csv("Rent.20200221.csv")

# Examine the structure of Rent.csv
summary(rent)

# Filter the data for a specific program
rentSDS <- filter(rent, 
                  Peers == ''|
                  Peers == '') 

# Draw boxplots of rent by location
boxplot(rentbd1 ~ Peers, data = rent,
        col = "lightsteelblue3",
        notch = T,
        xlab = "Location",
        ylab = "Monthly rent ($)",
        main = "Monthly rent for 1 BR apartments by city\n2015-2019")