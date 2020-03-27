# Load the libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# Create data frame "rent" from CSV file w/headers
rent <- read_csv("Rent.20200221.csv")

# Examine the structure of Rent.csv
summary(Rent)

# Filter the data for a specific program
peersSDS <- filter(Rent, Peers %in% c('Uchicago, Toyota Tech, Northwestern','UWashington', 'UFlorida', 
                                      'Duke Main, UNC', 'UCLA,USC, Cal Tech', 'UCDavis', 'UMichigan',
                                      'Wharton, UPenn', 'CMU', 'Stanford', 'UCBerkeley','Colorado State', 
                                      'UMissouri', 'Columbia, NYU','Cornell', 'Princeton', 'Harvard, MIT', 
                                      'Yale', 'Ohio State', 'SMU, North Texas State', 'UT')) 

peersPhysics <- filter(Rent, Peers %in% c('Harvard, MIT', 'UCLA,USC, Cal Tech', 'Princeton', 'Stanford', 
                                          'UCBerkeley', 'Cornell', 'Uchicago, Toyota Tech, Northwestern',
                                          'UIUC, Uillinois', 'UCSB', 'Columbia, NYU', 'Yale', 'UMaryland', 'UT'))

peersHDFS <- filter(Rent, Peers %in% c('Florida State', 'Georgetown', 'Columbia, NYU', 'Ohio State',
                                       'Penn State', 'Stanford', 'Tufts', 'UCLA,USC, Cal Tech', 
                                       'Uconnecticut', 'UMaryland', 'John Hopkins, UMaryland', 
                                       'Wharton, UPenn', 'UIllinois', 'UMinnesota', 'UMissouri', 'Vanderbilt',
                                       'UWisconsin', 'UT'))

peersCS <- filter(Rent, Peers %in% c('Harvard, MIT', 'UWashington', 'CMU', 'Princeton', 'UCBerkeley',
                                     'Stanford', 'Cornell', 'UT'))

peersNeuro <- filter(Rent, Peers %in% c('John Hopkins, UMaryland','Columbia, NYU', 'Stanford', 'Princeton',
                                        'Baylor College of Medicine, Rice', 'WashingtonU', 'UT'))

peersChem <- filter(Rent, Peers %in% c('Duke Main, UNC', 'UMichigan', 'Purdue', 'UWashington', 'UWisconsin',
                                       'Uchicago, Toyota Tech, Northwestern', 'UMinnesota', 'UCLA,USC, Cal Tech',
                                       'UColorado', 'UT'))

peersNutriSci <- filter(Rent, Peers %in% c('Duke Main, UNC', 'Texas A&M', 'UCBerkeley', 'BostonU, Harvard, MIT, Medford',
                                           'UT'))

peersEEBPB <- filter(Rent, Peers %in% c('BostonU, Harvard, MIT, Medford', 'Cal Tech', 'Columbia, NYU', 
                                        'Cornell', 'Duke Main, UNC', 'Harvard, MIT', 'Michigan State', 
                                        'North Texas State', 'Oregon State', 'Princeton', 'Stanford', 
                                        'Texas A&M', 'UArizona', 'UCBerkeley', 'UCDavis', 'Columbia, NYU',
                                        'UCLA,USC, Cal Tech', 'UColorado', 'UConnecticut', 'UCSD, Scripps',
                                        'UFlorida', 'UIUC, Uillinois', 'UKansas', 'UMichigan', 'UWashington',
                                        'UWisconsin', 'WashingtonU', 'Yale', 'UT'))

peersMarSci <- filter(Rent, Peers %in% c('Woods Hole', 'Florida State', 'RSMAS', 'VIMS', 'Oregon State',
                                         'UCSB', 'UWashington', 'UCSD, Scripps', 'UCSC', 'UMaine', 'USF',
                                         'Duke Field', 'Duke Main, UNC', 'UT'))

# Draw boxplots of rent by location
boxplot(rentbd1 ~ Peers, data = Rent,
        col = c(rep('lightsteelblue3',40), rep('darkorange2',1)),
        notch = T,
        xlab = "Location",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments by city\n2015-2019",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

# Draw boxplot of rentbd1 by specific program
boxplot(formula = rentbd1 ~ Peers, data = peersSDS,
        col = c(rep('lightsteelblue3',16), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, \nSatistics & Data Science",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersPhysics,
        col = c(rep('lightsteelblue3',9), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, Physics",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersHDFS,
        col = c(rep('lightsteelblue3',11), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, \n Human Development & Family Science",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersCS,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, \nComputer Science",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersNeuro,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, \nNeuroscience",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersChem,
        col = c(rep('lightsteelblue3',7), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, Chemistry",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersNutriSci,
        col = c(rep('lightsteelblue3',4), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, \nNutritional Science",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersEEBPB,
        col = c(rep('lightsteelblue3',20), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, \n EEB & PB",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd1 ~ Peers, data = peersMarSci,
        col = c(rep('lightsteelblue3',10), rep('darkorange2',1)),
        notch = T,
        xlab = "Competing Institutions",
        ylab = "Monthly Rent ($)",
        main = "Monthly rent for 1 BR apartments, \nMarine Science",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

#Draw boxplot of rentbd0 by specific program
boxplot(formula = rentbd0 ~ Peers, data = peersSDS,
        col = c(rep('lightsteelblue3',16), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent ($)',
        main = 'Monthly rent for studio apartments, \nStatistics & Data Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersPhysics,
        col = c(rep('lightsteelblue3',9), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for studio apartments, Physics',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersHDFS,
        col = c(rep('lightsteelblue3',11), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for studio apartments, \nHuman Development & Family Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersCS,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for studio apartments, \nComputer Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersNeuro,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = "Monthly rent for studio apartments, \nNeuroscience",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersChem,
        col = c(rep('lightsteelblue3',7), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = "Monthly rent for studio apartments, Chemistry",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersNutriSci,
        col = c(rep('lightsteelblue3',4), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for studio apartments, \nNutritional Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersEEBPB,
        col = c(rep('lightsteelblue3',20), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for studio apartments, \n EEB & PB',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd0 ~ Peers, data = peersMarSci,
        col = c(rep('lightsteelblue3',10), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for studio apartments, \nMarine Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

#Draw boxplot for rentbd2 by specific program
boxplot(formula = rentbd2 ~ Peers, data = peersSDS,
        col = c(rep('lightsteelblue3',16), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent ($)',
        main = 'Monthly rent for 2 BR apartments, \nStatistics & Data Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersPhysics,
        col = c(rep('lightsteelblue3',9), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'MonthlyRent',
        main = 'Monthly rent for 2 BR apartments, Physics',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersHDFS,
        col = c(rep('lightsteelblue3',11), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 2 BR apartments, \nHuman Development & Family Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersCS,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 2 BR apartments, \nComputer Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersNeuro,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = "Monthly rent for 2 BR apartments, \nNeuroscience",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersChem,
        col = c(rep('lightsteelblue3',7), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = "Monthly rent for 2 BR apartments, Chemistry",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersNutriSci,
        col = c(rep('lightsteelblue3',4), rep('darkorange2')),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 2 BR apartments, \nNutritional Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersEEBPB,
        col = c(rep('lightsteelblue3',20), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 2 BR apartments, \n EEB & PB',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd2 ~ Peers, data = peersMarSci,
        col = c(rep('lightsteelblue3',10), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 2 BR apartments, \nMarine Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

#Draw boxplot for rentbd3 by specific program
boxplot(formula = rentbd3 ~ Peers, data = peersSDS,
        col = c(rep('lightsteelblue3',16), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent ($)',
        main = 'Monthly rent for 3 BR apartments, \nStatistics & Data Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd3 ~ Peers, data = peersPhysics,
        col = c(rep('lightsteelblue3',9), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 3 BR apartments, Physics',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd3 ~ Peers, data = peersHDFS,
        col = c(rep('lightsteelblue3',11), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 3 BR apartments, \nHuman Development & Family Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd3 ~ Peers, data = peersCS,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 3 BR apartments, \nComputer Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd3 ~ Peers, data = peersNeuro,
        col = c(rep('lightsteelblue3',5), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = "Monthly rent for 3 BR apartments, \nNeuroscience",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd3 ~ Peers, data = peersChem,
        col = c(rep('lightsteelblue3',7), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = "Monthly rent for 3 BR apartments, Chemistry",
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd3 ~ Peers, data = peersNutriSci,
        col = c(rep('lightsteelblue3',4), rep('darkorange2')),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 3 BR apartments, \nNutritional Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7)

boxplot(formula = rentbd3 ~ Peers, data = peersEEBPB,
        col = c(rep('lightsteelblue3',20), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 3 BR apartments, \n EEB & PB',
        las = 2,
        cex.axis=.5,
        cex.lab = .7,
        horizontal = T)

boxplot(formula = rentbd3 ~ Peers, data = peersMarSci,
        col = c(rep('lightsteelblue3',10), rep('darkorange2',1)),
        notch = T,
        xlab = 'Competing Institutions',
        ylab = 'Monthly Rent',
        main = 'Monthly rent for 3 BR apartments, \nMarine Science',
        las = 2,
        cex.axis=.5,
        cex.lab = .7,
        horizontal=T)

library(ggplot2)

levels(peersChem$effect) <- gsub(" ", "\n", 
levels(peersChem$effect))
ggplot(peersChem,
       aes(x = Peers,
           y = rentbd0)) +
        geom_boxplot()+ 
        coord_flip()

levels(peersEEBPB$effect) <- gsub(" ", "\n", 
levels(peersEEBPB$effect))
ggplot(peersEEBPB,
       aes(x = Peers,
           y = rentbd0)) +
        geom_boxplot()+ 
        coord_flip()+
        ggtitle('Monthly rent for 3 BR apartments, \n EEB & PB')+
        labs(y="Monthly Rent", x = "Competing Institutions")







