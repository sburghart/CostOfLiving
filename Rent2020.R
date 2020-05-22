#By: Tracy Morales
# Load the libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Create data frame "Rent" from CSV file w/headers
Rent <- read_csv("Rent.20200221.csv")

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

# Draw violin plots of rent by location
ggplot(Rent,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin()+ 
        coord_flip()+
        ggtitle('Monthly rent for 1 BR apartments by city')+
        labs(y="Monthly Rent ($)", x ="Location")

# Draw violin plot of rentbd0 by specific program
ggplot(peersSDS,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nStatistics & Data Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersPhysics,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nPhysics')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersHDFS,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nHuman Development & Family Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersCS,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nComputer Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNeuro,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nNeuroscience')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersChem,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nChemistry')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNutriSci,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nNutritional Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersEEBPB,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nEcology, Evolution and Behavior & Plant Biology')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersMarSci,
       aes(x = Peers,
           y = rentbd0)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 0 BR Apartments, \nMarine Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()


#Draw violin plot of rentbd1 by specific program
ggplot(peersSDS,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nStatistics & Data Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersPhysics,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nPhysics')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersHDFS,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nHuman Development & Family Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersCS,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nComputer Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNeuro,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nNeuroscience')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersChem,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nChemistry')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNutriSci,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nNutritional Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersEEBPB,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nEcology, Evolution and Behavior & Plant Biology')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersMarSci,
       aes(x = Peers,
           y = rentbd1)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 1 BR Apartments, \nMarine Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

#Draw violin plot for rentbd2 by specific program
ggplot(peersSDS,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nStatistics & Data Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersPhysics,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nPhysics')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersHDFS,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nHuman Development & Family Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersCS,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nComputer Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNeuro,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nNeuroscience')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersChem,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nChemistry')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNutriSci,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nNutritional Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersEEBPB,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nEcology, Evolution and Behavior & Plant Biology')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersMarSci,
       aes(x = Peers,
           y = rentbd2)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 2 BR Apartments, \nMarine Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

#Draw violin plot for rentbd3 by specific program
ggplot(peersSDS,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nStatistics & Data Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersPhysics,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nPhysics')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersHDFS,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nHuman Development & Family Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersCS,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nComputer Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNeuro,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nNeuroscience')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersChem,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nChemistry')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersNutriSci,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nNutritional Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersEEBPB,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nEcology, Evolution and Behavior & Plant Biology')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()

ggplot(peersMarSci,
       aes(x = Peers,
           y = rentbd3)) +
        geom_violin(fill = 'lightsteelblue3')+ 
        coord_flip()+
        ggtitle('Monthly Rent for 3 BR Apartments, \nMarine Science')+
        labs(y="Monthly Rent ($)", x ="Competing Institutions")+
        theme_bw()









