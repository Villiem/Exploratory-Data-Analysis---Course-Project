library(tidyverse)
pmed <- readRDS('data/summarySCC_PM25.rds')
scc <- readRDS('data/Source_Classification_Code.rds')

## Question 1
## Have total emissions from PM2.5 decreased 
## in the United States from 1999 to 2008? 
## Using the base plotting system, 
## make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

total <- pmed %>%
  group_by(year) %>% 
  summarise(total_emisions = sum(Emissions))

png('plot1.png',480,480)
barplot(total$total_emisions, names.arg = total$year,
        xlab = "Year",
        ylab = 'PM2.5 Emissions (tons)',
        main = 'Total PM2.5 Emissions from all sources')
dev.off()