library(tidyverse)
pmed <- readRDS('data/summarySCC_PM25.rds')
scc <- readRDS('data/Source_Classification_Code.rds')

## Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510"\color{red}{\verb|fips == "24510"|}fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.1

total_maryland <- pmed %>% 
  filter(fips == '24510') %>% 
  group_by(year) %>% 
  summarise(total_emisions = sum(Emissions))
png('plot2.png',480,480)
barplot(total_maryland$total_emisions, names.arg = total_maryland$year,
        xlab = "Year",
        ylab = 'PM2.5 Emissions (tons)',
        main = 'Total PM2.5 Emissions of Maryland from all sources')
dev.off()