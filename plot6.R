library(tidyverse)
pmed <- readRDS('data/summarySCC_PM25.rds')
scc <- readRDS('data/Source_Classification_Code.rds')

## Question 6
bal_LA <- pmed %>% 
  filter(type=="ON-ROAD" & fips %in% c('24510', '06037')) %>% 
  group_by(year, fips) %>% 
  summarise(total_e = sum(Emissions))
bal_LA$fips2 <- factor(bal_LA$fips, labels = c("Los Angeles", "Baltimore"))
g <- ggplot(bal_LA, aes(x = year, y = total_e))
g + geom_bar(stat = 'identity') + facet_grid(~fips2) +
  coord_flip() + labs(
    title =  "Total PM2.5 emission from motor vehicle sources (tons)",
    y = "Total Emissions") 
ggsave('plot6.png')
