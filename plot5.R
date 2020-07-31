library(tidyverse)
pmed <- readRDS('data/summarySCC_PM25.rds')
scc <- readRDS('data/Source_Classification_Code.rds')

## Question 5
baltimore_E <- pmed %>% 
  filter(fips=="24510" & type=="ON-ROAD") %>%
  group_by(year) %>%
  summarise(Emissions = sum(as.numeric(as.character(Emissions)))) %>% 
  mutate(year = as.factor(year))

g <-ggplot(baltimore_E, aes(x=year,y=Emissions))+ggtitle("Total PM2.5 emission from motor vehicle sources (tons)\n in Baltimore")
g+geom_bar(stat="identity") + theme_linedraw()
ggsave('plot5.png')