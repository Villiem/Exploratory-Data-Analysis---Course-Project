library(tidyverse)
pmed <- readRDS('data/summarySCC_PM25.rds')
scc <- readRDS('data/Source_Classification_Code.rds')

coal_related  <- grepl("coal", scc$Short.Name, ignore.case=TRUE)|
  grepl("coal", scc$EI.Sector,ignore.case = T)

coal_related <- scc[coal_related, ]

annual_coal_related2 <- pmed %>%
  filter(SCC %in% coal_related$SCC) %>%
  group_by(year) %>%
  summarise(Emissions = sum(as.numeric(as.character(Emissions)))) %>% 
  mutate(year = as.factor(year))

g<-ggplot(annual_coal_related2, aes(x=year,y=Emissions))+ggtitle("Total PM2.5 emission from coal combustion-related sources (tons)\n in the United States")
g+geom_bar(stat="identity") + theme_linedraw()

ggsave('plot4.png')