if(!file.exists("./data")){
  dir.create("./data")
}
# the data for the project:
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

# reading
library(tidyverse)
pmed <- readRDS('data/summarySCC_PM25.rds')
scc <- readRDS('data/Source_Classification_Code.rds')
# head(pmed)
# str(pmed)
# str(scc)
# head(scc)

## Question 1
## Have total emissions from PM2.5 decreased 
## in the United States from 1999 to 2008? 
## Using the base plotting system, 
## make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

total <- pmed %>%
  group_by(year) %>% 
  summarise(total_emisions = sum(Emissions))

barplot(total$total_emisions, names.arg = total$year,
        xlab = "Year",
        ylab = 'PM2.5 Emissions (tons)',
        main = 'Total PM2.5 Emissions from all sources')

## Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510"\color{red}{\verb|fips == "24510"|}fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.1

total_maryland <- pmed %>% 
  filter(fips == '24510') %>% 
  group_by(year) %>% 
  summarise(total_emisions = sum(Emissions))

barplot(total_maryland$total_emisions, names.arg = total_maryland$year,
        xlab = "Year",
        ylab = 'PM2.5 Emissions (tons)',
        main = 'Total PM2.5 Emissions of Maryland from all sources')

g <- pmed %>% 
  filter(fips == '24510')%>% 
  ggplot(aes(x = year, y = Emissions, fill=type))
g+geom_bar(stat="identity")+
  facet_grid(.~type)+
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))+
  guides(fill=FALSE)

## 4

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


## Question 5
baltimore_E <- pmed %>% 
filter(fips=="24510" & type=="ON-ROAD") %>%
  group_by(year) %>%
  summarise(Emissions = sum(as.numeric(as.character(Emissions)))) %>% 
  mutate(year = as.factor(year))

g <-ggplot(baltimore_E, aes(x=year,y=Emissions))+ggtitle("Total PM2.5 emission from motor vehicle sources (tons)\n in Baltimore")
g+geom_bar(stat="identity") + theme_linedraw()


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
