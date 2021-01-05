# R Script

library("ggplot2")
library("dplyr")

# Path to dataset is creating by this trick
scriptPath = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/../dataset")
# And this trick to delete a parasite space
scriptPath = gsub ("src ", "src", scriptPath)
# Homicides
homicidesF = read.csv(file = file.path(scriptPath, "homicide-reports-1980_2014.csv"), sep = ",",
                     na.strings = c("NA", "Unknown"))
homicides <- droplevels(na.omit(homicidesF))

rowNb = nrow(homicides)
columnNb = ncol(homicides)
sprintf("There are %i columns and %i rows in this database",columnNb,rowNb)

# First, we look at the number of homicides per year
homicidesPerYear <- homicides %>% group_by(Year) %>% summarise(nb = n())

ggplot() + 
  ggtitle("Number of Homicide Events Per Year") +
  geom_line(data = homicidesPerYear, aes(x = Year, y = nb), color = "red") +
  xlab('Year') +
  ylab('Number of Homicides')

# What the fuck happened in the 90s

homicidesPerState <- homicides %>% group_by(Year, State) %>% summarise(nb = n())
ggplot(data = homicidesPerState, aes(x = Year, y = nb), color=State) +
  geom_line() +
  facet_wrap("State")

# What the fuck California
library(ggrepel)

# Men/women perpetrator ratio
perPerSex <- homicides %>% group_by(Perpetrator.Sex) %>% summarise(nb = n())
perPerSexPourcent <- perPerSex$nb / nrow(homicides) * 100
ggplot(data = perPerSex, aes(x = "", y = perPerSexPourcent, fill = Perpetrator.Sex)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_label_repel(aes(label = perPerSexPourcent), size=5, show.legend = F, nudge_x = 0.5) +
  guides(fill = guide_legend(title = "Sex"))

# Men/women perpetrator ratio
library(dplyr)
perPerSex <- homicidesF %>% group_by(Year, Perpetrator.Sex) %>% summarise(nb = n())
# Plot
ggplot(perPerSex, aes(x=Year, y=nb, fill=Perpetrator.Sex)) + 
  geom_area(alpha=0.6 , size=1, colour="black")

# Men/women victims ratio
library(dplyr)
vicPerSex <- homicidesF %>% group_by(Year, Victim.Sex) %>% summarise(nb = n())
# Plot
ggplot(vicPerSex, aes(x=Year, y=nb, fill=Victim.Sex)) + 
  geom_area(alpha=0.6 , size=1, colour="black")

# Crime solved
crimeSolved <- homicides %>% group_by(Crime.Solved) %>% summarise(nb = n())
crimeSolvedPourcent <- crimeSolved$nb / nrow(homicides) * 100
ggplot(data = crimeSolved, aes(x = "", y = crimeSolvedPourcent, fill = Crime.Solved)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = 1/crimeSolvedPourcent + c(0, cumsum(crimeSolvedPourcent)[-length(crimeSolvedPourcent)]), label = scales::percent(crimeSolvedPourcent/100)), size=5, hjust = 2)

# Perpetrators ethnicity
ethnicityPerYear <- homicides %>% group_by(Year, Perpetrator.Ethnicity) %>% summarise(nb = n())
ggplot(data=ethnicityPerYear, aes(x=Year, y=nb, fill=Perpetrator.Ethnicity)) +
  geom_bar(stat="identity")

# Victims ethnicity
victimEthnicityPerYear <- homicides %>% group_by(Year, Victim.Ethnicity) %>% summarise(nb = n())
ggplot(data=victimEthnicityPerYear, aes(x=Year, y=nb, fill=Victim.Ethnicity)) +
  geom_bar(stat="identity")

# Perpetrators "race"
racePerYear <- homicides %>% group_by(Year, Perpetrator.Race) %>% summarise(nb = n())
ggplot(data=racePerYear, aes(x=Year, y=nb, fill=Perpetrator.Race)) +
  geom_bar(stat="identity")


library(babynames)
library(viridis)
library(hrbrthemes)
library(plotly)

# Perpetrator ethnicity comparison for each year
data <- homicides %>% group_by(Year, Perpetrator.Ethnicity) %>% summarise(nb = n())

# Plot
p <- data %>% 
  ggplot( aes(x=Year, y=nb, fill=Perpetrator.Ethnicity, text=Perpetrator.Ethnicity)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Perpetrator ethnicity per year") +
  theme_ipsum() +
  theme(legend.position="top")

# Turn it interactive
p <- ggplotly(p, tooltip="text")
p

# Perpetrator ethnicity comparison for each year
data1 <- homicidesF %>% group_by(Year, Perpetrator.Race) %>% summarise(nb = n())

# Plot
p <- data1 %>% 
  ggplot( aes(x=Year, y=nb, fill=Perpetrator.Race, text=Perpetrator.Race)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Perpetrator race per year") +
  theme_ipsum() +
  theme(legend.position="top")

# Turn it interactive
p <- ggplotly(p, tooltip="text")
p
