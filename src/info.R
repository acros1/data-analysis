# R Script

library("ggplot2")
library("dplyr")

# Path to dataset is creating by this trick
scriptPath = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/../dataset")
# And this trick to delete a parasite space
scriptPath = gsub ("src ", "src", scriptPath)
# Homicides
homicides = read.csv(file = file.path(scriptPath, "homicide-reports-1980_2014.csv"), sep = ",")

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

# Crime solved
crimeSolved <- homicides %>% group_by(Crime.Solved) %>% summarise(nb = n())
crimeSolvedPourcent <- crimeSolved$nb / nrow(homicides) * 100
ggplot(data = crimeSolved, aes(x = "", y = crimeSolvedPourcent, fill = Crime.Solved)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = 1/crimeSolvedPourcent + c(0, cumsum(crimeSolvedPourcent)[-length(crimeSolvedPourcent)]), label = scales::percent(crimeSolvedPourcent/100)), size=5, hjust = 2)


