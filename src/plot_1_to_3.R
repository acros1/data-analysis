# R Script
# -----------------Librairies-----------------

library("ggplot2")
library("dplyr")
library("ggrepel")
#library("babynames")
library("viridis")
library("hrbrthemes")
library("plotly")

# -----------------Dataset import-----------------
# Path to dataset is creating by this trick
scriptPath = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/../dataset")
# And this trick to delete a parasite space
scriptPath = gsub ("src ", "src", scriptPath)
# homicidesF contains "Unknown" values
homicidesF = read.csv(file = file.path(scriptPath, "homicide-reports-1980_2014.csv"), sep = ",",
                     na.strings = c("NA", "Unknown"))
# homicides is free of "Unknown" values (can be useful for some analysis)
homicides <- droplevels(na.omit(homicidesF))

# Dataset info
rowNb = nrow(homicides)
columnNb = ncol(homicides)
sprintf("There are %i columns and %i rows in this database",columnNb,rowNb)

# -----------------Plot 1 - Line chart-----------------
# First, we look at the number of homicides per year
homicidesPerYear <- homicidesF %>% group_by(Year) %>% summarise(nb = n())
# Plot
ggplot() + 
  ggtitle("Number of Homicide Events Per Year") +
  geom_line(data = homicidesPerYear, aes(x = Year, y = nb), color = "red") +
  xlab('Year') +
  ylab('Number of Homicides')

# What the fuck happened in the 90s

# -----------------Plot 2 - Spaghetti chart-----------------
# Spaghetti chart of homicides per states per year
homicidesPerState <- homicidesF %>% group_by(Year, State) %>% summarise(nb = n())
# Plot
ggplot(data = homicidesPerState, aes(x = Year, y = nb), color=State) +
  geom_line() +
  facet_wrap("State")

# What the fuck California

# -----------------Plot 3 - Stacked area chart-----------------
# Quantity of male and female perpetrators
perPerSex <- homicidesF %>% group_by(Year, Perpetrator.Sex) %>% summarise(nb = n())
# Plot
ggplot(perPerSex, aes(x=Year, y=nb, fill=Perpetrator.Sex)) + 
  geom_area(alpha=0.6 , size=1, colour="black")

# -----------------Plot 4 - Stacked area chart-----------------
# Quantity of male and female victims
vicPerSex <- homicidesF %>% group_by(Year, Victim.Sex) %>% summarise(nb = n())
# Plot
ggplot(vicPerSex, aes(x=Year, y=nb, fill=Victim.Sex)) + 
  geom_area(alpha=0.6 , size=1, colour="black")

# -----------------Plot 5 - Stacked area chart-----------------
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
