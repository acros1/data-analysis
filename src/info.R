# R Script

library("ggplot2")
library("dplyr")

# Path to dataset is creating by this trick
scriptPath = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/../dataset")
# And this trick to delete a parasite space
scriptPath = gsub ("src ", "src", scriptPath)
# homicides
homicides = read.csv(file = file.path(scriptPath, "homicide-reports-1980_2014.csv"), sep = ",")

rowNb = nrow(homicides)
columnNb = ncol(homicides)
sprintf("There are %i columns and %i rows in this database",columnNb,rowNb)

#first, we look at the number of homicides per year
gp_perYear <- homicides %>% group_by(Year)
#gp_perYear <- gp_perYear %>% group_by(Record.ID)
plotPerYear <- gp_perYear %>% summarise(nb = n())

ggplot() + 
  ggtitle("Number of Homicide Events Per Year") +
  geom_point(data = plotPerYear, aes(x = Year, y = nb), color = "red") +
  xlab('Year') +
  ylab('Number of Homicides')

#what the fuck happened in the 90s

plotPerState <- homicides %>% group_by(Year, State) %>% summarise(nb = n())
ggplot(data = plotPerState,aes(x = Year, y = nb), color=State) +
  geom_line() +
  facet_wrap("State")

#what the fuck california