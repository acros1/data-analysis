# R Script

library("ggplot2")
library("dplyr")

#the dataset, called "database", is imported through the "import dataset" feature
#there is not point in using the read() function as the dataset needs to be extracted from an archive so the
#path would need to be changed on every machine

rowNb = nrow(database)
columnNb = ncol(database)
sprintf("There are %i columns and %i rows in this database",columnNb,rowNb)

#first, we look at the number of homicides per year
gp_perYear <- database %>% group_by(Year)
#gp_perYear <- gp_perYear %>% group_by(Record.ID)
plotPerYear <- gp_perYear %>% summarise(nb = n())

ggplot() + 
  ggtitle("Number of Homicide Events Per Year") +
  geom_point(data = plotPerYear, aes(x = Year, y = nb), color = "red") +
  xlab('Year') +
  ylab('Number of Homicides')

#what the fuck happened in the 90s
