# R Script

library("ggplot2")
library("dplyr")
library(ggrepel)

# Path to dataset is creating by this trick
scriptPath = paste(dirname(rstudioapi::getSourceEditorContext()$path), "/../dataset")
# And this trick to delete a parasite space
scriptPath = gsub ("src ", "src", scriptPath)
# Homicides
homicides = read.csv(file = file.path(scriptPath, "homicide-reports-1980_2014.csv"), sep = ",")
homicidesFiltered <- filter(homicides, Relationship != "Unknown")

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

#We also look at the stats per individual state
homicidesPerState <- homicides %>% group_by(Year, State) %>% summarise(nb = n())
ggplot(data = homicidesPerState, aes(x = Year, y = nb), color=State) +
  geom_line() +
  facet_wrap("State")

# Crime solved
crimeSolved <- homicides %>% group_by(Crime.Solved) %>% summarise(nb = n())
crimeSolvedPourcent <- crimeSolved$nb / nrow(homicides) * 100
ggplot(data = crimeSolved, aes(x = "", y = crimeSolvedPourcent, fill = Crime.Solved)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = 1/crimeSolvedPourcent + c(0, cumsum(crimeSolvedPourcent)[-length(crimeSolvedPourcent)]), label = scales::percent(crimeSolvedPourcent/100)), size=5, hjust = 2)

#relationship per year
relationshipPerYear <- homicides %>% group_by(Year, Relationship) %>% summarise(nb = n())
#put the total amount of homicides per year on each row for percentage calculation
for (i in 1:nrow(relationshipPerYear)) {
  relationshipPerYear$total[i] <- filter(homicidesPerYear, Year == relationshipPerYear$Year[i])$nb
}
#calculate the percentage
relationshipPerYear$percent <- relationshipPerYear$nb / relationshipPerYear$total * 100
#plot a coloured bar graph
ggplot(data=relationshipPerYear, aes(x=Year, y=percent, fill=Relationship)) +
  geom_bar(stat="identity")

#relationships but removing unknown values
relationshipPerYearFiltered <- homicides %>% group_by(Year, Relationship) %>% summarise(nb = n()) %>% filter(Relationship != "Unknown")
#homicides but removing unknown values
homicidesPerYearFiltered <- homicidesFiltered %>% group_by(Year) %>% summarise(nb = n())
#put the total amount of homicides per year on each row for percentage calculation
for (i in 1:nrow(relationshipPerYearFiltered)) {
  relationshipPerYearFiltered$total[i] <- filter(homicidesPerYearFiltered, Year == relationshipPerYearFiltered$Year[i])$nb
}
relationshipPerYearFiltered$percent <- relationshipPerYearFiltered$nb / relationshipPerYearFiltered$total * 100
ggplot(data=relationshipPerYearFiltered, aes(x=Year, y=percent, fill=Relationship)) +
  geom_area(stat="identity")

#sort by categories
relationshipPerYearCategories = homicidesFiltered
#sorting all relationship into 9 categories for more clarity:
#acquaintance, significant other, ex significant other, family, indirect family, professional, stranger, unkown
for (i in 1:nrow(relationshipPerYearCategories)){
  relationshipPerYearCategories$Category[i] <- switch (relationshipPerYearCategories$Relationship[i],
    "Acquaintance" =  "acquaintance",
    "Boyfriend" =  "significant other",
    "Boyfriend/Girlfriend" =  "significant other",
    "Brother" =  "family",
    "Common-Law Husband" =  "significant other",
    "Common-Law Wife" =  "significant other",
    "Daughter" =  "family",
    "Employee" =  "professional",
    "Employer" =  "professional",
    "Ex-Husband" = "ex significant other",
    "Ex-Wife" =  "ex significant other",
    "Family" =  "family",
    "Father" =  "family",
    "Friend" =  "acquaintance",
    "Girlfriend" =  "significant other",
    "Husband" =  "significant other",
    "In-Law" =  "indirect family",
    "Mother" =  "family",
    "Neighbor" =  "acquaintance",
    "Sister" =  "family",
    "Son"=  "family",
    "Stepdaughter" = "indirect family",
    "Stepfather" =  "indirect family",
    "Stepmother" =  "indirect family",
    "Stepson" =  "indirect family",
    "Stranger" =  "stranger",
    "Wife" =  "significant other",
    "Unknown" =  "unknown"
  )
}
#formating data to get a stacked area graph and percentages
relationshipPerYearCategoriesPercent <- relationshipPerYearCategories %>% group_by(Year, Category) %>% summarize(nb=n())
for (i in 1:nrow(relationshipPerYearCategoriesPercent)) {
  relationshipPerYearCategoriesPercent$total[i] <- filter(homicidesPerYear, Year == relationshipPerYearCategoriesPercent$Year[i])$nb
}
relationshipPerYearCategoriesPercent$percent <- relationshipPerYearCategoriesPercent$nb / relationshipPerYearCategoriesPercent$total * 100
ggplot(data=relationshipPerYearCategoriesPercent, aes(x=Year, y=percent, fill=Category)) +
  geom_area(stat="identity")

#let's do the same without unknowns
relationshipPerYearFilteredCategories = homicidesFiltered
for (i in 1:nrow(relationshipPerYearFilteredCategories)){
  relationshipPerYearFilteredCategories$Category[i] <- switch (relationshipPerYearFilteredCategories$Relationship[i],
                                                       "Acquaintance" =  "acquaintance",
                                                       "Boyfriend" =  "significant other",
                                                       "Boyfriend/Girlfriend" =  "significant other",
                                                       "Brother" =  "family",
                                                       "Common-Law Husband" =  "significant other",
                                                       "Common-Law Wife" =  "significant other",
                                                       "Daughter" =  "family",
                                                       "Employee" =  "professional",
                                                       "Employer" =  "professional",
                                                       "Ex-Husband" = "ex significant other",
                                                       "Ex-Wife" =  "ex significant other",
                                                       "Family" =  "family",
                                                       "Father" =  "family",
                                                       "Friend" =  "acquaintance",
                                                       "Girlfriend" =  "significant other",
                                                       "Husband" =  "significant other",
                                                       "In-Law" =  "indirect family",
                                                       "Mother" =  "family",
                                                       "Neighbor" =  "acquaintance",
                                                       "Sister" =  "family",
                                                       "Son"=  "family",
                                                       "Stepdaughter" = "indirect family",
                                                       "Stepfather" =  "indirect family",
                                                       "Stepmother" =  "indirect family",
                                                       "Stepson" =  "indirect family",
                                                       "Stranger" =  "stranger",
                                                       "Wife" =  "significant other",
                                                       "Unknown" =  "unknown"
  )
}

relationshipPerYearFilteredCategoriesPercent <- relationshipPerYearFilteredCategories %>% group_by(Year, Category) %>% summarize(nb=n())
for (i in 1:nrow(relationshipPerYearFilteredCategoriesPercent)) {
  relationshipPerYearFilteredCategoriesPercent$total[i] <- filter(homicidesPerYearFiltered, Year == relationshipPerYearFilteredCategoriesPercent$Year[i])$nb
}

relationshipPerYearFilteredCategoriesPercent$percent <- relationshipPerYearFilteredCategoriesPercent$nb / relationshipPerYearFilteredCategoriesPercent$total * 100
ggplot(data=relationshipPerYearFilteredCategoriesPercent, aes(x=Year, y=percent, fill=Category)) +
  geom_area(stat="identity")

#now let's look at age difference
agedifference = homicides
agedifference$diff <- agedifference$Perpetrator.Age - agedifference$Victim.Age
agedifference$absdiff <- abs(agedifference$Perpetrator.Age - agedifference$Victim.Age)

agedifferenceplot <- agedifference %>% group_by(Year) %>% summarize(averagediff = mean(diff), nb = n())

for (i in 1:nrow(agedifference)) {
  agedifference$total[i] <- filter(homicidesPerYear, Year == agedifference$Year[i])$nb
}
agedifference$yearfactor <-as.factor(agedifference$Year)
#have to fix the values as there are a few outliers at unreasonable age differences
agedifferencefix <- filter(agedifference, diff >(-80))

ggplot(agedifferencefix, aes(x=yearfactor,y=diff)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Age difference between perpetrator and victim")
