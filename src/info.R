# R Script
# -----------------Librairies-----------------

library("ggplot2")
library("dplyr")
library("ggrepel")
library("babynames")
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

# -----------------Plot 6 and 7 - Stacked area chart-----------------
# Relationship per year
relationshipPerYear <- homicides %>% group_by(Year, Relationship) %>% summarise(nb = n())
#put the total amount of homicides per year on each row for percentage calculation
for (i in 1:nrow(relationshipPerYear)) {
  relationshipPerYear$total[i] <- filter(homicidesPerYear, Year == relationshipPerYear$Year[i])$nb
}
# Calculate the percentage
relationshipPerYear$percent <- relationshipPerYear$nb / relationshipPerYear$total * 100
# Plot a coloured bar graph
ggplot(data=relationshipPerYear, aes(x=Year, y=percent, fill=Relationship)) +
  geom_bar(stat="identity")

# Relationships but removing unknown values
relationshipPerYearFiltered <- homicides %>% group_by(Year, Relationship) %>% summarise(nb = n()) %>% filter(Relationship != "Unknown")
# Homicides but removing unknown values
homicidesPerYearFiltered <- homicidesFiltered %>% group_by(Year) %>% summarise(nb = n())
# Put the total amount of homicides per year on each row for percentage calculation
for (i in 1:nrow(relationshipPerYearFiltered)) {
  relationshipPerYearFiltered$total[i] <- filter(homicidesPerYearFiltered, Year == relationshipPerYearFiltered$Year[i])$nb
}
relationshipPerYearFiltered$percent <- relationshipPerYearFiltered$nb / relationshipPerYearFiltered$total * 100
ggplot(data=relationshipPerYearFiltered, aes(x=Year, y=percent, fill=Relationship)) +
  geom_area(stat="identity")

# Filtered relationships but removing very low values
relationshipFilteredNoLowValues <- relationshipPerYearFiltered %>% filter(percent>1.5)
ggplot(data=relationshipFilteredNoLowValues, aes(x=Year, y=percent, fill=Relationship)) +
  geom_bar(stat="identity")

# Sort by categories
relationshipPerYearCategories = homicidesFiltered
# Sorting all relationship into 9 categories for more clarity:
# Acquaintance, significant other, ex significant other, family, indirect family, professional, stranger, unkown
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
# Formating data to get a percentage bar graph
relationshipPerYearCategoriesPercent <- relationshipPerYearCategories %>% group_by(Year, Category) %>% summarize(nb=n())
for (i in 1:nrow(relationshipPerYearCategoriesPercent)) {
  relationshipPerYearCategoriesPercent$total[i] <- filter(homicidesPerYear, Year == relationshipPerYearCategoriesPercent$Year[i])$nb
}

relationshipPerYearCategoriesPercent$percent <- relationshipPerYearCategoriesPercent$nb / relationshipPerYearCategoriesPercent$total * 100
ggplot(data=relationshipPerYearCategoriesPercent, aes(x=Year, y=percent, fill=Category)) +
  geom_area(stat="identity")

# Let's do the same without unknowns
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

# -----------------Plot 8 - Box chart-----------------
# Now let's look at age difference
agedifference = homicides
agedifference$diff <- agedifference$Perpetrator.Age - agedifference$Victim.Age
agedifference$absdiff <- abs(agedifference$Perpetrator.Age - agedifference$Victim.Age)

agedifferenceplot <- agedifference %>% group_by(Year) %>% summarize(averagediff = mean(diff), nb = n())

for (i in 1:nrow(agedifference)) {
  agedifference$total[i] <- filter(homicidesPerYear, Year == agedifference$Year[i])$nb
}
agedifference$yearfactor <-as.factor(agedifference$Year)
# Have to fix the values as there are a few outliers at unreasonable age differences
agedifferencefix <- filter(agedifference, diff >(-80))

ggplot(agedifferencefix, aes(x=yearfactor,y=diff)) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Age difference between perpetrator and victim")
