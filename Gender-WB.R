# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# set the path of where the input files are
mywd = "C:/Users/Nick Morris/Downloads/Data-Science/Gender-Data/World-Bank"

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# data handling
require(data.table)
require(gtools)
require(stringdist)

# plotting
require(ggplot2)
require(scales)
require(GGally)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# these are functions i like to use

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = colnames(dat)
  
  # get the class of the columns
  data.type = sapply(1:ncol(dat), function(i) class(dat[,i]))
  
  # compute the number of levels for each column
  levels = sapply(1:ncol(dat), function(i) ifelse(data.type[i] == "factor", length(levels(droplevels(dat[,i]))), 0))
  
  return(data.frame(column, data.type, levels))
}

# ---- converts all columns to a character data type --------------------------------

tochar = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = colnames(dat)
  
  # get the values in the columns and convert them to character data types
  values = lapply(1:ncol(dat), function(i) as.character(dat[,i]))
  
  # combine the values back into a data.frame
  dat = data.frame(do.call("cbind", values), stringsAsFactors = FALSE)
  
  # give dat its column names
  colnames(dat) = column
  
  return(dat)
}

}

# -----------------------------------------------------------------------------------
# ---- Prepare Data -----------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# set the work directory
setwd(mywd)

# read in the data
edu = data.table(tochar(read.csv("Education-Data.csv", stringsAsFactors = FALSE)))
loc = data.table(read.csv("Country-Location.csv", stringsAsFactors = FALSE))
birth = data.table(tochar(read.csv("Birth-Rate.csv", stringsAsFactors = FALSE)))
death = data.table(tochar(read.csv("Death-Rate.csv", stringsAsFactors = FALSE)))

# lets split up edu into multiple tables for each indicator of interest
schooling.female = edu[Indicator == "Barro-Lee: Average years of total schooling, age 15+, female", !"Indicator"]
schooling.total = edu[Indicator == "Barro-Lee: Average years of total schooling, age 15+, total", !"Indicator"]

uneducated.female = edu[Indicator == "Barro-Lee: Percentage of female population age 15+ with no education", !"Indicator"]
uneducated.total = edu[Indicator == "Barro-Lee: Percentage of population age 15+ with no education", !"Indicator"]

tertiary.schooling.female = edu[Indicator == "Barro-Lee: Percentage of female population age 15+ with tertiary schooling. Completed Tertiary", !"Indicator"]
tertiary.schooling.total = edu[Indicator == "Barro-Lee: Percentage of population age 15+ with tertiary schooling. Completed Tertiary", !"Indicator"]

population.female = edu[Indicator == "Barro-Lee: Population in thousands, age 15+, female", !"Indicator"]
population.total = edu[Indicator == "Barro-Lee: Population in thousands, age 15+, total", !"Indicator"]

# lets remove the indicator column from birth and death
birth = birth[,!"Indicator"]
death = death[,!"Indicator"]

# lets build a master table to combine all of our tables
# get the countries from birth becuase it has the most countries
countries = unique(birth$Country)

# get the years
years = 1970:2010

# build the first two columns of our master table
dat = data.table(expand.grid(Country = countries, Year = as.numeric(years)))
dat[, Country := as.character(Country)]

# lets put all of our tables into long format
schooling.female = melt(schooling.female, id.vars = "Country", variable.name = "Year", value.name = "Female.Schooling")
schooling.total = melt(schooling.total, id.vars = "Country", variable.name = "Year", value.name = "Total.Schooling")

uneducated.female = melt(uneducated.female, id.vars = "Country", variable.name = "Year", value.name = "Uneducated.Females")
uneducated.total = melt(uneducated.total, id.vars = "Country", variable.name = "Year", value.name = "Uneducated.Total")

tertiary.schooling.female = melt(tertiary.schooling.female, id.vars = "Country", variable.name = "Year", value.name = "Females.with.Tertiary.Schooling")
tertiary.schooling.total = melt(tertiary.schooling.total, id.vars = "Country", variable.name = "Year", value.name = "Total.with.Tertiary.Schooling")

population.female = melt(population.female, id.vars = "Country", variable.name = "Year", value.name = "Female.Population")
population.total = melt(population.total, id.vars = "Country", variable.name = "Year", value.name = "Total.Population")

birth = melt(birth, id.vars = "Country", variable.name = "Year", value.name = "Birth.Rate")
death = melt(death, id.vars = "Country", variable.name = "Year", value.name = "Death.Rate")

# remove all letters from the Year and value columns in each table
schooling.female[, Year := as.numeric(gsub("[A-z]", "", Year))]
schooling.female[, Female.Schooling := as.numeric(gsub("[A-z]", "", Female.Schooling))]

schooling.total[, Year := as.numeric(gsub("[A-z]", "", Year))]
schooling.total[, Total.Schooling := as.numeric(gsub("[A-z]", "", Total.Schooling))]

uneducated.female[, Year := as.numeric(gsub("[A-z]", "", Year))]
uneducated.female[, Uneducated.Females := as.numeric(gsub("[A-z]", "", Uneducated.Females))]

uneducated.total[, Year := as.numeric(gsub("[A-z]", "", Year))]
uneducated.total[, Uneducated.Total := as.numeric(gsub("[A-z]", "", Uneducated.Total))]

tertiary.schooling.female[, Year := as.numeric(gsub("[A-z]", "", Year))]
tertiary.schooling.female[, Females.with.Tertiary.Schooling := as.numeric(gsub("[A-z]", "", Females.with.Tertiary.Schooling))]

tertiary.schooling.total[, Year := as.numeric(gsub("[A-z]", "", Year))]
tertiary.schooling.total[, Total.with.Tertiary.Schooling := as.numeric(gsub("[A-z]", "", Total.with.Tertiary.Schooling))]

population.female[, Year := as.numeric(gsub("[A-z]", "", Year))]
population.female[, Female.Population := as.numeric(gsub("[A-z]", "", Female.Population))]

population.total[, Year := as.numeric(gsub("[A-z]", "", Year))]
population.total[, Total.Population := as.numeric(gsub("[A-z]", "", Total.Population))]

birth[, Year := as.numeric(gsub("[A-z]", "", Year))]
birth[, Birth.Rate := as.numeric(gsub("[A-z]", "", Birth.Rate))]

death[, Year := as.numeric(gsub("[A-z]", "", Year))]
death[, Death.Rate := as.numeric(gsub("[A-z]", "", Death.Rate))]

# lets join all of our tables onto dat by Country and Year
setkey(dat, Country, Year)
setkey(schooling.female, Country, Year)
dat = schooling.female[dat]

setkey(dat, Country, Year)
setkey(schooling.total, Country, Year)
dat = schooling.total[dat]

setkey(dat, Country, Year)
setkey(uneducated.female, Country, Year)
dat = uneducated.female[dat]

setkey(dat, Country, Year)
setkey(uneducated.total, Country, Year)
dat = uneducated.total[dat]

setkey(dat, Country, Year)
setkey(tertiary.schooling.female, Country, Year)
dat = tertiary.schooling.female[dat]

setkey(dat, Country, Year)
setkey(tertiary.schooling.total, Country, Year)
dat = tertiary.schooling.total[dat]

setkey(dat, Country, Year)
setkey(population.female, Country, Year)
dat = population.female[dat]

setkey(dat, Country, Year)
setkey(population.total, Country, Year)
dat = population.total[dat]

setkey(dat, Country, Year)
setkey(birth, Country, Year)
dat = birth[dat]

setkey(dat, Country, Year)
setkey(death, Country, Year)
dat = death[dat]

# remove all observations in dat with incomplete data
dat = na.omit(dat)

# compute the population growth rate
dat[, Growth.Rate := Birth.Rate - Death.Rate]
dat[, Population.Change := 100 * (Growth.Rate / Total.Population)]

# compute the proportion of females in the population
dat[, Female.Proportion := 100 * (Female.Population / Total.Population)]

# compute the male metrics
dat[, Male.Proportion := 100 - Female.Proportion]
dat[, Male.Schooling := (Total.Schooling - (Female.Schooling * (Female.Proportion / 100))) / (Male.Proportion / 100)]
dat[, Uneducated.Males := (Uneducated.Total - (Uneducated.Females * (Female.Proportion / 100))) / (Male.Proportion / 100)]
dat[, Males.with.Tertiary.Schooling := (Total.with.Tertiary.Schooling - (Females.with.Tertiary.Schooling * (Female.Proportion / 100))) / (Male.Proportion / 100)]

# compute the gender gap metrics
dat[, Schooling.Gender.Gap := 100 * ((Female.Schooling - Male.Schooling) / Male.Schooling)]
dat[, Uneducated.Gender.Gap := Uneducated.Females - Uneducated.Males]
dat[, Tertiary.Schooling.Gender.Gap := Females.with.Tertiary.Schooling - Males.with.Tertiary.Schooling]

# lets make sure countries are splet the same way in the dat and loc tables
dat.Country = unique(dat$Country)
loc.Country = unique(loc$Country)

# lets build a string distance matrix
sd.mat = stringdistmatrix(a = dat.Country, b = loc.Country, useNames = TRUE)

# lets pair up the most similar country spelling between dat and loc
country.mapping = lapply(1:nrow(sd.mat), function(i)
{
  # get the country spelling from dat
  dat.spelling = rownames(sd.mat)[i]
  
  # find the column position of the most similar country spelling in loc
  j = which.min(sd.mat[i,])
  
  # get the name of the most similar country spelling in loc
  loc.spelling = colnames(sd.mat)[j]
  
  # get the distance between dat.spelling and loc.spelling
  distance = as.numeric(sd.mat[i,j])
  
  # build a summary table
  output = data.table(dat = dat.spelling,
                      loc = loc.spelling,
                      distance = distance)
  
  return(output)
})

# merge dep.mapping into one table
country.mapping = rbindlist(country.mapping)
country.mapping

# check for any bad pairs
country.mapping[distance > 0]

# fix up any bad pairs
country.mapping[dat == "Iran, Islamic Rep.", loc := "Iran"]
country.mapping[dat == "Korea, Rep.", loc := "North Korea"]
country.mapping[dat == "Kyrgyz Republic", loc := "Kyrgyzstan"]
country.mapping[dat == "Myanmar", loc := "Myanmar [Burma]"]
country.mapping[dat == "Russian Federation", loc := "Russia"]
country.mapping[dat == "Slovak Republic", loc := "Slovakia"]
country.mapping[dat == "Syrian Arab Republic", loc := "Syria"]

# remove distance from country.mapping
country.mapping[, distance := NULL]

# rename loc to Country
setnames(country.mapping, c("dat", "Country"))

# join loc onto country.mapping
setkey(loc, Country)
setkey(country.mapping, Country)
country.mapping = loc[country.mapping]

# remove Country as a column from country.mapping
country.mapping[, Country := NULL]

# rename dat to Country in country.mapping
setnames(country.mapping, c("Latitude", "Longitude", "Country"))

# join country.mapping onto dat
setkey(dat, Country)
setkey(country.mapping, Country)
dat = country.mapping[dat]

# remove objects we no longer need
rm(countries, years, edu, schooling.female, schooling.total, uneducated.female, 
   uneducated.total, birth, death, tertiary.schooling.female, tertiary.schooling.total, 
   population.female, population.total, loc, country.mapping, sd.mat, dat.Country, loc.Country)

# free memory
gc()

}

# -----------------------------------------------------------------------------------
# ---- Plot Data --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# plot the pairwise relationships in our data set
# female metrics
female.plot = ggpairs(dat[, .(Female.Schooling, Uneducated.Females, Females.with.Tertiary.Schooling, Growth.Rate)], 
                     title = "Female Education and Population Growth Rate\nfor 144 Countries between 1970-2010",
#                     mapping = aes(fill = Year, color = Year),
                     axisLabels = "internal",
                     upper = list(continuous = wrap("density", alpha = 1), combo = "box"),
                     lower = list(continuous = wrap("points", alpha = 1/3), combo = wrap("dot")),
                     diag = list(continuous = wrap("diagAxis"))) +
  theme_bw(30) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

female.plot

# male metrics
male.plot = ggpairs(dat[, .(Male.Schooling, Uneducated.Males, Males.with.Tertiary.Schooling, Growth.Rate)], 
                      title = "Male Education and Population Growth Rate\nfor 144 Countries between 1970-2010",
                      #                     mapping = aes(fill = Year, color = Year),
                      axisLabels = "internal",
                      upper = list(continuous = wrap("density", alpha = 1), combo = "box"),
                      lower = list(continuous = wrap("points", alpha = 1/3), combo = wrap("dot")),
                      diag = list(continuous = wrap("diagAxis"))) +
  theme_bw(30) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

male.plot

# population metrics
pop.plot = ggpairs(dat[, .(Total.Schooling, Uneducated.Total, Total.with.Tertiary.Schooling, Growth.Rate)], 
                    title = "Education and Population Growth Rate\nfor 144 Countries between 1970-2010",
                    #                     mapping = aes(fill = Year, color = Year),
                    axisLabels = "internal",
                    upper = list(continuous = wrap("density", alpha = 1), combo = "box"),
                    lower = list(continuous = wrap("points", alpha = 1/3), combo = wrap("dot")),
                    diag = list(continuous = wrap("diagAxis"))) +
  theme_bw(30) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

pop.plot

# gender gap metrics
gap.plot = ggpairs(dat[, .(Schooling.Gender.Gap, Uneducated.Gender.Gap, Tertiary.Schooling.Gender.Gap, Growth.Rate)], 
                   title = "Gender Gap in Education and Population Growth Rate\nfor 144 Countries between 1970-2010",
                   #                     mapping = aes(fill = Year, color = Year),
                   axisLabels = "internal",
                   upper = list(continuous = wrap("density", alpha = 1), combo = "box"),
                   lower = list(continuous = wrap("points", alpha = 1/3), combo = wrap("dot")),
                   diag = list(continuous = wrap("diagAxis"))) +
  theme_bw(30) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

gap.plot

# set up a pdf file to capture our graphic
pdf("Female Education and Population Growth Rate.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(female.plot)

# close off the connection
dev.off()

# set up a pdf file to capture our graphic
pdf("Male Education and Population Growth Rate.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(male.plot)

# close off the connection
dev.off()

# set up a pdf file to capture our graphic
pdf("Education and Population Growth Rate.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(pop.plot)

# close off the connection
dev.off()

# set up a pdf file to capture our graphic
pdf("Gender Gap in Education and Population Growth Rate.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(gap.plot)

# close off the connection
dev.off()

# extract the variables of interest from dat
DT = data.table(dat[, .(Growth.Rate, Latitude, Longitude, Year,
                        Total.Schooling, Uneducated.Total, Total.with.Tertiary.Schooling, 
                        Schooling.Gender.Gap, Uneducated.Gender.Gap, Female.Proportion)])

# export DT
write.csv(DT, "train.csv", row.names = FALSE)
write.csv(DT[,!"Growth.Rate"], "test.csv", row.names = FALSE)

}


















