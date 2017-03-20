setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

#CLEAN OPEN DATA CENSUS DATA

#Read in the full open data census for all years
odc_all <- read.csv("ODC_all.csv")

#Find out how many cities are represented in the data
length(table(odc_all$place))

#Make the frequency table into a dataframe
city_frequency <- data.frame(table(odc_all$place))

#Make a list of just the city names so that I have that variable spelled out
odc_city_names <- city_frequency$Var1
write.csv(odc_city_names, file = "odc_city_names.csv")

#So, the csv files available from the ODC site just contain rows for individual datasets.
#There is no variable for the ODC assigned score. So I have to make a new one.
#I've captured the screen of the ODC on 3/6/17, and those are the scores I'll use. 

odc_scores <- read.csv("odc_scores.csv", stringsAsFactors = FALSE)
msa_list <- read.csv("msa.list.csv", stringsAsFactors = FALSE)
msa_list$area_title_trimmed <- sub(" MSA", "", msa_list$area_title)

#Now I need to match cities with their corresponding MSA
#Input - city name. Output - MSA

odc_to_msa <- function (odc_city_name) {
  #Make matrix that separates city name and state:
  city.state <- strsplit(as.character(odc_city_name), ", ")[[1]]
  
  city.name <- city.state[1]
  state.name <- city.state[2]
  
  matches <- msa_list$area_title_trimmed[grepl(city.name, msa_list$area_title_trimmed)]
  matches <- matches[grepl(state.name, matches)]
  
  if (length(matches)==0) {
    return(NA)
  } else if (length(matches)>1) {
    return("At least more than one city matches an MSA")
  } else {
    return(msa_list$area_title[msa_list$area_title_trimmed == matches])
  }
  
}

odc_scores$area_title <- sapply(odc_scores$odc_city_name, odc_to_msa)

write.csv(odc_scores, file = "clean.odc.scores.csv", row.names = FALSE)

#CLEAN INFORMATION SECTOR CONCENTRATION (LQ) DATA

#Make a copy of the dataset that I won't touch:
raw.info <- read.csv("qcew_msa_1022_2015_annual_private_information.csv")

#Get just the entries for the metro- and micro- politan statistical areas
#From looking at the documentation, it's clear that msa's are 
#are designated by "C" before the area_fips code,
#so I can use that to subset.

#The grep() function  subsets by character string
#Found this example online:
#irisSubset <- iris[grep("osa", iris$Species), ]

info.msa <- raw.info[grep("C", raw.info$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients, which 
#is indicated in the column "lq_disclosure_code"
#Unfortunatley, the "no's" are coded with an "N" and the "yes's" are just missing values
#So I can simply run a logical on them. 

#First, I'll code missing values as Y and then I'll subset for the Y's
#I can't just make the blanks Y's because the column is a factor and 
#there is no factor level defined for Y
#So instead of messing with adding a new factor level, which seems complicated,
#I'll coerce the vector into being a character
#The blanks aren't considered NA's anymore, so I can't just us as.na() to replace
#Instead I have to replace the blank characters like this: 

info.msa$lq_disclosure_code <- as.character(info.msa$lq_disclosure_code)
info.msa$lq_disclosure_code[info.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

info.msa.lq.disclosed <- subset(info.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

info.lq <- data.frame(area_fips = info.msa.lq.disclosed$area_fips, info_lq = info.msa.lq.disclosed$lq_annual_avg_emplvl)

hist(info.lq$info_lq)


#FIPS CODES
#This is the file that was linked to from the BLS LQ documentation page:
#https://web.archive.org/web/20170202073616/https://data.bls.gov/cew/doc/access/csv_data_slices.htm
#https://web.archive.org/web/20170202073535/https://data.bls.gov/cew/doc/titles/area/area_titles.htm

raw.fips.area <- read.csv("fips_codes_area_titles.csv")
head(raw.fips.area)

#Get just the entries for the metro- and micro- politan statistical areas

key.fips.msa.names <- raw.fips.area[grep("C", raw.fips.area$area_fips), ]
head(key.fips.msa.names)

#Further refine to exclude Micropolitan msa's. I'm only interested in larger cities.
#Micropolitan statistical areas have "MicroSA" following the place name, while
#Metropolitan statistical areas have "MSA" following the place name. 
#So I can do grep() for "MSA" as well

key.fips.msa.names <- key.fips.msa.names[grep("MSA", key.fips.msa.names$area_title), ]
head(key.fips.msa.names)

#Merge these dataframes together according to the area_fips column

#new.df <- merge(first.df, second.df, by.x="SelectedColumn", by.y = "SelectedColumn", all.x = TRUE, all.y = TRUE)
#NOTE: GOOD IDEA TO ALWAYS DO all.x=TRUE and all.y=TRUE BECAUSE THAT WAY YOU CAN SEE THE DATA
#THAT DOESN'T MATCH UP, INSTEAD OF IT JUST DISAPPEARING. THIS IS IMPORTANT BECAUSE SOMETIMES
#IT MIGHT HELP YOU CATCH ANOMALIES IN DATA, E.G. LIKE ONE OBSERVATION BEING "SEATTLE, WA" AND 
#ANOTHER BEING "SEATTLE, WASHINGTON").

#Merging the info lq data with the key.fips.msa.names file

info.lq <- merge(key.fips.msa.names, info.lq, by.x = "area_fips", 
                 by.y = "area_fips", all.x = TRUE, all.y = TRUE)

head(info.lq)
write.csv(info.lq, file = "clean.info.csv", row.names = FALSE)


# CLEAN HOME VALUE DATA

#Make a copy of the dataset that I won't touch:
raw.homes <- read.csv("census_msa_2015_1yr_med_home_value.csv")

head(raw.homes)
colnames(raw.homes)
nrow(raw.homes)

#The "GEO.id2" from this table and the "area_fips" from the lq table seem to be different
#in a systematic way. They both have four relevant digits, but in the lq data from BLS, they
#added a "C" to the end of those four digits, and in the census data they seem to have added 
#a "0" to the end of those four digits. 
#The "GEO.display.label" column in this table is also formatted slightly differently from the 
#"area_title" column in the lq table, although they represent the same information.
#In this census table, the place name is followed by "Metro Area" whereas in the lq table,
#the place name is followed by "MSA"
#In order to merge, I have to have either the id# or the place name match perfectly. 
#I think I'm going to change the place name in the census data because it seems like less of a
#dramatic transformation than changing the id numbers. 

#Going to try to emulate this example:
#group <- c("12357e", "12575e", "197e18", "e18947")
#group
#[1] "12357e" "12575e" "197e18" "e18947"

#gsub("e", "", group)
#[1] "12357" "12575" "19718" "18947"

#First, create a new df from the raw data that I can manipulate
home.val.msa <- raw.homes
colnames(home.val.msa)

home.val.msa$GEO.display.label <- gsub("Metro Area", "MSA", home.val.msa$GEO.display.label)
head(home.val.msa)

#Now I'm going to rename the column to be the same as the lq table, so that I can perform a merge
#Emulating this code:
#colnames(dataframe)[which(names(dataframe) == "columnName")] <- "newColumnName"
#Or this one:
#names(data)[names(data) == "oldVariableName"] <- "newVariableName"
#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'


names(home.val.msa)[names(home.val.msa) == "GEO.display.label"] <- "area_title"
colnames(home.val.msa)
names(home.val.msa)[names(home.val.msa) == "HD01_VD01"] <- "home_value"
colnames(home.val.msa)
names(home.val.msa)[names(home.val.msa) == "HD02_VD01"] <- "home_value_error"
colnames(home.val.msa)

colnames(home.val.msa)
head(home.val.msa)
#Just have to get rid of the first row, which explains the original column headings
#Emulating this code:
#df = df[-1,]

home.val.msa = home.val.msa[-1,]

head(home.val.msa)

#Going to write just the area_title column to a csv file so that I can match it with odc city names

write.table(home.val.msa$area_title, file = "msa.list.csv", sep=",", row.names = FALSE, col.names = c("area_title"))

write.csv(home.val.msa, file = "clean.home.val.csv", row.names = FALSE)

#CLEAN POPULATION DATA

#Make a copy of the dataset that I won't touch:
raw.pop <- read.csv("census_msa_2015_1yr_med_total_pop.csv")

head(raw.pop)
colnames(raw.pop)
nrow(raw.pop)

#The columns I need are GEO.display.label and HD01_VD01

#First, create a new df from the raw data that I can manipulate
pop.msa <- raw.pop
colnames(pop.msa)

pop.msa$GEO.display.label <- gsub("Metro Area", "MSA", pop.msa$GEO.display.label)
head(pop.msa)

#Now I'm going to rename the column to be the same as the lq table, so that I can perform a merge
#Emulating this code:
#colnames(dataframe)[which(names(dataframe) == "columnName")] <- "newColumnName"
#Or this one:
#names(data)[names(data) == "oldVariableName"] <- "newVariableName"
#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'


names(pop.msa)[names(pop.msa) == "GEO.display.label"] <- "area_title"
colnames(pop.msa)
names(pop.msa)[names(pop.msa) == "HD01_VD01"] <- "pop_est"
colnames(pop.msa)
names(pop.msa)[names(pop.msa) == "HD02_VD01"] <- "pop_est_error"
colnames(pop.msa)

head(pop.msa)
#Just have to get rid of the first row, which explains the original column headings
#Emulating this code:
#df = df[-1,]

pop.msa = pop.msa[-1,]

head(pop.msa)

write.csv(pop.msa, file = "clean.pop.csv", row.names = FALSE)


#CREATE A NEW DATAFRAME OF COMPLETE CASES OF ALL MY CLEAN VARIABLES AND WRITE TO A NEW FILE

#Import the clean versions of the data sets for my four variables and assign to new dataframes.

odc.df <-read.csv("clean.odc.scores.csv")
wealth.df <- read.csv("clean.home.val.csv")
info.df <-read.csv("clean.info.csv")
pop.df <- read.csv("clean.pop.csv")

#Merge the variable files together

merge.1.oiwp <- merge(odc.df, wealth.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)
merge.2.oiwp <- merge(merge.1.oiwp, info.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)
merge.3.oiwp <- merge(merge.2.oiwp, pop.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)

#Create a main dataframe to work with from this merge and include just 
#the variables I want in the order I want.

d.oiwp <- data.frame(city_name = merge.3.oiwp$odc_city_name, 
                     area_title = merge.3.oiwp$area_title, 
                     odc_score = merge.3.oiwp$odc_score, 
                     info_lq = merge.3.oiwp$info_lq,
                     home_value = merge.3.oiwp$home_value,
                     pop_est = merge.3.oiwp$pop_est) 

write.csv(d.oiwp, file = "all.cases.odc.info.wealth.pop.csv")

#Now just get complete cases, for which I all 4 variables are present
d.oiwp.complete.cases <- d.oiwp[complete.cases(d.oiwp), ]  
write.csv(d.oiwp.complete.cases, file = "complete.cases.odc.info.wealth.pop.csv")

#Plot my variables for the ODC cities

nrow(d.oiwp.complete.cases)
summary(d.oiwp.complete.cases[, c("odc_score", "info_lq", "home_value", "pop_est")])
hist(d.oiwp.complete.cases$odc_score)
hist(d.oiwp.complete.cases$info_lq)
hist(d.oiwp.complete.cases$home_value)
hist(d.oiwp.complete.cases$pop_est)

#Summary statistics for all cities in the ODC
nrow(d.oiwp.complete.cases)
summary(d.oiwp.complete.cases[,c(3,4,5,6)])

#REGRESSION ANALYSIS OF THE DV ODC PRESENCE ACCORDING TO THE IV'S INFO SECTOR, WEALTH, AND POPULATION

msa.info <- read.csv("clean.info.csv")
msa.home.val <- read.csv("clean.home.val.csv")
msa.pop <- read.csv("clean.pop.csv")

msa.in.odc <- d.oiwp.complete.cases
msa.in.odc <- data.frame(area_title = msa.in.odc$area_title)

msa.all <- read.csv("msa.list.csv")

msa.all$in_odc <- msa.all$area_title %in% msa.in.odc$area_title
#The way I've done this so far, I'm not including cities that had scores of zero in the ODC dataset, 
#since I am working from the "complete cases" table. 
#Mako says to try it with the zeroes as well and make a decision about it, but be 
#transparent about which ones I choose. 


merge.1 <- merge(msa.all, msa.info[,c("area_title", "info_lq")], by = "area_title")
merge.2 <- merge(merge.1, msa.home.val[,c("area_title", "home_value")], by = "area_title")
merge.3 <- merge(merge.2, msa.pop[,c("area_title", "pop_est")], by = "area_title")
head(merge.3)
msa.all <- merge.3

#Get complete cases:
msa.all <- msa.all[complete.cases(msa.all), ]

#Get summary statistics for complete cases:
head(msa.all)

msa.all.true <- as.data.frame(msa.all[which(msa.all$in_odc=="TRUE"), ])
msa.all.false <- as.data.frame(msa.all[which(msa.all$in_odc=="FALSE"), ])

nrow(msa.all.true)
summary(msa.all.true[,c(3,4,5)])
nrow(msa.all.false)
summary(msa.all.false[,c(3,4,5)])


#Run a logistic regression just for the info sector

glm.odc.presence.info <- glm(in_odc ~ info_lq, data=msa.all, family = binomial("logit"))
summary(glm.odc.presence.info)

#Run a logistic regression controlling for median home value and population size

#Log tranform home value and population size to normalize their distributions
hist(msa.all$home_value)
hist(log(msa.all$home_value))
hist(msa.all$pop_est)
hist(log(msa.all$pop_est))

glm.odc.presence.oiwp <- glm(in_odc ~ info_lq + log(home_value) + log(pop_est), data=msa.all, 
                             family = binomial("logit"))
summary(glm.odc.presence.oiwp)

exp(glm.odc.presence.oiwp$coefficients)

#Get the pseudo-variance:

install.packages("pscl")
library(pscl)
pR2(glm.odc.presence.oiwp)


#Calculate the increased odds that a city with a one unit increase in concentration
#of the info sector will appear in the ODC

exp(1.6598)
# = 5.258259

# Therefore, every one unit in information lq is associated with odds of being 
#included in the ODC that are about 5 times
#higher than an otherwise identical city with one unit lq lower. 
#Controlling for info lq, there is no effect of home pricing. 

#Get sample values for y to help interpret the coefficients of this model:

#-29.7768+1.6598(xinfo)+0.6205(log(xhomevalue))+1.4937(log(population))

#Calculate the odds of a city appearing in the ODC if their information sector
#is 1, meaning that it's the same as the rest of the country as a whole, and 
#the median home value is $219700, which is the median median home value, and
#the population is 1271142, which is the median population.
#This essentially represents an average US city. 

#-29.7768+1.6598(1)+0.6205(log(219700))+1.4937(log(1271142))
-29.7768+(1.6598*1)+(0.6205*12.30002)+(1.4937*14.05543)
# = 0.5097582

#Now Transform this to find out what the probability is that this average city
#will appear in the ODC
1/(1+exp(-1*-0.5097582))

# = .3752502
# Therefore, it has a 38% chance of appearing in the ODC

#Then do the same thing again, holding home value and population constant, but
#increasing the concentration of the information sector by 1 unit, to 2

-29.7768+(1.6598*2)+(0.6205*12.30002)+(1.4937*14.05543)
# = 2.169558

#Now Transform this to find out what the probability is that this otherwise average city
#with an info concentration of 2 will appear in the ODC:

1/(1+exp(-1*2.169558))

# = 0.8974823
#Therefore it has a 90% chance of appearing in the ODC

#install.packages("stargazer")
library(stargazer)
stargazer(glm.odc.presence.info, glm.odc.presence.oiwp, type="html")

write(stargazer(glm.odc.presence.info, glm.odc.presence.oiwp, type="html"), file = "odc.presence.models.html")


#REGRESSION ANALYSIS OF THE DV ODC SCORES ACCORDING TO THE IV'S INFO SECTOR, WEALTH, AND POPULATION

#If my coefficient is 420, this is how you would interpret it:
#log(1.1)*420
# 40.03028
# A 10% increase in home value is associated with  a 40 point increase in ODC score. See here:
#http://stats.idre.ucla.edu/sas/faq/how-can-i-interpret-log-transformed-variables-in-terms-of-percent-change-in-linear-regression/
#http://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed/

#x=info, y = score
lm.score.info <- lm(odc_score ~ info_lq, d.oiwp.complete.cases)
summary(lm.score.info)
confint(lm.score.info)

plot(d.oiwp.complete.cases$info_lq, d.oiwp.complete.cases$odc_score, 
     xlab = "Information Sector Concentration (LQ)", ylab = "ODC Score")
abline(lm(d.oiwp.complete.cases$odc_score ~ d.oiwp.complete.cases$info_lq))

#x=home value, y = score
lm.score.home <- lm(odc_score ~ log(home_value), d.oiwp.complete.cases)
summary(lm.score.home)
confint(lm.score.home)

plot(log(d.oiwp.complete.cases$home_value), d.oiwp.complete.cases$odc_score, 
     xlab = "Home Value", ylab = "ODC Score")
abline(lm(d.oiwp.complete.cases$odc_score ~ log(d.oiwp.complete.cases$home_value)))

#x = population, y = score
lm.score.pop <- lm(odc_score ~ log(pop_est), d.oiwp.complete.cases)
summary(lm.score.pop)
confint(lm.score.pop)

plot(log(d.oiwp.complete.cases$pop_est), d.oiwp.complete.cases$odc_score, 
     xlab = "Population Estimate", ylab = "ODC Score")
abline(lm(d.oiwp.complete.cases$odc_score ~ log(d.oiwp.complete.cases$pop_est)))

#Now try a combined linear model:

lm.info.wealth.pop <- lm(odc_score ~ info_lq + log(home_value) + log(pop_est), data = d.oiwp.complete.cases)
summary(lm.info.wealth.pop)
confint(lm.info.wealth.pop)


#Plotting residuals
residuals(lm.info.wealth.pop)
hist(residuals(lm.info.wealth.pop))
#Should be normally distributed

#Plotting the residuals against x
plot(d.oiwp.complete.cases$home_value, residuals(lm.info.wealth.pop))
plot(d.oiwp.complete.cases$info_lq, residuals(lm.info.wealth.pop))

#Should be evenly distributed across the "0" line
#If not, might have some heteroschasisty


#install.packages("stargazer")
library(stargazer)
stargazer(lm.score.info, lm.score.home, lm.score.pop, lm.info.wealth.pop, type="html")

write(stargazer(lm.score.info, lm.score.home, lm.score.pop, lm.info.wealth.pop, type="html"), 
      file = "odc.score.models.html")

