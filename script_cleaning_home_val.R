setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

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

