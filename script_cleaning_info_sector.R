setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

#INFORMATION SECTOR DATA MUNGING

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
info.lq

hist(info.lq$info_lq)

###############

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

#MERGE THESE DATAFRAMES TOGETHER ACCORDING TO THE area_fips COLUMN

#new.df <- merge(first.df, second.df, by.x="SelectedColumn", by.y = "SelectedColumn", all.x = TRUE, all.y = TRUE)
#NOTE: GOOD IDEA TO ALWAYS DO all.x=TRUE and all.y=TRUE BECAUSE THAT WAY YOU CAN SEE THE DATA
#THAT DOESN'T MATCH UP, INSTEAD OF IT JUST DISAPPEARING. THIS IS IMPORTANT BECAUSE SOMETIMES
#IT MIGHT HELP YOU CATCH ANOMALIES IN DATA, E.G. LIKE ONE OBSERVATION BEING "SEATTLE, WA" AND 
#ANOTHER BEING "SEATTLE, WASHINGTON").

#Merging the info lq data with the key.fips.msa.names file

info.lq <- merge(key.fips.msa.names, info.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)

head(info.lq)
write.csv(info.lq, file = "clean.info.csv", row.names = FALSE)


### I shouldn't need to do complete cases now because I already selected for them above:
################################
#Get a set of complete cases

#lq.data <- all.sector.lq[complete.cases(all.sector.lq), ]
#head(lq.data)

#When I do this, I get zero complete cases.
#After looking through it, I realized that this is because the public administration data
#isn't aggregated to the msa level, so when I subset that data, I ended up with an empty df.

#For now, I'm just going to do complete cases for the other columns and exclude this one
#until I figure out what to do. 
#Found this example online, which I'll try to emulate:
#final[complete.cases(final[,5:6]),]

#lq.data.complete <- all.sector.lq[complete.cases(all.sector.lq[, c(1:12, 14:16)]), ]
#head(lq.data.complete)



