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

#GOODS PRODUCING SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.goods <- read.csv("qcew_msa_101_2015_annual_private_goods_producing.csv")

#Get just the entries for the metro- and micro- politan statistical areas

goods.msa <- raw.goods[grep("C", raw.goods$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

goods.msa$lq_disclosure_code <- as.character(goods.msa$lq_disclosure_code)
goods.msa$lq_disclosure_code[goods.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

goods.msa.lq.disclosed <- subset(goods.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

goods.lq <- data.frame(area_fips = goods.msa.lq.disclosed$area_fips, goods_lq = goods.msa.lq.disclosed$lq_annual_avg_emplvl)
goods.lq

hist(goods.lq$goods_lq)

#SERVICE PROVIDING SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.service <- read.csv("qcew_msa_102_2015_annual_private_service_providing.csv")

#Get just the entries for the metro- and micro- politan statistical areas

service.msa <- raw.service[grep("C", raw.service$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

service.msa$lq_disclosure_code <- as.character(service.msa$lq_disclosure_code)
service.msa$lq_disclosure_code[service.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

service.msa.lq.disclosed <- subset(service.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

service.lq <- data.frame(area_fips = service.msa.lq.disclosed$area_fips,
                         service_lq = service.msa.lq.disclosed$lq_annual_avg_emplvl)
service.lq

hist(service.lq$service_lq)

#NATURAL RESOURCE MINING SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.mining <- read.csv("qcew_msa_1011_2015_annual_private_nat_rsrs_mining.csv")

#Get just the entries for the metro- and micro- politan statistical areas

mining.msa <- raw.mining[grep("C", raw.mining$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

mining.msa$lq_disclosure_code <- as.character(mining.msa$lq_disclosure_code)
mining.msa$lq_disclosure_code[mining.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

mining.msa.lq.disclosed <- subset(mining.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

mining.lq <- data.frame(area_fips = mining.msa.lq.disclosed$area_fips,
                         mining_lq = mining.msa.lq.disclosed$lq_annual_avg_emplvl)
mining.lq

hist(mining.lq$mining_lq)

#CONSTRUCTION SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.construction <- read.csv("qcew_msa_1012_2015_annual_private_construction.csv")

#Get just the entries for the metro- and micro- politan statistical areas

construction.msa <- raw.construction[grep("C", raw.construction$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

construction.msa$lq_disclosure_code <- as.character(construction.msa$lq_disclosure_code)
construction.msa$lq_disclosure_code[construction.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

construction.msa.lq.disclosed <- subset(construction.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

construction.lq <- data.frame(area_fips = construction.msa.lq.disclosed$area_fips,
                        construction_lq = construction.msa.lq.disclosed$lq_annual_avg_emplvl)
construction.lq

hist(construction.lq$construction_lq)

#MANUFACTURING SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.manufacturing <- read.csv("qcew_msa_1013_2015_annual_private_manufacturing.csv")

#Get just the entries for the metro- and micro- politan statistical areas

manufacturing.msa <- raw.manufacturing[grep("C", raw.manufacturing$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

manufacturing.msa$lq_disclosure_code <- as.character(manufacturing.msa$lq_disclosure_code)
manufacturing.msa$lq_disclosure_code[manufacturing.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

manufacturing.msa.lq.disclosed <- subset(manufacturing.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

manufacturing.lq <- data.frame(area_fips = manufacturing.msa.lq.disclosed$area_fips,
                              manufacturing_lq = manufacturing.msa.lq.disclosed$lq_annual_avg_emplvl)
manufacturing.lq

hist(manufacturing.lq$manufacturing_lq)

#TRADE, TRANSPORTATION & UTILITIES SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.trade.trans.util <- read.csv("qcew_msa_1021_2015_annual_private_trade_transport_utilities.csv")

#Get just the entries for the metro- and micro- politan statistical areas

trade.trans.util.msa <- raw.trade.trans.util[grep("C", raw.trade.trans.util$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

trade.trans.util.msa$lq_disclosure_code <- as.character(trade.trans.util.msa$lq_disclosure_code)
trade.trans.util.msa$lq_disclosure_code[trade.trans.util.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

trade.trans.util.msa.lq.disclosed <- subset(trade.trans.util.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

trade.trans.util.lq <- data.frame(area_fips = trade.trans.util.msa.lq.disclosed$area_fips,
                               trade.trans.util_lq = trade.trans.util.msa.lq.disclosed$lq_annual_avg_emplvl)
trade.trans.util.lq

hist(trade.trans.util.lq$trade.trans.util_lq)

#FINANCIAL ACTIVITIES SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.finance <- read.csv("qcew_msa_1023_2015_annual_private_financial_activities.csv")

#Get just the entries for the metro- and micro- politan statistical areas

finance.msa <- raw.finance[grep("C", raw.finance$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

finance.msa$lq_disclosure_code <- as.character(finance.msa$lq_disclosure_code)
finance.msa$lq_disclosure_code[finance.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

finance.msa.lq.disclosed <- subset(finance.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

finance.lq <- data.frame(area_fips = finance.msa.lq.disclosed$area_fips,
                               finance_lq = finance.msa.lq.disclosed$lq_annual_avg_emplvl)
finance.lq

hist(finance.lq$finance_lq)

#PROFESSIONAL AND BUSINESS SERVICES SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.pro.bus <- read.csv("qcew_msa_1024_2015_annual_private_professional_and_business_services.csv")

#Get just the entries for the metro- and micro- politan statistical areas

pro.bus.msa <- raw.pro.bus[grep("C", raw.pro.bus$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

pro.bus.msa$lq_disclosure_code <- as.character(pro.bus.msa$lq_disclosure_code)
pro.bus.msa$lq_disclosure_code[pro.bus.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

pro.bus.msa.lq.disclosed <- subset(pro.bus.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

pro.bus.lq <- data.frame(area_fips = pro.bus.msa.lq.disclosed$area_fips,
                         pro.bus_lq = pro.bus.msa.lq.disclosed$lq_annual_avg_emplvl)
pro.bus.lq

hist(pro.bus.lq$pro.bus_lq)


#EDUCATION AND HEALTH SERVICES  SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.ed.health <- read.csv("qcew_msa_1025_2015_annual_private_education_and_health_services.csv")

#Get just the entries for the metro- and micro- politan statistical areas

ed.health.msa <- raw.ed.health[grep("C", raw.ed.health$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

ed.health.msa$lq_disclosure_code <- as.character(ed.health.msa$lq_disclosure_code)
ed.health.msa$lq_disclosure_code[ed.health.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

ed.health.msa.lq.disclosed <- subset(ed.health.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

ed.health.lq <- data.frame(area_fips = ed.health.msa.lq.disclosed$area_fips,
                         ed.health_lq = ed.health.msa.lq.disclosed$lq_annual_avg_emplvl)
ed.health.lq

hist(ed.health.lq$ed.health_lq)


#LEISURE & HOSPITALITY SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.leis.hosp <- read.csv("qcew_msa_1026_2015_annual_private_leisure_and_hospitality.csv")

#Get just the entries for the metro- and micro- politan statistical areas

leis.hosp.msa <- raw.leis.hosp[grep("C", raw.leis.hosp$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

leis.hosp.msa$lq_disclosure_code <- as.character(leis.hosp.msa$lq_disclosure_code)
leis.hosp.msa$lq_disclosure_code[leis.hosp.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

leis.hosp.msa.lq.disclosed <- subset(leis.hosp.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

leis.hosp.lq <- data.frame(area_fips = leis.hosp.msa.lq.disclosed$area_fips,
                         leis.hosp_lq = leis.hosp.msa.lq.disclosed$lq_annual_avg_emplvl)
leis.hosp.lq

hist(leis.hosp.lq$leis.hosp_lq)


#OTHER SERVICES SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.other.serv <- read.csv("qcew_msa_1027_2015_annual_private_other_services.csv")

#Get just the entries for the metro- and micro- politan statistical areas

other.serv.msa <- raw.other.serv[grep("C", raw.other.serv$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

other.serv.msa$lq_disclosure_code <- as.character(other.serv.msa$lq_disclosure_code)
other.serv.msa$lq_disclosure_code[other.serv.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

other.serv.msa.lq.disclosed <- subset(other.serv.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

other.serv.lq <- data.frame(area_fips = other.serv.msa.lq.disclosed$area_fips,
                         other.serv_lq = other.serv.msa.lq.disclosed$lq_annual_avg_emplvl)
other.serv.lq

hist(other.serv.lq$other.serv_lq)

#LOCAL PUBLIC ADMINISTRATION SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.pub.admin <- read.csv("qcew_msa_1028_2015_localgov_public_administration.csv")

#Get just the entries for the metro- and micro- politan statistical areas

pub.admin.msa <- raw.pub.admin[grep("C", raw.pub.admin$area_fips), ]
head(pub.admin.msa)
#There aren't any msa's reported for public administration data
#This is what is messing everything up later!
#Why I couldn't plot this one, and why I'm getting zero complete cases!
#Public administration data is clearly not aggregated to the msa level
#So not sure what I should do

#Now, keep only the cities msa's that have reported location quotients 

pub.admin.msa$lq_disclosure_code <- as.character(pub.admin.msa$lq_disclosure_code)
pub.admin.msa$lq_disclosure_code[pub.admin.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

pub.admin.msa.lq.disclosed <- subset(pub.admin.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

pub.admin.lq <- data.frame(area_fips = pub.admin.msa.lq.disclosed$area_fips, 
                           pub.admin_lq = pub.admin.msa.lq.disclosed$lq_annual_avg_emplvl)
pub.admin.lq

hist(pub.admin.lq$pub.admin_lq)
#I GET AN ERROR MESSAGE FOR THIS ONE, AND I'M NOT SURE WHY!

#UNCLASSIFIED SECTOR DATA MUNGING

#Make a copy of the dataset that I won't touch:
raw.unclassified <- read.csv("qcew_msa_1029_2015_annual_private_unclassified.csv")

#Get just the entries for the metro- and micro- politan statistical areas

unclassified.msa <- raw.unclassified[grep("C", raw.unclassified$area_fips), ]

#Now, keep only the cities msa's that have reported location quotients 

unclassified.msa$lq_disclosure_code <- as.character(unclassified.msa$lq_disclosure_code)
unclassified.msa$lq_disclosure_code[unclassified.msa$lq_disclosure_code==""] <- "Y"

#Now subset only for the Y's

unclassified.msa.lq.disclosed <- subset(unclassified.msa, lq_disclosure_code == "Y")

#Now create a new dataframe with only the columns I want, which is area_fips and lq_annual_avg_emplvl

unclassified.lq <- data.frame(area_fips = unclassified.msa.lq.disclosed$area_fips,
                         unclassified_lq = unclassified.msa.lq.disclosed$lq_annual_avg_emplvl)
unclassified.lq

hist(unclassified.lq$unclassified_lq)


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

#First, start merging the lq data with the key.fips.msa.names file

merged.0 <- merge(key.fips.msa.names, construction.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.1 <- merge(merged.0, ed.health.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.2 <- merge(merged.1, finance.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.3 <- merge(merged.2, goods.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.4 <- merge(merged.3, info.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.5 <- merge(merged.4, leis.hosp.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.6 <- merge(merged.5, manufacturing.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.7 <- merge(merged.6, mining.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.8 <- merge(merged.7, other.serv.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.9 <- merge(merged.8, pro.bus.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.10 <- merge(merged.9, pub.admin.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.11 <- merge(merged.10, service.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.12 <- merge(merged.11, trade.trans.util.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)
merged.13 <- merge(merged.12, unclassified.lq, by.x = "area_fips", by.y = "area_fips", all.x = TRUE, all.y = TRUE)

all.sector.lq <- merged.13
head(all.sector.lq)
write.csv(all.sector.lq, file = "all_sector_lq.csv")

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

lq.data.complete <- all.sector.lq[complete.cases(all.sector.lq[, c(1:12, 14:16)]), ]
head(lq.data.complete)

write.csv(lq.data.complete, file = "clean.lq.csv", row.names = FALSE)

