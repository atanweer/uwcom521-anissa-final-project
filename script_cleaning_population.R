setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

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

