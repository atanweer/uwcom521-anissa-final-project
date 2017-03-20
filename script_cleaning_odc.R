setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

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

