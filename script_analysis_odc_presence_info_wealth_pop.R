setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

#What I want to do with this analysis is see if wealth or information predict the inclusion of an msa in the ODC

#So, compare the list of all msa's with the list of msa's in the complete cases of the odc_score analysis

list.files()
msa.info <- read.csv("clean.info.csv")
msa.home.val <- read.csv("clean.home.val.csv")
msa.pop <- read.csv("clean.pop.csv")

msa.in.odc <- read.csv("complete.cases.odc.info.wealth.pop.csv")
msa.in.odc <- data.frame(area_title = msa.in.odc$msa)

msa.all <- read.csv("msa.list.csv")

msa.all$in.odc <- msa.all$area_title %in% msa.in.odc$area_title
#The way I've done this so far, I'm not including cities that had scores of zero in the ODC dataset, 
#since I am working from the "complete cases" table. 
#Mako says to try it with the zeroes as well and make a decision about it, but be 
#transparent about which ones I choose. 


merge.1 <- merge(msa.all, msa.info[,c("area_title", "info_lq")], by = "area_title")
merge.2 <- merge(merge.1, msa.home.val[,c("area_title", "est.med.home.val.dollars")], by = "area_title")
merge.3 <- merge(merge.2, msa.pop[,c("area_title", "population.est")], by = "area_title")
head(merge.3)
msa.all <- merge.3
#There are some NA's in for the information column, but I think the linear model should drop them

glm.odc.presence.info <- glm(in.odc ~ info_lq, data=msa.all, family = binomial("logit"))
summary(glm.odc.presence.info)

glm.odc.presence.oiwp <- glm(in.odc ~ info_lq + log(est.med.home.val.dollars) + population.est, data=msa.all, 
                        family = binomial("logit"))
summary(glm.odc.presence.oiwp)

exp(glm.odc.presence.oiwp$coefficients)


# Every one unit in information lq is associated with odds of being included in the ODC that are 52 times
#higher than an otherwise identical city with one unit lq lower. 
#Controlling for info lq, there is no effect of home pricing. 


#Present model with home values, show they matter
#Rich cities are just tech cities. 

#> 1/(1+exp(-1*2.32))
#[1] 0.9105199
#> 1/(1+exp(-1*-1.58))
#[1] 0.1707955

#A city with an info concentration of 1 would have a 17% chance of being in the ODC
#A city with an info concentration of 2 would have a 91% chance of being in the ODC

#install.packages("stargazer")
library(stargazer)
stargazer(glm.odc.presence.info, glm.odc.presence.oiwp, type="html")

write(stargazer(glm.odc.presence.info, glm.odc.presence.oiwp, type="html"), file = "odc.presence.models.html")


