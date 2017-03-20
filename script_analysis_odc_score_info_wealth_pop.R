setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

#Import the clean versions of the data sets for my four variables and assign to new dataframes.

odc.df <-read.csv("clean.odc.scores.csv")
wealth.df <- read.csv("clean.home.val.csv")
info.df <-read.csv("clean.info.csv")
pop.df <- read.csv("clean.pop.csv")

#Merge the three variable files together

merge.1.oiwp <- merge(odc.df, wealth.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)
merge.2.oiwp <- merge(merge.1.oiwp, info.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)
merge.3.oiwp <- merge(merge.2.oiwp, pop.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)

#Create a main dataframe to work with from this merge and include just 
#the variables I want in the order I want.

d.oiwp <- data.frame(city_name = merge.3.oiwp$odc_city_name, 
                     msa = merge.3.oiwp$area_title, 
                     odc_score = merge.3.oiwp$odc_score, 
                     information_lq = merge.3.oiwp$info_lq,
                     home_value = merge.3.oiwp$est.med.home.val.dollars,
                     pop_estimate = merge.3.oiwp$population.est) 
                     
write.csv(d.oiwp, file = "all.cases.odc.info.wealth.pop.csv")

#Recode ODC scores of 0 as NA

#d.oiwp$odc_score[d.oiwp$odc_score == 0] <- NA
#Commented this out because Mako said it would be better to include the zeroes. 

#Now just get complete cases
d.oiwp.complete.cases <- d.oiwp[complete.cases(d.oiwp), ]  
write.csv(d.oiwp.complete.cases, file = "complete.cases.odc.info.wealth.pop.csv")

nrow(d.oiwp.complete.cases)
#Only 30 observations of complete cases


#plot(d.oiwp.complete.cases$home_value, d.oiwp.complete.cases$odc_score)
#home values, as expected, are skewed, sologging home values


plot(log(d.oiwp.complete.cases$home_value), d.oiwp.complete.cases$odc_score)

lm.score.wealth <- lm(odc_score ~ log(home_value), data=d.oiwp.complete.cases)
summary(lm.score.wealth)
confint(lm.score.wealth)


plot(log(d.oiwp.complete.cases$home_value), d.oiwp.complete.cases$odc_score, 
     xlab = "Median Home Value ($)", ylab = "ODC Score")
abline(lm(d.oiwp.complete.cases$odc_score ~ log(d.oiwp.complete.cases$home_value)))


#If my coefficient is 420, this is how you would interpret it:
#log(1.1)*420
# 40.03028
# A 10% increase in home value is associated with  a 40 point increase in ODC score. See here:
#http://stats.idre.ucla.edu/sas/faq/how-can-i-interpret-log-transformed-variables-in-terms-of-percent-change-in-linear-regression/
#http://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed/


plot(d.oiwp.complete.cases$information_lq, d.oiwp.complete.cases$odc_score, 
     xlab = "Information Sector Concentration (LQ)")

lm.score.info <- lm(odc_score ~ information_lq, d.oiwp.complete.cases)
summary(lm.score.info)
confint(lm.score.info)

plot(d.oiwp.complete.cases$information_lq, d.oiwp.complete.cases$odc_score, 
     xlab = "Information Sector Concentration (LQ)", ylab = "ODC Score")
abline(lm(d.oiwp.complete.cases$odc_score ~ d.oiwp.complete.cases$information))

plot(d.oiwp.complete.cases$pop_estimate, 
     d.oiwp.complete.cases$odc_score, 
     xlab = "Population Estimate")

lm.score.pop <- lm(odc_score ~ pop_estimate, d.oiwp.complete.cases)
summary(lm.score.pop)
confint(lm.score.pop)

plot(d.oiwp.complete.cases$pop_estimate, d.oiwp.complete.cases$odc_score, 
     xlab = "Population Estimate", ylab = "ODC Score")
abline(lm(d.oiwp.complete.cases$odc_score ~ d.oiwp.complete.cases$pop_estimate))

#Now try a combined linear model:

lm.info.wealth.pop <- lm(odc_score ~ log(home_value) + information_lq + pop_estimate, data = d.oiwp.complete.cases)
summary(lm.info.wealth.pop)
confint(lm.info.wealth.pop)


#Plotting residuals
residuals(lm.info.wealth.pop)
hist(residuals(lm.info.wealth.pop))
#Should be normally distributed

#Plotting the residuals against x
plot(d.oiwp.complete.cases$home_value, residuals(lm.info.wealth.pop))
plot(d.oiwp.complete.cases$information, residuals(lm.info.wealth.pop))

#Should be evenly distributed across the "0" line
#If not, might have some heteroschasisty


#install.packages("stargazer")
library(stargazer)
stargazer(lm.score.info, lm.score.wealth, lm.score.pop, lm.info.wealth.pop, type="html")

write(stargazer(lm.score.info, lm.score.wealth, lm.score.pop, lm.info.wealth.pop, type="html"), 
      file = "odc.score.models.html")


