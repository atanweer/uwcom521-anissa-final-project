setwd("/Users/anissa/Github/uwcom521-anissa-final-project")

#Import the clean versions of the data sets for my three variables and assign to new dataframes.

odc.df <-read.csv("clean.odc.scores.csv")
wealth.df <- read.csv("clean.home.val.csv")
industry.df <-read.csv("clean.lq.csv")

#Merge the three variable files together

merge.1 <- merge(odc.df, wealth.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)
merge.2 <- merge(merge.1, industry.df, by.x = "area_title", by.y = "area_title", all.x = TRUE, all.y = TRUE)

#Create a main dataframe to work with from this merge and include just 
#the variables I want in the order I want.

d.all.cases <- data.frame(city_name = merge.2$odc_city_name, msa = merge.2$area_title, odc_score = merge.2$odc_score, 
                odc_rank = merge.2$odc_rank, home_value = merge.2$est.med.home.val.dollars, 
                construction = merge.2$construction_lq, ed_health = merge.2$ed.health_lq, 
                finance = merge.2$finance_lq, goods =  merge.2$goods_lq, information = merge.2$info_lq, 
                leis_hosp = merge.2$leis.hosp_lq, manufacturing = merge.2$manufacturing_lq, 
                mining = merge.2$mining_lq, other_serv = merge.2$other.serv_lq, pro_bus = merge.2$pro.bus_lq, 
                #pub_admin = merge.2$pub.admin_lq, 
                service = merge.2$service_lq, 
                trad_trans_util =  merge.2$trade.trans.util_lq, unclassified = merge.2$unclassified_lq)
write.csv(d.all.cases, file = "all.cases.csv")

#Recode ODC scores of 0 as NA

d.all.cases$odc_score[d.all.cases$odc_score == 0] <- NA

#Now just get complete cases
d.complete.cases <- d.all.cases[complete.cases(d.all.cases), ]  
write.csv(d.complete.cases, file = "complete.cases.csv")

nrow(d.complete.cases)
#Only 31 observations of complete cases
#That's not enough for 15 variables (14 sectors + wealth)
#So I think I should just do a model that's theoretically informed, and look at a particular industry
#concentration ... information. Or may information, public administration, and higher ed. Since 
#those are the sectors I'm actually interested in!

#plot(d.complete.cases$home_value, d.complete.cases$odc_score)
#This didn't look very linear at all, so commenting it out, logging home values


plot(log(d.complete.cases$home_value), d.complete.cases$odc_score)

lm.score.wealth <- lm(odc_score ~ log(home_value), data=d.complete.cases)
summary(lm.score.wealth)
confint(lm.score.wealth)


plot(log(d.complete.cases$home_value), d.complete.cases$odc_score, xlab = "Median Home Value ($)", ylab = "ODC Score")
abline(lm(d.complete.cases$odc_score ~ log(d.complete.cases$home_value)))


#If my coefficient is 420, this is how you would interpret it:
#log(1.1)*420
# 40.03028
# A 10% increase in home value is associated with  a 40 point increase in ODC score. See here:
#http://stats.idre.ucla.edu/sas/faq/how-can-i-interpret-log-transformed-variables-in-terms-of-percent-change-in-linear-regression/
#http://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed/


plot(d.complete.cases$information, d.complete.cases$odc_score, xlab = "Information Sector Concentration (LQ)")

lm.score.info <- lm(odc_score ~ information, data=d.complete.cases)
summary(lm.score.info)
confint(lm.score.info)

plot(d.complete.cases$information, d.complete.cases$odc_score, 
     xlab = "Information Sector Concentration (LQ)", ylab = "ODC Score")
abline(lm(d.complete.cases$odc_score ~ d.complete.cases$information))

#Now try a combined linear model:

lm.wealth.info <- lm(odc_score ~ log(home_value) + information, data = d.complete.cases)
summary(lm.wealth.info)
confint(lm.wealth.info)


#Plotting residuals
residuals(lm.wealth.info)
hist(residuals(lm.wealth.info))
#Should be normally distributed

#Plotting the residuals against x
plot(d.complete.cases$home_value, residuals(lm.wealth.info))
plot(d.complete.cases$information, residuals(lm.wealth.info))

#Should be evenly distributed across the "0" line
#If not, might have some heteroschasisty


#install.packages("stargazer")
library(stargazer)
stargazer(lm.score.info, lm.score.wealth, lm.wealth.info, type="html")

write(stargazer(lm.score.info, lm.score.wealth, lm.wealth.info, type="html"), 
      file = "odc.score.models.html")

nrow(d.complete.cases)
