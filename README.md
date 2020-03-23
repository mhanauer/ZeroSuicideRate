---
title: "Zero Suicide Rate"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Libraring packages
```{r}
library(stringr)
library(prettyR)
library(rstanarm)
library(MissMech)
library(lubridate)
```
Load in data. Need to check variables of interest are doing ok.
Check descriptives make sure nothing is out of bounds
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/ZeroSuicide/Matt'sData")
zero_suicide = read.csv("zero_suicide.csv", header= TRUE, na.strings = c("multiple ???", "NA"))
zero_suicide_denom = read.csv("zero_suicide_denom.csv", header = TRUE)
setwd("P:/Evaluation/TN Lives Count_Writing/ZeroSuicide/CDC")
cdc_rate = read.csv("cdc_rate.csv", header = TRUE)
centerstone_cdc_pop = read.csv("centerstone_cdc_pop.csv", header = TRUE)

zero_suicide_denom
cdc_rate
centerstone_cdc_pop

```
#######################
Count analysis 
Data cleaning
#########################
Notes
Put together variables you want which are below

```{r}
head(zero_suicide)
head(zero_suicide$centernet.termination.date)
zero_suicide_dat = data.frame(death_date = zero_suicide$Date.of.Incident.k., location = zero_suicide$Centerstone.Location, dob = zero_suicide$Date.of.Birth.k., gender = zero_suicide$Gender, event = zero_suicide$Type.of.Event, path_enroll_death = zero_suicide$Enrolled.in.Pathway...time.of.Incident, cssrs_date = zero_suicide$Date.of.Most.Recent.C.SSRS, current_path_enroll_date = zero_suicide$most.current.Pathway.Date.Enrolled, current_path_disenroll_date = zero_suicide$Most.current.Pathway.Date.Disenrolled, prim_diagnosis = zero_suicide$Primary.Diagnosis, num_prior_hospital = zero_suicide$Number.of.Prior.Hospital.Admissions, total_kept_services = zero_suicide$X..of.total.kept.services, first_contact_date = zero_suicide$first.service.contact.date.with.centerstone, centernet_term_date = zero_suicide$centernet.termination.date, ID = zero_suicide$Consumer.ID.)

head(zero_suicide_dat)
zero_suicide_dat = subset(zero_suicide_dat, event == "SDC" | event == "SDC, HCBC" | event == "SDC/HBC")
dim(zero_suicide_dat)  # matches the number in the spreadsheet
head(zero_suicide_dat)
### Change the following variables to dates death_date, dob, cssrs_date, current_path_enroll_date, current_path_disenroll_date, first_contact_date, ,centernet
library(lubridate)
zero_suicide_dat$death_date = mdy(zero_suicide_dat$death_date)
zero_suicide_dat$dob = mdy(zero_suicide_dat$dob)
zero_suicide_dat$cssrs_date = mdy(zero_suicide_dat$cssrs_date)
zero_suicide_dat$current_path_enroll_date = mdy(zero_suicide_dat$current_path_enroll_date)
zero_suicide_dat$current_path_disenroll_date = mdy(zero_suicide_dat$current_path_disenroll_date)
zero_suicide_dat$first_contact_date = mdy(zero_suicide_dat$first_contact_date)
zero_suicide_dat$centernet_term_date = mdy(zero_suicide_dat$centernet_term_date)

### Create age at time of event variable by taking difference between date of event and dob divide by 365
age_at_death = round(as.numeric(zero_suicide_dat$death_date - zero_suicide_dat$dob)/365, 0)
zero_suicide_dat$age_at_death = age_at_death
zero_suicide_dat$dob = NULL

#Hanging Y here are the original numbers changed the factors
#N / 2 = 69
#Y / 7 = 26
#Y / 8 = 1
#Not implemented yet / 5 / Pre_Path

describe.factor(zero_suicide_dat$path_enroll_death)
### See if something changes 
zero_suicide_dat$path_enroll_death = ifelse(zero_suicide_dat$path_enroll_death == " Y", "Y", zero_suicide_dat$path_enroll_death)

#Test if the numbers are the same
#dat_test = data.frame(path_enroll_death_test = zero_suicide_dat$path_enroll_death_test, path_enroll_death = zero_suicide_dat$path_enroll_death)
#head(dat_test)
#dat_test

zero_suicide_dat$path_enroll_death = ifelse(zero_suicide_dat$path_enroll_death == 5, "Pre_Path", ifelse(zero_suicide_dat$path_enroll_death == 2, "N", ifelse(zero_suicide_dat$path_enroll_death == 7, "Y", ifelse(zero_suicide_dat$path_enroll_death == 8, "Y", "Wrong"))))
describe.factor(zero_suicide_dat$path_enroll_death)

### Make prior number of hosptial visits numeric
zero_suicide_dat$num_prior_hospital = as.numeric(zero_suicide_dat$num_prior_hospital)

#### Make total number of kept services numeric
zero_suicide_dat$total_kept_services = as.numeric(zero_suicide_dat$total_kept_services)

### Get rid of death dates that are NA, make them something else then subset
sum(is.na(zero_suicide_dat$death_date))
###
zero_suicide_dat$death_date[is.na(zero_suicide_dat$death_date)] = "2020-01-01"
range(zero_suicide_dat$death_date)
zero_suicide_dat = subset(zero_suicide_dat, death_date < "2020-01-01")
sum(is.na(zero_suicide_dat$death_date))
range(zero_suicide_dat$death_date)
sum(is.na(zero_suicide_dat$death_date))
#### Create intervention varable
zero_suicide_dat$zero_suicide = ifelse(zero_suicide_dat$death_date >="2014-01-01", 1, 0)
#### Only looking at 2009 and beyond, because that is all the rate data that we have
zero_suicide_dat = subset(zero_suicide_dat, death_date >= "2009-04-01")
zero_suicide_dat$prim_diagnosis
```
############
Get diagnoses
Get a word cloud to identify top words
https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a



```{r}
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(SnowballC)
library(magrittr)
diagnosis = zero_suicide_dat$prim_diagnosis
diagnosis = wordStem(diagnosis)
length(diagnosis)
diagnosis = tolower(diagnosis)
### Stem by first comma, because everything after that is a modifer
test = substr(diagnosis, 1, regexpr(",",diagnosis)-1)
diag_test = data.frame(diagnosis, test)
write.csv(diag_test, "diag_test.csv", row.names = FALSE)
diag_test = read.csv("diag_test.csv", header = TRUE)
diag_test$test = as.character(diag_test$test)
diag_test$diagnosis= as.character(diag_test$diagnosis)
diag_test$test_diag = ifelse(diag_test$test == "", diag_test$diagnosis, diag_test$test)
diagnosis = diag_test$test_diag
```
##########
Number of words to identify the cases above
```{r}
diagnosis_doc = Corpus(VectorSource(diagnosis))
diagnosis_doc 
diagnosis_doc <- diagnosis_doc %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
diagnosis_doc <- tm_map(diagnosis_doc, removeWords, stopwords("english"))
diagnosis_doc

diagnosis_doc <- TermDocumentMatrix(diagnosis_doc) 
matrix_diagnosis_doc <- as.matrix(diagnosis_doc) 
words_diagnosis_doc <- sort(rowSums(matrix_diagnosis_doc),decreasing=TRUE) 
dat_diagnosis_doc <- data.frame(word = names(words_diagnosis_doc),freq=words_diagnosis_doc)
dat_diagnosis_doc
```
Put together into bipolar
bipolar, biplor, bpi

Put together for depressed
depressive, depressed, dep, depress

Need to figure out bipolar comes before depressed
Put together for ptsd
ptsd, posttraumatic

put together for anxiety
anxiety, anviety, gad

Schizo
schizophrenia, schizoaffective, schiz, schizo

```{r}

```



###################
Count analysis
Particpant character
###################


```{r}
range(zero_suicide_dat$death_date, na.rm = TRUE)
describe.factor(zero_suicide_dat$gender)

### Zero Suicide
describe.factor(zero_suicide_dat$zero_suicide)

range(zero_suicide_dat$current_path_disenroll_date, na.rm = TRUE)
#describe.factor(zero_suicide_dat$prim_diagnosis)
describe.factor(zero_suicide_dat$num_prior_hospital)
mean(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)
sd(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)
range(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)

describe.factor(zero_suicide_dat$age_at_death)
mean(zero_suicide_dat$age_at_death, na.rm = TRUE)
sd(zero_suicide_dat$age_at_death, na.rm = TRUE)
range(zero_suicide_dat$age_at_death, na.rm = TRUE)


### Error in pathway descriptives, because there are more deaths prior to enrollment according to death date (i.e. see Zero Suicide variables)
describe.factor(zero_suicide_dat$zero_suicide)
describe.factor(zero_suicide_dat$path_enroll_death)


```
Questions to answer
1. Raw number of people who died by suicide while on the pathway at the time of death: 13

2. Raw number of people who died by suicide who had previously been on the pathway (but are no longer on the pathway at the time of death): 16

3. Raw number of people who died by suicide who had never been on the pathway at the time of death: 54
```{r}
zero_suicide_questions = data.frame(current_path_disenroll_date = zero_suicide_dat$current_path_disenroll_date, current_path_enroll_date= zero_suicide_dat$current_path_enroll_date, death_date = zero_suicide_dat$death_date, ID= zero_suicide_dat$ID)

head(zero_suicide_questions)
## Get rid of anyone before implementation
zero_suicide_questions = subset(zero_suicide_questions, death_date >= "2014-01-01")
dim(zero_suicide_questions)

zero_suicide_questions[is.na(zero_suicide_questions)] = "2020-01-01"
dim(zero_suicide_questions)

## Assuming NAs are N's
### Number of people never on the pathway
zero_suicide_q1 = subset(zero_suicide_questions, current_path_enroll_date != "2020-01-01")
dim(zero_suicide_q1)
sum(is.na(zero_suicide_q1$current_path_disenroll_date))
### Now need number to get rid of those who were on the pathway, but died after disenrollment (i.e. question three)
zero_suicide_q1$died_after_diss = ifelse(zero_suicide_q1$death_date > zero_suicide_q1$current_path_disenroll_date, 1, 0)


##### Now get rid of those people who died after diss
zero_suicide_q1 = subset(zero_suicide_q1, died_after_diss == 0)
zero_suicide_q1

## Q1 answer
dim(zero_suicide_q1)[1]


### Answer people who died on pathway and were on the pathway at one point, but not at time of death need to confirm NAs assuming that mean no path.  Get people were on the pathway at one points which means any date on current enrollment.  Make the NAs for that variable "20120-01-01" 
zero_suicide_q2 = 
zero_suicide_q2 = subset(zero_suicide_questions, current_path_disenroll_date != "2020-01-01")
#### Now we need to find if the date for the death is after the disenrollment date make a new variable
zero_suicide_q2$death_after_diss = ifelse(zero_suicide_q2$death_date-zero_suicide_q2$current_path_disenroll_date > 1, 1,0)
head(zero_suicide_q2)

### Q2 answer
describe.factor(zero_suicide_q2$death_after_diss)

#### Q3 answer
zero_suicide_q3 = subset(zero_suicide_questions, current_path_enroll_date == "2020-01-01")
dim(zero_suicide_q3)[1]


dim(zero_suicide_questions)
13+16+54

##### Those who were on the pathway at one point in time
pathway_any = subset(zero_suicide_questions, current_path_enroll_date != "2020-01-01")
write.csv(pathway_any, "pathway_any.csv", row.names = FALSE)
```
Answers
```{r}
## Q1 answer
dim(zero_suicide_q1)[1]

### Q2 answer
describe.factor(zero_suicide_q2$death_after_diss)

## Q3 answer
dim(zero_suicide_q3)[1]

```

#########################
Other particpant charac
Maybe urban rural divide?
Need to grab the first word for location and see what happens

```{r}
library(stringr)
zero_suicide_dat$loc_first_word = word(zero_suicide_dat$location, 1)
describe.factor(zero_suicide_dat$loc_first_word)
location = data.frame(location = zero_suicide_dat$loc_first_word)
write.csv(location, "location.csv", row.names = FALSE)
```


######################
Count analysis
Data cleaning
####################
Notes

First aggregate then implementation variable
No covariates, because there is only a few deaths per month so just stating whether that person is male or female, etc.

So you have to get the zeros, but R doesn't know when there is a zero, because it is aggregating by death date.  If no one died that month, then we would not have any data.  

```{r}
zero_suicide_dat_agg = zero_suicide_dat
dim(zero_suicide_dat_agg)
### Rounds down to the month so it is in the right month
zero_suicide_dat_agg$death_date = floor_date(zero_suicide_dat_agg$death_date, unit = "months")
zero_suicide_dat_agg$suicide = rep(1, dim(zero_suicide_dat_agg)[1])
describe.factor(zero_suicide_dat_agg$suicide)
library(dplyr)

########## Just get April 2009 and forward to be consisent with the rate
range(zero_suicide_dat_agg$death_date, na.rm = TRUE)

zero_suicide_dat_agg = data.frame(death_date = zero_suicide_dat_agg$death_date, suicide = zero_suicide_dat_agg$suicide)
head(zero_suicide_dat_agg)

zero_suicide_dat_agg = zero_suicide_dat_agg %>%
  group_by(death_date) %>%
  summarise_all(funs(sum))

zero_suicide_dat_agg

zeros = data.frame(death_date = c("2009-06-01", "2010-01-01", "2010-04-01", "2010-05-01", "2010-07-01", "2010-12-01", "2011-02-01", "2011-03-01", "2011-05-01", "2011-07-01", "2011-08-01", "2011-09-01", "2011-10-01", "2011-11-01", "2012-12-01", "2012-03-01", "2012-07-01", "2012-11-01", "2013-03-01", "2013-04-01", "2013-08-01", "2013-10-01", "2014-06-01", "2014-07-01", "2014-08-01", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01", "2015-08-01", "2015-09-01", "2016-03-01", "2016-04-01", "2016-06-01", "2016-07-01", "2016-11-01", "2016-12-01", "2017-02-01", "2017-06-01", "2017-11-01", "2018-06-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01"))
zeros = data.frame(zeros, suicide = rep(0, dim(zeros)[1]))
zeros$death_date = ymd(zeros$death_date)
zero_suicide_dat_agg = rbind(zero_suicide_dat_agg, zeros)
zero_suicide_dat_agg = zero_suicide_dat_agg[order(zero_suicide_dat_agg$death_date),]
zero_suicide_dat_agg
zero_suicide_dat_agg$zero_suicide = ifelse(zero_suicide_dat_agg$death_date < "2014-01-01", 0,1)
zero_suicide_dat_agg
### Match total number of suicides
sum(zero_suicide_dat_agg$suicide)
```
####################
Descrip Zero Suicide
#####################
```{r}
########### Difference in mean number of suicides pre and post intervention
library(descr)
compmeans(zero_suicide_dat_agg$suicide, zero_suicide_dat_agg$zero_suicide)

###### Number of suicides per and post
pre_int = subset(zero_suicide_dat_agg, zero_suicide == 0)
post_int = subset(zero_suicide_dat_agg, zero_suicide == 1)

sum(pre_int$suicide)
sum(post_int$suicide)

############### Range of suicides
#### Range of deaths number of deaths before and after
range_before = subset(zero_suicide_dat_agg, zero_suicide == 0) 
range_after = subset(zero_suicide_dat_agg, zero_suicide == 1) 

range(range_before$suicide)
range(range_after$suicide)


```


###############
Count Analysis
Plots
###############
```{r}

library(descr)
### Add time varible to test slope
range(zero_suicide_dat_agg$death_date)


### add time varible
zero_suicide_dat_agg$time = 1:dim(zero_suicide_dat_agg)[1]

zero_suicide_dat_agg$death_date[58] 

library(scales)
min <- as.Date("2009-4-1")
max <- as.Date("2019-4-1")

ggplot(zero_suicide_dat_agg, aes(x = death_date, y = suicide))+
  geom_line()+
  labs(title="Figure 1 suicides by year")+
  geom_vline(xintercept = zero_suicide_dat_agg$death_date[58] , colour="red")+
  xlab("Date of death")+
  ylab("Count of suicides")+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"), limits = c(min, max))
```
##########################
Count analysis
Count with time 
#########################
Notes

Develop the model with poisson and neg comparison test for residuals afterward 
```{r}
### Level and Slope change
model_p_time = glm(suicide ~ zero_suicide*time, family = "poisson", data = zero_suicide_dat_agg)
summary(model_p_time)
library(lmtest)
library(sandwich)

results_robust = coeftest(model_p_time, vcov = sandwich)
results_robust
exp(results_robust[,1:2])
con_robust =  coefci(model_p_time, vcov = sandwich)
con_robust
exp(con_robust[2,1:2])

########### Comparing the nb to the p
model_nb_time = glm.nb(suicide ~ zero_suicide*time, data = zero_suicide_dat_agg)
summary(model_nb_time)
AIC(model_p_time)
AIC(model_nb_time)
BIC(model_p_time)
BIC(model_nb_time)
pchisq(2 * (logLik(model_p_time) - logLik(model_nb_time)), df = 1, lower.tail = FALSE)


```
Review final model looks good.
```{r}
residModelH = residuals(model_p_time)
hist(residModelH)
plot(zero_suicide_dat_agg$death_date, residModelH)
acf(residModelH)
pacf(residModelH)

### Use other test to test whether the data is stationary
library(urca)
lag_n_short = c(2:10)
mean_station_short = list()
for(i in 1:length(lag_n_short)){
mean_station_short[[i]]  =  ur.kpss(residModelH, type="tau", use.lag
 = lag_n_short[[i]])
mean_station_short[[i]] = summary(mean_station_short[[i]])
}
mean_station_short

lag_n_long = c(11:20)
mean_station_long = list()
for(i in 1:length(lag_n_long)){
mean_station_long[[i]]  =  ur.kpss(residModelH, type="mu", use.lag
 = lag_n_long[[i]])
mean_station_long[[i]] = summary(mean_station_long[[i]])
}
mean_station_long


lag_n_short = c(2:10)
trend_station_short = list()
for(i in 1:length(lag_n_short)){
trend_station_short[[i]]  =  ur.kpss(residModelH, type="mu", use.lag
 = lag_n_short[[i]])
trend_station_short[[i]] = summary(trend_station_short[[i]])
}
trend_station_short

lag_n_long = c(11:20)
trend_station_long = list()
for(i in 1:length(lag_n_long)){
trend_station_long[[i]]  =  ur.kpss(residModelH, type="tau", use.lag
 = lag_n_long[[i]])
trend_station_long[[i]] = summary(trend_station_long[[i]])
}
trend_station_long
```
#############################################
Rate analysis
Data cleaning
############################################
Notes

Need to get the date formated correctly
Need to line up the dates with the data sets.
Drop first three months no padding 
```{r}
zero_suicide_denom
zero_suicide_dat_agg

zero_suicide_denom$Period = paste0(zero_suicide_denom$Period, "-01")
zero_suicide_denom$Period = ymd(zero_suicide_denom$Period)
### Get rid of first three months so padding
zero_suicide_denom = subset(zero_suicide_denom, Period > "2009-03-01" & Period < "2019-05-01")
zero_suicide_denom

zero_suicide_rate = zero_suicide_dat_agg
### Check that dates are in order
zero_suicide_rate$client_count = zero_suicide_denom$ClientCount
zero_suicide_denom$Period == zero_suicide_rate$death_date
dim(zero_suicide_rate)
dim(zero_suicide_dat_agg)
zero_suicide_rate
```
#############
Rate analysis 
Plot
#############
```{r}
library(descr)

zero_suicide_dat_agg$death_date[58] 
zero_suicide_rate$death_date[58]
zero_suicide_rate$suicide_rate = NULL
zero_suicide_rate$suicide_rate_1000 =  (zero_suicide_rate$suicide / zero_suicide_rate$client_count)*1000
zero_suicide_rate

library(scales)
min <- as.Date("2009-4-1")
max <- as.Date("2019-4-1")
library(ggplot2)
ggplot(zero_suicide_rate, aes(x = death_date, y = suicide_rate_1000))+
  geom_line()+
  labs(title="Figure 1 suicide rate by year")+
  geom_vline(xintercept = zero_suicide_rate$death_date[58] , colour="red")+
  xlab("Date of death")+
  ylab("Rate of suicides per 1,000 clients")+
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"), limits = c(min, max))
```


#############
Rate analysis
Analysis
#############
```{r}
head(zero_suicide_rate)
# N for analysis
dim(zero_suicide_rate)
### Add time
zero_suicide_rate$time = 1:dim(zero_suicide_rate)[1]

#### Review slop change
### Slope change
model_p_rate_time = glm(suicide ~ zero_suicide*time + offset(log(client_count)), family = "poisson", data = zero_suicide_rate)
summary(model_p_rate_time)
library(lmtest)
library(sandwich)

results_robust = coeftest(model_p_rate_time, vcov = sandwich)
round(results_robust,3)
round(exp(results_robust[,1:2]),3)
con_robust =  coefci(model_p_rate_time, vcov = sandwich)
con_robust
round(exp(con_robust[,1:2]),3)


library(MASS)
model_nb_time_rate = glm.nb(suicide ~ zero_suicide*time + offset(log(client_count)), data = zero_suicide_rate)
summary(model_nb_time_rate)
AIC(model_p_rate_time)
AIC(model_nb_time_rate)
BIC(model_p_rate_time)
BIC(model_nb_time_rate)
pchisq(2 * (logLik(model_p_rate_time) - logLik(model_nb_time_rate)), df = 1, lower.tail = FALSE)

```
################
Rate analysis
Assumptions
################
```{r}
residModelH = residuals(model_p_rate_time)
hist(residModelH)
plot(zero_suicide_rate$death_date, residModelH)
range(exp(residModelH))
acf(residModelH)
pacf(residModelH)

### Use other test to test whether the data is stationary
library(urca)
lag_n_short = c(2:10)
mean_station_short = list()
for(i in 1:length(lag_n_short)){
  mean_station_short[[i]]  =  ur.kpss(residModelH, type="mu", use.lag
                                      = lag_n_short[[i]])
  mean_station_short[[i]] = summary(mean_station_short[[i]])
}
mean_station_short

lag_n_long = c(11:20)
mean_station_long = list()
for(i in 1:length(lag_n_long)){
  mean_station_long[[i]]  =  ur.kpss(residModelH, type="mu", use.lag
                                     = lag_n_long[[i]])
  mean_station_long[[i]] = summary(mean_station_long[[i]])
}
mean_station_long


lag_n_short = c(2:10)
trend_station_short = list()
for(i in 1:length(lag_n_short)){
  trend_station_short[[i]]  =  ur.kpss(residModelH, type="tau", use.lag
                                       = lag_n_short[[i]])
  trend_station_short[[i]] = summary(trend_station_short[[i]])
}
trend_station_short

lag_n_long = c(11:20)
trend_station_long = list()
for(i in 1:length(lag_n_long)){
  trend_station_long[[i]]  =  ur.kpss(residModelH, type="tau", use.lag
                                      = lag_n_long[[i]])
  trend_station_long[[i]] = summary(trend_station_long[[i]])
}
trend_station_long

```
Look at 20 months after implementation August 31st
First year greater than  2013-12-31 until less than 2015-01-01
```{r}
zero_suicide_rate_2014_2015 = subset(zero_suicide_rate, zero_suicide_rate$death_date < "2015-09-01" & zero_suicide_rate$death_date > "2013-12-31")
zero_suicide_rate_2014_2015$death_date
zero_suicide_rate_2014_2015$death_date = floor_date(zero_suicide_rate_2014_2015$death_date, unit = "year")
zero_suicide_rate_2014_2015
compare_2014_2015 = compmeans(zero_suicide_rate_2014_2015$suicide_rate_1000, zero_suicide_rate_2014_2015$death_date)
compare_2014_2015
p_change_rate = (compare_2014_2015[1]- compare_2014_2015[2]) / compare_2014_2015[2]
p_change_rate



### Count
zero_suicide_rate_2014_2015$suicide = as.numeric(zero_suicide_rate_2014_2015$suicide)
count_2014_2015 =compmeans(zero_suicide_rate_2014_2015$suicide, zero_suicide_rate_2014_2015$death_date) 
count_2014_2015
(.67-1.92)/1.92



```
What is the count and average number of zero deaths per year
What is the count of zero deaths per month 
What was the number of months with zero suicides for the first 20 months
```{r}
zero_dat =zero_suicide_rate 
zero_dat$death_date
zero_dat = zero_dat[c("death_date", "suicide")]
zero_dat_sum_month = zero_dat
zero_dat_sum_month$death_date  = floor_date(zero_dat_sum_month$death_date, unit = "year")
zero_dat_sum_month$suicide_zero = ifelse(zero_dat_sum_month$suicide == 0,1,0)
zero_dat_sum_month$suicide = NULL
zero_dat_sum_year = zero_dat_sum_month %>%
  group_by(death_date) %>%
  summarise_all(funs(sum))
names(zero_dat_sum_year) = c("year", "n_months_zero_suicides")
## 2009 should be 1 and 2010 should be 5
zero_dat_sum_year

mean_zero_suicide_all = mean(zero_dat_sum_year$n_months_zero_suicides)
mean_zero_suicide_all
sd_zero_suicide_all = sd(zero_dat_sum_year$n_months_zero_suicides)
sd_zero_suicide_all

zero_dat_sum_year_pre = subset(zero_dat_sum_year, year < "2014-01-01")
mean_zero_suicide_pre = mean(zero_dat_sum_year_pre$n_months_zero_suicides)
mean_zero_suicide_pre
sd_zero_suicide_pre = sd(zero_dat_sum_year_pre$n_months_zero_suicides)
sd_zero_suicide_pre

zero_dat_sum_year_post = subset(zero_dat_sum_year, year >= "2014-01-01")
mean_zero_suicide_post = mean(zero_dat_sum_year_post$n_months_zero_suicides)
mean_zero_suicide_post
sd_zero_suicide_post = sd(zero_dat_sum_year_post$n_months_zero_suicides)
sd_zero_suicide_post

mean_zero_suicide_all = rbind(mean_zero_suicide_all, mean_zero_suicide_pre, mean_zero_suicide_post)
mean_zero_suicide_all

sd_zero_suicide_all = rbind(sd_zero_suicide_all, sd_zero_suicide_pre, sd_zero_suicide_post)



mean_sd_zero_suicide = data.frame(mean_zero_suicide_all, sd_zero_suicide_all)
mean_sd_zero_suicide
names = c("all", "pre", "post")
mean_sd_zero_suicide = data.frame(names, mean_sd_zero_suicide)
names(mean_sd_zero_suicide) = c("stage", "mean", "sd")
rownames(mean_sd_zero_suicide)= NULL
mean_sd_zero_suicide[,2:3] = round(mean_sd_zero_suicide[,2:3],2)
mean_sd_zero_suicide
write.csv(mean_sd_zero_suicide, "mean_sd_zero_suicide.csv", row.names = FALSE)

zero_20_month_dat = subset(zero_suicide_rate, zero_suicide_rate$death_date < "2015-09-01" & zero_suicide_rate$death_date > "2013-12-31")
dim(zero_20_month_dat)
zero_20_month_dat$zero_month = ifelse(zero_20_month_dat$suicide == 0,1,0)
zero_month_count= sum(zero_20_month_dat$zero_month)
zero_month_count
zero_20_month_dat
```



