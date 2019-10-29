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
library(prettyR)
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
zero_suicide_dat = data.frame(death_date = zero_suicide$Date.of.Incident.k., location = zero_suicide$Centerstone.Location, dob = zero_suicide$Date.of.Birth.k., gender = zero_suicide$Gender, event = zero_suicide$Type.of.Event, path_enroll_death = zero_suicide$Enrolled.in.Pathway...time.of.Incident, cssrs_date = zero_suicide$Date.of.Most.Recent.C.SSRS, current_path_enroll_date = zero_suicide$most.current.Pathway.Date.Enrolled, current_path_disenroll_date = zero_suicide$Most.current.Pathway.Date.Disenrolled, prim_diagnosis = zero_suicide$Primary.Diagnosis, num_prior_hospital = zero_suicide$Number.of.Prior.Hospital.Admissions, total_kept_services = zero_suicide$X..of.total.kept.services, first_contact_date = zero_suicide$first.service.contact.date.with.centerstone, centernet_term_date = zero_suicide$centernet.termination.date)

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
```
Questions to answer
1. Raw number of people who died by suicide while on the pathway at the time of death: 13

2. Raw number of people who died by suicide who had previously been on the pathway (but are no longer on the pathway at the time of death): 16

3. Raw number of people who died by suicide who had never been on the pathway at the time of death: 67
```{r}
zero_suicide_questions = data.frame(path_enroll_death = zero_suicide_dat$path_enroll_death, current_path_disenroll_date = zero_suicide_dat$current_path_disenroll_date, current_path_enroll_date= zero_suicide_dat$current_path_enroll_date, death_date = zero_suicide_dat$death_date)
head(zero_suicide_questions)
## Get rid of anyone before implementation
zero_suicide_questions = subset(zero_suicide_questions, path_enroll_death != "Pre_Path")
zero_suicide_questions[is.na(zero_suicide_questions)] = "2020-01-01"
dim(zero_suicide_questions)
## Assuming NAs are N's
### Number of people never on the pathway
zero_suicide_q1 = subset(zero_suicide_questions, current_path_enroll_date < "2020-01-01")
### Now need number to get rid of those who were on the pathway, but died after disenrollment (i.e. question three)
zero_suicide_q1$died_after_diss = ifelse(zero_suicide_q1$death_date > zero_suicide_q1$current_path_disenroll_date, 0, 1)


##### Now get rid of those people who died after diss
zero_suicide_q1 = subset(zero_suicide_q1, died_after_diss == 1)
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
13+16+67
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
###################
Count analysis
Particpant character
###################


```{r}
#### Only looking at 2009 and beyond, because that is all the rate data that we have
zero_suicide_dat = subset(zero_suicide_dat, death_date >= "2009-04-01")
describe.factor(zero_suicide_dat$death_date)
range(zero_suicide_dat$death_date, na.rm = TRUE)
describe.factor(zero_suicide_dat$gender)
#describe.factor(zero_suicide_dat$event)
describe.factor(zero_suicide_dat$cssrs_date)
range(zero_suicide_dat$current_path_enroll_date, na.rm = TRUE)
range(zero_suicide_dat$current_path_disenroll_date, na.rm = TRUE)
#describe.factor(zero_suicide_dat$prim_diagnosis)
describe.factor(zero_suicide_dat$num_prior_hospital)
describe.factor(zero_suicide_dat$total_kept_services)
#### Figure who these people are 2208-07-03"
range(zero_suicide_dat$first_contact_date, na.rm = TRUE)
range(zero_suicide_dat$centernet_term_date, na.rm = TRUE)
head(zero_suicide_dat)
describe.factor(zero_suicide_dat$age_at_death)
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

Need to figure out what to do with Centerstone location
Prefer to just include two locations one that is the biggest (or include those locations that we have some sense either did really well or really poorly).

Figure out how to aggregate with full date (need to aggregate by month not day).  


Descriptives for grant and paper (need to get a few more cleaned before final)
```{r}
zero_suicide_dat
### Number of deaths
dim(zero_suicide_dat)[1]

#### Create intervention varable
zero_suicide_dat$zero_suicide = ifelse(zero_suicide_dat$death_date >="2014-01-01", 1, 0)
#### Number of deaths before and after
describe.factor(zero_suicide_dat$zero_suicide)
## Time range for deaths
range(zero_suicide_dat$death_date, na.rm = TRUE)
describe.factor(zero_suicide_dat$gender)
## Location
describe.factor(zero_suicide_dat$loc_first_word)
### average number of prior hopsital visits
mean(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)
sd(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)
### Average number of kept appointments get clarification
mean(zero_suicide_dat$total_kept_services, na.rm =TRUE)
sd(zero_suicide_dat$total_kept_services, na.rm =TRUE)
#### Death during implementation

### Average age at death
mean(zero_suicide_dat$age_at_death, na.rm = TRUE)
sd(zero_suicide_dat$age_at_death, na.rm = TRUE)

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
  mean_station_short[[i]]  =  ur.kpss(residModelH, type="tau", use.lag
                                      = lag_n_short[[i]])
  mean_station_short[[i]] = summary(mean_station_short[[i]])
}
mean_station_short

lag_n_long = c(11:20)
mean_station_long = list()
for(i in 1:length(lag_n_long)){
  mean_station_long[[i]]  =  ur.kpss(residModelH, type="tau", use.lag
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
###########################
CDC Comparison analysis
Data cleaning
###########################


Need number of deaths per year per age group and total number of people in each age group.  Then you can follow the formula in the excel sheet.
Need total deaths by age group per year

So create an age group variable

Then get the counts per year per age group

Got rid of two people with missing death dates, but we know died by suicide.
```{r}
range(zero_suicide_dat$age_at_death, na.rm = TRUE)

zero_suicide_dat$suicide = rep(1, dim(zero_suicide_dat)[1])

zero_suicide_cdc = data.frame(death_date = zero_suicide_dat$death_date, suicide = zero_suicide_dat$suicide, age_at_death = zero_suicide_dat$age_at_death)

zero_suicide_cdc$age_at_death_cat = ifelse(zero_suicide_cdc$age_at_death <= 14, "5-14", ifelse(zero_suicide_cdc$age_at_death <= 24, "15-24", ifelse(zero_suicide_cdc$age_at_death <= 34, "25-34", ifelse(zero_suicide_cdc$age_at_death <= 44, "35-44", ifelse(zero_suicide_cdc$age_at_death <= 54, "45-54", ifelse(zero_suicide_cdc$age_at_death <= 64, "55-64", ifelse(zero_suicide_cdc$age_at_death <= 74, "65-74", "75+")))))))


### Check that it worked
zero_suicide_cdc
describe.factor(zero_suicide_cdc$age_at_death_cat)
zero_suicide_cdc = subset(zero_suicide_cdc, age_at_death_cat != "5-14")


zero_suicide_cdc$death_date = floor_date(zero_suicide_cdc$death_date, unit = "year")
zero_suicide_cdc$death_date
centerstone_cdc_suicides = zero_suicide_cdc %>%
  group_by(death_date, age_at_death_cat) %>%
  summarise_if(is.numeric, sum)

centerstone_cdc_suicides$age_at_death = NULL
centerstone_cdc_suicides
```
#################
CDC Analysis
Data cleaning
#################
Notes
Load in the data for cdc and centerstone cdc rates
```{r}

## Adjust rate from cdc
head(cdc_rate)
### Centerstone totals population by age
head(centerstone_cdc_pop)
dim(centerstone_cdc_pop)

### Subset set cdc to only dates in cdc rate
centerstone_cdc_suicides = subset(centerstone_cdc_suicides, death_date > "2008-01-01" & death_date < "2018-01-01")
range(zero_suicide_cdc$death_date, na.rm = TRUE)

### Only three deaths at 14 just getting rid of them
describe.factor(centerstone_cdc_suicides$age_at_death_cat)
#### Need to add in zero rows for those counts that are zero
#2009 55-64
#2010	15-24
#2010	55-64
#2011	45-54
#2011	55-64
#2011	65-74
#2012	65-74
#2013	15-24
#2013	65-74
#2014	55-64
#2014	65-74
#2015	65-74
#2016	15-24
#2016	65-74
#2017	65-74


### Generate data set with missing zero dates and then rbind and order next
missing_zeros = data.frame(death_date = c("2009-01-01","2010-01-01","2010-01-01", "2011-01-01","2011-01-01", "2013-01-01", "2014-01-01", "2016-01-01"), age_at_death_cat = c("55-64","15-24", "55-64", "45-54", "55-64", "15-24", "55-64", "15-24"), suicide = rep(0, 8))

missing_zeros$death_date = ymd(missing_zeros$death_date)

missing_zeros
write.csv(centerstone_cdc_suicides, "centerstone_cdc_suicides.csv", row.names = FALSE)
centerstone_cdc_suicides = read.csv("centerstone_cdc_suicides.csv", header = TRUE)
write.csv(missing_zeros, "missing_zeros.csv",row.names = FALSE)
missing_zeros = read.csv("missing_zeros.csv", header= TRUE)
centerstone_cdc_suicides  = rbind(centerstone_cdc_suicides, missing_zeros)
centerstone_cdc_suicides
### Now order by year then age group
### Create a grouping variable to group by age death cat not working
centerstone_cdc_suicides$age_group = ifelse(centerstone_cdc_suicides$age_at_death_cat == "15-24", 0, ifelse(centerstone_cdc_suicides$age_at_death_cat == "25-34", 1, ifelse(centerstone_cdc_suicides$age_at_death_cat == "35-44", 2, ifelse(centerstone_cdc_suicides$age_at_death_cat == "45-54", 3, ifelse(centerstone_cdc_suicides$age_at_death_cat == "55-64", 4, ifelse(centerstone_cdc_suicides$age_at_death_cat == "65-74", 5, "Wrong"))))))
describe.factor(centerstone_cdc_suicides$age_group)


centerstone_cdc_suicides = centerstone_cdc_suicides[order(centerstone_cdc_suicides$death_date, centerstone_cdc_suicides$age_group),]
centerstone_cdc_suicides
##### Now grab population from pop data set
centerstone_cdc_suicides$pop = centerstone_cdc_pop$ClientCount
####
### Dates are all in order
centerstone_cdc_suicides$death_date == centerstone_cdc_suicides$death_date
centerstone_cdc_suicides$age_group = NULL
### Now we need to divide by the number by the pop multiple by 100,000
centerstone_cdc_suicides$crude_rate = (centerstone_cdc_suicides$suicide/centerstone_cdc_suicides$pop)*100000
centerstone_cdc_suicides

### Then we need to get the total pop for each year and rep that six times
year_pop_centerstone = centerstone_cdc_suicides %>%
  group_by(death_date)%>%
  summarise_if(is.numeric, sum)
year_pop_centerstone = year_pop_centerstone$pop
year_pop_centerstone = rep(year_pop_centerstone, each = 5)
year_pop_centerstone
length(year_pop_centerstone)

### Now combine data with crude rates with the pop per year data. 

centerstone_cdc_suicides$year_pop_centerstone = year_pop_centerstone
head(centerstone_cdc_suicides)
centerstone_cdc_suicides
### Now get age adjustment which means takes the standard population percentage of age groups times the crude rate then summed this is the standardization part
stand_2000_age = rep(c(0.139, 0.138, 0.163, 0.135, 0.087), 9)
length(stand_2000_age)
dim(centerstone_cdc_suicides)

#### Now take the crude rate multiplied by the age adjust 
centerstone_cdc_suicides$age_adjust_rate = centerstone_cdc_suicides$crude_rate*stand_2000_age
centerstone_cdc_suicides
#### Now sum by year 
centerstone_cdc_suicides_rate = centerstone_cdc_suicides %>%
  group_by(death_date)%>%
  summarise_if(is.numeric, sum)
centerstone_cdc_suicides_rate
#### Clean up CDC rate
cdc_rate = subset(cdc_rate, year > 2008)
cdc_rate$year = paste0(cdc_rate$year,"-01", "-01")
cdc_rate$year = ymd(cdc_rate$year)

### Doubles to get rid of 

```
Now plot the CDC rate 
```{r}
centerstone_cdc_suicides_rate$death_date = ymd(centerstone_cdc_suicides_rate$death_date)
centerstone_rate_graph = ggplot(centerstone_cdc_suicides_rate, aes(x = death_date, y = age_adjust_rate))+
  geom_line()+  
  labs(title="Figure 2 Centerstone age adjusted suicide (per 100,000 clients) rate 2009 to 2017")+
    scale_x_date(breaks= as.Date(c("2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01")), labels = date_format("%Y"))+ 
  geom_vline(xintercept = centerstone_cdc_suicides_rate$death_date[6], colour="red")+
  xlab("Year")+
  ylab("Centerstone age adjusted suicide rate")+
  theme(axis.title.y= element_text(size = 8))
centerstone_rate_graph
############

#########

cdc_rate_graph = ggplot(cdc_rate, aes(x = year, y = CDC_rate))+
  geom_line()+  
  labs(title="Figure 3 CDC age adjusted suicide (per 100,000 clients) rate 2009 to 2017")+
    scale_x_date(breaks= as.Date(c("2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01")), labels = date_format("%Y"))+ 
  geom_vline(xintercept = centerstone_cdc_suicides_rate$death_date[6], colour="red")+
  xlab("Year")+
  ylab("CDC age adjusted suicide rate")+
  ylim(min = 8, max = 12)+ 
  theme(axis.title.y= element_text(size = 8))

library(gridExtra)
grid.arrange(centerstone_rate_graph, cdc_rate_graph, nrow = 2)

```
####################
CDC Compare analysis
Compare rate of change
######################
Get some p_changes
```{r}
centerstone_cdc_suicides_rate
library(quantmod)
p_change_centerstone =  Delt(centerstone_cdc_suicides_rate$age_adjust_rate, type = c("arithmetic"))
colnames(p_change_centerstone) = "p_change_centerstone"
p_change_centerstone = data.frame(p_change_centerstone)
p_change_centerstone_dat =  data.frame(year = centerstone_cdc_suicides_rate$death_date, p_change_centerstone)

p_change_centerstone_dat
mean(p_change_centerstone_dat$p_change_centerstone, na.rm = TRUE)
sd(p_change_centerstone_dat$p_change_centerstone, na.rm = TRUE)
wilcox.test(p_change_centerstone_dat$p_change_centerstone)
#### Data are independent
acf(na.omit(p_change_centerstone_dat$p_change_centerstone))
pacf(na.omit(p_change_centerstone_dat$p_change_centerstone))
#### Now do cdc
p_change_cdc = Delt(cdc_rate$CDC_rate, type = "arithmetic")
colnames(p_change_cdc) = "p_change_cdc"
p_change_cdc = data.frame(p_change_cdc)
p_change_cdc_dat = data.frame(year = cdc_rate$year, p_change_cdc)
mean(p_change_cdc_dat$p_change_cdc, na.rm = TRUE)
sd(p_change_cdc_dat$p_change_cdc, na.rm = TRUE)
wilcox.test(p_change_cdc$p_change_cdc)

### Data are independent
acf(na.omit(p_change_cdc$p_change_cdc))
pacf(na.omit(p_change_cdc$p_change_cdc))
##### Now compare Centerstone to CDC
wilcox.test(p_change_centerstone_dat$p_change_centerstone, p_change_cdc$p_change_cdc)


```
Look at the 
