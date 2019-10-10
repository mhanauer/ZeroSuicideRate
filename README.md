---
title: "AMA Results"
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
```
Descriptive statistics
Which variables do we want to look at
Date.of.Incident.k., Centerstone.Location, Date.of.Birth.k., Gender, Type.of.Event, Enrolled.in.Pathway...time.of.Incident, Date.of.Most.Recent.C.SSRS, most.current.Pathway.Date.Enrolled, Most.current.Pathway.Date.Disenrolled, Active.Inactive.Consumer, Primary.Diagnosis, Number.of.Prior.Hospital.Admissions, X..of.total.kept.services, first.service.contact.date.with.centerstone, centernet.termination.date

No days_last_contact_event, because the variable is very messy.
No active_inactive, because almost everyone is active.

Rename vars
Filter for just suicide deaths: SDC, SDC, HCBC SDC/HBC 
Make dates, dates in R
Create age at time of event var

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

### Create age at time of event variable by taking different between date of event and dob divide by 365
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
```
Questions to answer
Raw number of people who died by suicide while on the pathway at the time of death: 13

Raw number of people who died by suicide who had previously been on the pathway (but are no longer on the pathway at the time of death): 16

Raw number of people who died by suicide who had never been on the pathway at the time of death: 67
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
zero_suicide_q1$died_after_diss = ifelse(zero_suicide_q1$death_date > zero_suicide_q1$current_path_disenroll_date, 1, 0)


##### Now get rid of those people who died after diss
zero_suicide_q1 = subset(zero_suicide_q1, died_after_diss == 0)
zero_suicide_q1

## Q1 answer
dim(zero_suicide_q1)[1]



### Answer people who died on pathway and were on the pathway at one point, but not at time of death need to confirm NAs assuming that mean no path.  Get people were on the pathway at one points which means any date on current enrollment.  Make the NAs for that variable "20120-01-01" 

zero_suicide_q2 = subset(zero_suicide_q2, current_path_disenroll_date != "2020-01-01")
#### Now we need to find if the date for the death is after the disenrollment date make a new variable
sum(is.na(zero_suicide_q2))
zero_suicide_q2$death_after_diss = ifelse(zero_suicide_q2$death_date-zero_suicide_q2$current_path_disenroll_date > 0, 1,0)
head(zero_suicide_q2)

### Q2 answer
describe.factor(zero_suicide_q2$death_after_diss)

#### Q3 answer
zero_suicide_q3 = subset(zero_suicide_questions, current_path_enroll_date == "2020-01-01")
dim(zero_suicide_q3)[1]


dim(zero_suicide_questions)
```
Answers
```{r}
## Q1 answer
dim(zero_suicide_q1)[1]

### Q2 answer
describe.factor(zero_suicide_q2$death_after_diss)

## Q3 answer
describe.factor(zero_suicide_q1$died_after_diss)

```


Look at variables and make sure nothing goofy is in them
Need to check the dates (check ranges)

Need help on location and diagnosis

```{r}
describe.factor(zero_suicide_dat$death_date)
range(zero_suicide_dat$death_date, na.rm = TRUE)
describe.factor(zero_suicide_dat$gender)
describe.factor(zero_suicide_dat$event)
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
Need to grab the first word for location and see what happens
Maybe due rural urban divide?? 
```{r}
library(stringr)
zero_suicide_dat$loc_first_word = word(zero_suicide_dat$location, 1)
describe.factor(zero_suicide_dat$loc_first_word)
```

Need to figure out what to do with Centerstone location
Prefer to just include two locations one that is the biggest (or include those locations that we have some sense either did really well or really poorly).

Figure out how to aggregate with full date (need to aggregate by month not day).  


Descriptives for grant and paper (need to get a few more cleaned before final)
```{r}
### Number of deaths
dim(zero_suicide_dat)[1]
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

### Average age at death
mean(zero_suicide_dat$age_at_death, na.rm = TRUE)
### Count of death by CDC defined categories 5-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84 
zero_suicide_dat$age_at_death_cat = ifelse(zero_suicide_dat$age_at_death <= 14, "5-14", ifelse(zero_suicide_dat$age_at_death <= 24, "15-24", ifelse(zero_suicide_dat$age_at_death <= 34, "25-34", ifelse(zero_suicide_dat$age_at_death <= 44, "35-44", ifelse(zero_suicide_dat$age_at_death <= 54, "45-54", ifelse(zero_suicide_dat$age_at_death <= 64, "55-64", ifelse(zero_suicide_dat$age_at_death <= 74, "65-74", ifelse(zero_suicide_dat$age_at_death <= 84, "75-84", "85+"))))))))
head(zero_suicide_dat)
describe.factor(zero_suicide_dat$age_at_death_cat)
```
First aggregate then implementation variable
No covariates, because there is only a few deaths per month so just stating whether that person is male or female, etc.

So you have to get the zeros, but R doesn't know when there is a zero, because it is aggregating by death date.  If no one died that month, then we would not have any data.  

Are there any variables that would allow me aggregate with zeros?  Don't think so? So just manually add them.

Use the previous aggregation as start then add from there.

```{r}
zero_suicide_dat_agg = zero_suicide_dat
### Rounds down to the month so it is in the right month
zero_suicide_dat_agg$death_date = floor_date(zero_suicide_dat_agg$death_date, unit = "months")
zero_suicide_dat_agg$suicide = rep(1, dim(zero_suicide_dat_agg)[1])

library(dplyr)

zero_suicide_dat_agg = data.frame(death_date = zero_suicide_dat_agg$death_date, suicide = zero_suicide_dat_agg$suicide)
head(zero_suicide_dat_agg)

zero_suicide_dat_agg = zero_suicide_dat_agg %>%
  group_by(death_date) %>%
  summarise_all(funs(sum))

zero_suicide_dat_agg = na.omit(zero_suicide_dat_agg)
## Get rid of first person outlier in 2001
zero_suicide_dat_agg = zero_suicide_dat_agg[-c(1),]
zero_suicide_dat_agg

zeros = data.frame(death_date = c("2002-09-01","2002-12-01","2003-03-01", "2003-04-01", "2003-12-01", "2004-1-01", "2004-3-01", "2004-07-01", "2004-10-01", "2005-01-01","2005-04-01", "2005-05-01", "2005-07-01", "2005-09-01", "2005-10-01", "2005-12-01", "2006-03-01", "2006-05-01", "2006-09-01", "2007-05-01", "2007-06-01", "2007-07-01", "2007-08-01", "2007-09-01", "2007-11-01", "2007-12-01", "2008-02-01", "2008-03-01", "2008-05-01", "2008-10-01", "2008-06-01", "2009-06-01", "2010-01-01", "2010-04-01", "2010-05-01", "2010-07-01", "2010-12-01", "2011-02-01", "2011-03-01", "2011-05-01", "2011-07-01", "2011-08-01", "2011-09-01", "2011-10-01", "2011-11-01", "2012-12-01", "2012-03-01", "2012-07-01", "2012-11-01", "2013-03-01", "2013-04-01", "2013-08-01", "2013-10-01", "2014-06-01", "2014-07-01", "2014-08-01", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01", "2015-08-01", "2015-09-01", "2016-03-01", "2016-04-01", "2016-06-01", "2016-07-01", "2016-11-01", "2016-12-01", "2017-02-01", "2017-06-01", "2017-11-01", "2018-06-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01"))
zeros = data.frame(zeros, suicide = rep(0, dim(zeros)[1]))
zeros$death_date = ymd(zeros$death_date)
zero_suicide_dat_agg = rbind(zero_suicide_dat_agg, zeros)
zero_suicide_dat_agg = zero_suicide_dat_agg[order(zero_suicide_dat_agg$death_date),]
zero_suicide_dat_agg
write.csv(zero_suicide_dat_agg, "zero_suicide_dat_agg.csv", row.names = FALSE)
```
Plots and descriptives
```{r}
zero_suicide_dat_agg$zero_suicide = ifelse(zero_suicide_dat_agg$death_date < "2014-01-01", 0,1)
library(descr)

## Mean comparison
compmeans(zero_suicide_dat_agg$suicide, zero_suicide_dat_agg$zero_suicide)

##Number of people who died while zero suicide was implemented
zero_suicide_dat_agg %>%
  group_by(zero_suicide) %>%
  summarise(suicide_by_treat = sum(suicide))
```



Get adjusted rates for Centerstone.





















