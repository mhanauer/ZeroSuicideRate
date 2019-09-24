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
library(Zelig)
library(BEST)
library(MCMCpack)
library(MissMech)
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
zero_suicide_dat$path_enroll_death = ifelse(zero_suicide_dat$path_enroll_death == " Y", "Y", zero_suicide_dat$path_enroll_death)

zero_suicide_dat$path_enroll_death = ifelse(zero_suicide_dat$path_enroll_death == 5, "Pre_Path", ifelse(zero_suicide_dat$path_enroll_death == 2, "N", ifelse(zero_suicide_dat$path_enroll_death == 7, "Y", ifelse(zero_suicide_dat$path_enroll_death == 8, "Y", "Wrong"))))
describe.factor(zero_suicide_dat$path_enroll_death)

### Make prior number of hosptial visits numeric
zero_suicide_dat$num_prior_hospital = as.numeric(zero_suicide_dat$num_prior_hospital)

#### Make total number of kept services numeric
zero_suicide_dat$total_kept_services = as.numeric(zero_suicide_dat$total_kept_services)


```
Review age for errors
Two people with zero need to get those corrected
```{r}
age_at_death_range = data.frame(death_date = zero_suicide_dat$death_date, age_at_death = age_at_death)
range(age_at_death_range$age_at_death, na.rm = TRUE)
### Find zero age
age_at_death_dat = subset(age_at_death_range, age_at_death == 0)
age_at_death_dat

age_at_death_range = subset(age_at_death_range, age_at_death > 0)
range(age_at_death_range$age_at_death, na.rm = TRUE)
describe.factor(age_at_death_range$age_at_death, decr.order = FALSE)
```
Questions to answer
Raw number of people who died by suicide while on the pathway at the time of death: 27

Raw number of people who died by suicide who had previously been on the pathway (but are no longer on the pathway at the time of death)

Raw number of people who died by suicide who had never been on the pathway at the time of death
```{r}
zero_suicide_questions = data.frame(path_enroll_death = zero_suicide_dat$path_enroll_death, current_path_disenroll_date = zero_suicide_dat$current_path_disenroll_date, current_path_enroll_date= zero_suicide_dat$current_path_enroll_date, death_date = zero_suicide_dat$death_date)
head(zero_suicide_questions)
## Get rid of anyone before implementation
zero_suicide_questions = subset(zero_suicide_questions, path_enroll_death != "Pre_Path")
## Q1 answer
describe.factor(zero_suicide_questions$path_enroll_death)
### Answer people who died on pathway and were on the pathway at one point, but not at time of death need to confirm NAs assuming that mean no path.  Get people were on the pathway at one points which means any date on current enrollment.  Make the NAs for that variable "20120-01-01" 
zero_suicide_q2 = zero_suicide_questions
zero_suicide_q2$current_path_enroll_date[is.na(zero_suicide_q2$current_path_enroll_date)] = "2020-01-01"
### Now subset for anyone who is not "2020-01-01"
zero_suicide_q2 = subset(zero_suicide_q2, current_path_enroll_date != "2020-01-01")
#### Now we need to find if the date for the death is after the disenrollment date make a new variable
sum(is.na(zero_suicide_q2))
zero_suicide_q2$death_after_diss = ifelse(zero_suicide_q2$death_date-zero_suicide_q2$current_path_disenroll_date > 0, 1,0)
head(zero_suicide_q2)
### Q2 answer
describe.factor(zero_suicide_q2$death_after_diss)

#### Now q3 those who have current path enroll as NA assuming means never on the path
zero_suicide_q3 = zero_suicide_questions
zero_suicide_q3$current_path_enroll_date[is.na(zero_suicide_q3$current_path_enroll_date)] = "2020-01-01"
zero_suicide_q3 = subset(zero_suicide_q3, current_path_enroll_date == "2020-01-01")
describe.factor(zero_suicide_q3$current_path_enroll_date)
dim(zero_suicide_q3)[1]
### Q3 answer
dim(zero_suicide_questions)
### Almost everyone who was not on the pathway at the time death was never on the pathway 69 versus 67
describe.factor(zero_suicide_questions$path_enroll_death)
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
