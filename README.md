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
head(zero_suicide$centernet.termination.date)
zero_suicide_dat = data.frame(death_date = zero_suicide$Date.of.Incident.k., location = zero_suicide$Centerstone.Location, dob = zero_suicide$Date.of.Birth.k., gender = zero_suicide$Gender, event = zero_suicide$Type.of.Event, path_enroll_death = zero_suicide$Enrolled.in.Pathway...time.of.Incident, cssrs_date = zero_suicide$Date.of.Most.Recent.C.SSRS, current_path_enroll_date = zero_suicide$most.current.Pathway.Date.Enrolled, current_path_disenroll_date = zero_suicide$Most.current.Pathway.Date.Disenrolled, active_inactive = zero_suicide$Active.Inactive.Consumer, prim_diagnosis = zero_suicide$Primary.Diagnosis, num_prior_hospital = zero_suicide$Number.of.Prior.Hospital.Admissions, total_kept_services = zero_suicide$X..of.total.kept.services, first_contact_date = zero_suicide$first.service.contact.date.with.centerstone, centernet_term_date = zero_suicide$centernet.termination.date)

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
```
Review age for errors
Two people with zero need to get those corrected
```{r}
age_at_death_range = data.frame(age_at_death = age_at_death)
age_at_death_range = subset(age_at_death_range, age_at_death > 0)
range(age_at_death_range, na.rm = TRUE)
describe.factor(age_at_death, decr.order = FALSE)
```
Two people were inactive?? Seems odd. 
```{r}
describe.factor(zero_suicide_dat$active_inactive)
```
Look at variables and make sure nothing goofy is in them
```{r}
describe.factor(zero_suicide_dat$gender)
describe.factor(zero_suicide_dat$event)
describe.factor(zero_suicide_dat$cssrs_date)
describe.factor(zero_suicide_dat$num_prior_hospital)
describe.factor(zero_suicide_dat$total_kept_services)
head(zero_suicide_dat)
```


