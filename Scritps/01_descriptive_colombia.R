###################################
# Descriptive statistics
###################################

rm(list=ls())

library(foreign)
library(lmtest)
library(sandwich)
library(stargazer)
library(msm)
library(Hmisc)
library(ggplot2)
library(lfe)
library(data.table)
library(tidyverse)
library(readstata13)
library(estimatr)
library(jtools)

########################
# Prepare data
########################

# load data
#load("C:/Users/catis/Dropbox/Migration Colombia/01_data/clean_data/clean_colombia2021march2.RData")
load("~/Dropbox/Migration Colombia/01_data/clean_data/clean_colombia_2021march2.RData")
names(d)

# drop units
table(d$municipio)
d = d[d$municipio!="bogota",]
d = d[d$municipio!="medellin",]
d = d[d$municipio!="cali",]
d = d[d$municipio!="barranquilla",]
table(d$municipio)

########################################################################
# Descriptive
########################################################################

descriptive = data.frame(d$outcome_health_lack,d$outcome_lack_education,d$min_distance,d$post,d$edu,d$age,d$female,d$size,d$num_victims)

colnames(descriptive) = c("Concerns about (lack of) health care service",
                    "Concerns about (lack of) education",
                    "Distance to closest border crossing",
                    "After 2015","Education",
                    "Age",
                    "Female",
                    "Size",
                    "Deaths associated to political violence (2010-2012)")
colnames(descriptive)

stargazer(descriptive,
          type = "text",
          colnames = FALSE,
          min.max = FALSE,
          title="", 
          summary.stat = c("mean", "sd","min","max","n"),
          digits=2, 
          rownames=FALSE, 
          align=TRUE, 
          float = TRUE, 
          float.env = "table", 
          table.placement = "H",
          header = FALSE, 
          no.space = TRUE,
          out="~/Dropbox/Migration Colombia/03_manuscript/tables/t_descriptive.html")

