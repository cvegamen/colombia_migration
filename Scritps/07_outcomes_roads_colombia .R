###################################
# DID estimates roads
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
#load("C:/Users/catis/Dropbox/Migration Colombia/01_data/clean_data/clean_colombia_2021march2.RData")
load("~/Dropbox/Migration Colombia/01_data/clean_data/clean_colombia_2021march2.RData")
names(d)

# drop units
table(d$municipio)
d = d[d$municipio!="bogota",]
d = d[d$municipio!="medellin",]
d = d[d$municipio!="cali",]
d = d[d$municipio!="barranquilla",]
table(d$municipio)

######################
# Main analyses
######################

# health_lack 
m1 <- felm(d$outcome_health_lack ~ d$min_distance_roads + d$post + d$min_distance_roads*d$post + d$edu + d$age + d$female + d$num_victims + d$size | 0 | 0 | d$municipio, keepX=TRUE)
summary(m1)
pe1 = m1$beta[9]*100
se1 = as.numeric(m1$cse[9]*100)
pe1
se1

# education_lack
m2 <- felm(d$outcome_lack_education ~ d$min_distance_roads + d$post + d$min_distance_roads*d$post + d$edu + d$age + d$female + d$num_victims + d$size | 0 | 0 | d$municipio, keepX=TRUE)
summary(m2)
pe2 = m2$beta[9]*100
se2 = as.numeric(m2$cse[9]*100)
pe2
se2

######################
# Plot
######################

lower95_1 = pe1 - se1*1.960
upper95_1 = pe1 + se1*1.960
lower95_2 = pe2 - se2*1.960
upper95_2 = pe2 + se2*1.960

pe_per = rbind(pe1,pe2)
se_per = rbind(se1,se2)
upper95_per = rbind(upper95_1,upper95_2)
lower95_per = rbind(lower95_1,lower95_2)
outcome_per = c("(Lack of) Health","(Lack of) Education")
results_per = data.frame(pe_per,se_per,lower95_per,upper95_per,outcome_per)
head(results_per)
levels(results_per$outcome_per)

cairo_pdf(file="~/Dropbox/Migration Colombia/03_manuscript/figures/figA3.pdf", 
          width=10, 
          height=5)
ggplot(results_per, aes(x=outcome_per, y=pe_per)) + geom_hline(yintercept=0, size=1, color = "gray", linetype = 1) + coord_flip(ylim = c(-.025, .025)) + geom_pointrange(aes(ymin=lower95_per,ymax=upper95_per),size=1,position = position_dodge(0.9)) + theme(legend.position="none") + ylab("") + xlab("") + theme_grey(base_size = 20) 
dev.off()

cairo_ps(file="~/Dropbox/Migration Colombia/03_manuscript/figures/figA3.eps", 
          width=10, 
          height=5)
ggplot(results_per, aes(x=outcome_per, y=pe_per)) + geom_hline(yintercept=0, size=1, color = "gray", linetype = 1) + coord_flip(ylim = c(-.025, .025)) + geom_pointrange(aes(ymin=lower95_per,ymax=upper95_per),size=1,position = position_dodge(0.9)) + theme(legend.position="none") + ylab("") + xlab("") + theme_grey(base_size = 20) 
dev.off()



