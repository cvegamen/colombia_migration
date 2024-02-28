###################################
# DID estimates
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

library(stargazer)
library(lme4)
library(ggplot2)
library(data.table)
library(pdftools)
library(sjPlot)

########################
# Prepare data
########################

# load data
load("C:/Users/catve/Box/Migration Colombia/01_data/clean_data/archive/clean_colombia_2021march2.RData")

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
m1 <- felm(outcome_health_lack ~ min_distance + post + min_distance * post +  edu + age + female + num_victims + size | 0 | 0 | municipio,  data = d, keepX = TRUE)
summary(m1)
pe1 = m1$beta[9]*100
se1 = as.numeric(m1$cse[9]*100)
pe1
se1

# education_lack
m2 <- felm(outcome_lack_education ~ min_distance + post + min_distance * post +  edu + age + female + num_victims + size | 0 | 0 | municipio,  data = d, keepX = TRUE)
summary(m2)

pe2 = m2$beta[9]*100
se2 = as.numeric(m2$cse[9]*100)
pe2
se2

#security
m3 <- felm(outcome_lack_security ~ min_distance + post + min_distance * post +  edu + age + female + num_victims + size | 0 | 0 | municipio,  data = d, keepX = TRUE)
summary(m3)

pe3 = m3$beta[9]*100
pe3
se3=as.numeric(m3$cse[9]*100)
se3
m4 <- felm(outcome_crime ~ min_distance + post + min_distance * post +  edu + age + female + num_victims + size | 0 | 0 | municipio,  data = d, keepX = TRUE)
summary(m4)

######################
# Plot health and edu
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

cairo_pdf(file="~/Dropbox/Migration Colombia/03_manuscript/figures/fig3.pdf", 
          width=10, 
          height=5)
ggplot(results_per, aes(x=outcome_per, y=pe_per)) + geom_hline(yintercept=0, size=1, color = "gray", linetype = 1) + coord_flip(ylim = c(-.025, .025)) + geom_pointrange(aes(ymin=lower95_per,ymax=upper95_per),size=1,position = position_dodge(0.9)) + theme(legend.position="none") + ylab("Change in concerns (in percentage points)") + xlab("") + theme_grey(base_size = 20) 
dev.off()

cairo_ps(file="~/Dropbox/Migration Colombia/03_manuscript/figures/fig3.eps", 
          width=10, 
          height=5)
ggplot(results_per, aes(x=outcome_per, y=pe_per)) + geom_hline(yintercept=0, size=1, color = "gray", linetype = 1) + coord_flip(ylim = c(-.025, .025)) + geom_pointrange(aes(ymin=lower95_per,ymax=upper95_per),size=1,position = position_dodge(0.9)) + theme(legend.position="none") + ylab("Change in concerns (in percentage points)") + xlab("") + theme_grey(base_size = 20) 
dev.off()


####
######################
# Plot security
######################

lower95_per = pe3 - se3*1.960
upper95_per = pe3 + se3*1.960


outcome_per = c("(Lack of) Security")
results_per = data.frame(pe3,se3,lower95_per,upper95_per,outcome_per)
head(results_per)

ggplot(results_per, aes(x=outcome_per, y=pe3)) + geom_hline(yintercept=0, size=1, color = "gray", linetype = 1) + coord_flip(ylim = c(-.025, .025)) + geom_pointrange(aes(ymin=lower95_per,ymax=upper95_per),size=1,position = position_dodge(0.9)) + theme(legend.position="none") + ylab("Change in concerns (in percentage points)") + xlab("") + theme_grey(base_size = 20) 
dev.off()

cairo_pdf(file="C:/Users/catve/Box/Migration Colombia/03_manuscript/figures/fig4.pdf", 
          width=10, 
          height=5)
ggplot(results_per, aes(x=outcome_per, y=pe3)) + geom_hline(yintercept=0, size=1, color = "gray", linetype = 1) + coord_flip(ylim = c(-.025, .025)) + geom_pointrange(aes(ymin=lower95_per,ymax=upper95_per),size=1,position = position_dodge(0.9)) + theme(legend.position="none") + ylab("Change in concerns (in percentage points)") + xlab("") + theme_grey(base_size = 20) 
dev.off()
cairo_ps(file="C:/Userscatve/Box/Migration Colombia/03_manuscript/figures/fig4.eps", 
         width=10, 
         height=5)

#########################################
### PLOT ALL
###################################
lower95_1 = pe1 - se1*1.960
upper95_1 = pe1 + se1*1.960
lower95_2 = pe2 - se2*1.960
upper95_2 = pe2 + se2*1.960
lower95_3 = pe3 - se3*1.960
upper95_3 = pe3 + se3*1.960

pe_per = rbind(pe1,pe2,pe3)
se_per = rbind(se1,se2,se3)
upper95_per = rbind(upper95_1,upper95_2,upper95_3)
lower95_per = rbind(lower95_1,lower95_2,lower95_3)
outcome_per = c("(Lack of) Health","(Lack of) Education", "(Lack of) Security")
results_per = data.frame(pe_per,se_per,lower95_per,upper95_per,outcome_per)
head(results_per)
levels(results_per$outcome_per)

cairo_pdf(file="C:/Users/catve/Box/Dissertation/Chapter 1/03_manuscript/Figures/figA4.pdf", 
          width=14, 
          height=5)
ggplot(results_per, aes(x=outcome_per, y=pe_per)) + 
  geom_hline(yintercept=0, size=1, color = "gray", linetype = 1) + 
  coord_flip(ylim = c(-.04, .04)) + geom_pointrange(aes(ymin=lower95_per,ymax=upper95_per),
                                                    size=0.9,position = position_dodge(0.9)) + 
  ylab("Change in concerns (in percentage points)") + 
  xlab("") + theme_grey(base_size = 25) 
dev.off()

