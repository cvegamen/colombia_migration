
rm(list=ls())

library(dplyr)
library(foreign)
library(xlsx)
library(readxl)
library(geosphere)
library(SoDA)
library(remotes)
library(matrixStats)
library(sp)
library(rgeos)
library(readstata13)
library(Hmisc)
library("readxl")
library(tidyverse)

## set working directory
setwd("~/Dropbox/Migration Colombia/01_data/raw_data/Control for Armed conflict/")

# load data
d01 = read.csv("AsesinatosSelectivos1981-2012.csv")
d02 = read.csv("AtaquesPoblaciones1988-2012.csv")
d03 = read.csv("AtentadosTerroristas1988-2012.csv")
d04 = read.csv("Masacres1980-2012.csv")

colnames(d01)
colnames(d02)
colnames(d03)
colnames(d04)

names(d01)[names(d01) == "A?.o"] <- "year"
names(d02)[names(d02) == "A?.o"] <- "year"
names(d03)[names(d03) == "A?.o"] <- "year"
names(d04)[names(d04) == "A?.o"] <- "year"
names(d01)[names(d01) == "Tipo.de.Implicado..1."] <- "actor"
names(d02)[names(d02) == "Tipo.de.Implicado..2."] <- "actor"
names(d03)[names(d03) == "Tipo.de.Implicado"] <- "actor"
names(d04)[names(d04) == "Tipo.de.Implicado"] <- "actor"
names(d01)[names(d01) == "Nº.V?.ctimas"] <- "victims"
names(d02)[names(d02) == "No.V?.ctimas.Fatales"] <- "victims"
names(d03)[names(d03) == "V?.ctimas.Fatales"] <- "victims"
names(d04)[names(d04) == "Nº.V?.ctimas"] <- "victims"
#subset

d01b=subset(d01, select = c(year,Municipio,actor,victims))
d02b=subset(d02, select = c(year,Municipio,actor,victims))
d03b=subset(d03, select = c(year,Municipio,actor,victims))
d04b=subset(d04, select = c(year,Municipio,actor,victims))

d = rbind(d01b,d02b,d03b,d04b) ##no tengo el mismo numero de obs
