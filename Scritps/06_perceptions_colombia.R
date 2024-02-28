#########################
# Perceptions Venezuela
#########################

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
library(tools)

# set working directory
setwd("~/Dropbox/Migration Colombia/01_data/raw_data")

###############
#LAPOP data
#############

d = read.dta13("Colombia_2014.dta")

# check data
describe(d$year)
describe(d$idnum)
describe(d$municipio)
head(d)

#############################
# Generate variables
#############################

#clean names municipalities
library(tools)
d$municipio=lapply(d$municipio, FUN=tolower)
d$municipio=lapply(d$municipio, FUN=trimws)
d$municipio = as.character(d$municipio)
options(max.print=1000000000)
sort(unlist(lapply(d$municipio, FUN=tolower)))
table(d$municipio)
d$municipio[d$municipio=="19622"] = "rosas"
d$municipio[d$municipio=="97001"] = "mitu"
d$municipio[d$municipio=="70473"] = "morroa"
d$municipio[d$municipio=="armenia_(63001)"] = "armenia"
d$municipio[d$municipio=="bogot<e1>, d.c."] = "bogota"
d$municipio[d$municipio=="bogota, d.c."] = "bogota"
d$municipio[d$municipio=="bogot<c1> d.c."] = "bogota"
d$municipio[d$municipio=="bogot<e1>, d.c"] = "bogota"
d$municipio[d$municipio=="bogot<e1>"] = "bogota"
d$municipio[d$municipio=="caldas_(5129)"] = "caldas"
d$municipio[d$municipio=="caucacia"] = "caucasia"
d$municipio[d$municipio=="cerro san antonio"] = "cerro de san antonio"
d$municipio[d$municipio=="chinchin<e1>"] = "chinchina"
d$municipio[d$municipio=="chiquinquir<e1>"] = "chiquinquira"
d$municipio[d$municipio=="ci<e9>naga de oro"] = "cienaga de oro"
d$municipio[d$municipio=="ci<e9>naga"] = "cienaga"
d$municipio[d$municipio=="caldas_(5129)"] = "caldas"
d$municipio[d$municipio=="c<fa>cuta"] = "cucuta"
d$municipio[d$municipio=="curuman<ed>"] = "curumani"
d$municipio[d$municipio=="facatativ<e1>"] = "facatativa"
d$municipio[d$municipio=="florencia_(18001)"] = "florencia"
d$municipio[d$municipio=="guamal_(50318)"] = "guamal"
d$municipio[d$municipio=="ibagu<e9>"] = "ibague"
d$municipio[d$municipio=="la uni<f3>n_(5400)"] = "la union"
d$municipio[d$municipio=="la uni<d3>n"] = "la union"
d$municipio[d$municipio=="la uni<f3>n"] = "la union"
d$municipio[d$municipio=="la uni?n"] = "la union"
d$municipio[d$municipio=="magangu<e9>"] = "magangue"
d$municipio[d$municipio=="manat<ed>"] = "manati"
d$municipio[d$municipio=="medell<ed>n"] = "medellin"
d$municipio[d$municipio=="monter<ed>a"] = "monteria"
d$municipio[d$municipio=="moniquir<e1>"] = "moniquira"
d$municipio[d$municipio=="montel<ed>bano"] = "montelibano"
d$municipio[d$municipio=="popay<e1>n"] = "popayan"
d$municipio[d$municipio=="puerto as<ed>s"] = "puerto asis"
d$municipio[d$municipio=="purac<e9>"] = "purace"
d$municipio[d$municipio=="quinch<ed>a"] = "quinchia"
d$municipio[d$municipio=="riofr<ed>o"] = "riofrio"
d$municipio[d$municipio=="rionegro_(68615)"] = "rionegro"
d$municipio[d$municipio=="sampu<e9>s"] = "sampues"
d$municipio[d$municipio=="sandon<e1>"] = "sandona"
d$municipio[d$municipio=="san martin_(20770)"] = "san martin"
d$municipio[d$municipio=="san mart<ed>n"] = "san martin"
d$municipio[d$municipio=="san vicente de chucur<ed>"] = "san vicente de chucuri"
d$municipio[d$municipio=="santuario_(66687)"] = "santuario"
d$municipio[d$municipio=="sutamarch<e1>n"] = "sutamarchan"
d$municipio[d$municipio=="urab<e1>"] = "uraba"
d$municipio[d$municipio=="viot<e1>"] = "viota"
d$municipio[d$municipio=="san andres de tumaco"] = "tumaco"

table(d$municipio)
sort(unique(d$municipio))

#distances municipalities

c = read.csv("coordenadas.csv", stringsAsFactors=FALSE, sep=";")

d <- merge(d , c ,by="municipio")

# distances pasos fronterizos 
d$lat_paso1 = 7.088729 
d$lon_paso1 = -70.740253 
d$lat_paso2 = 6.185968
d$lon_paso2 = -67.483876
d$lat_paso3 = 3.865200
d$lon_paso3 = -67.926498 
d$lat_paso4 = 11.3616666 
d$lon_paso4 = -72.135384
d$lat_paso5 = 7.817803 
d$lon_paso5 = -72.450352
d$lat_paso6 = 8.364482
d$lon_paso6 = -72.405688
d$lat_paso7 = 7.917146
d$lon_paso7 = -72.462511

library(geosphere)
library(SoDA)
library(remotes)
library(matrixStats)
library(sp)
library(rgeos)

# paso 1
d$lat_paso1 = 7.088729 
d$lon_paso1 = -70.740253 
lon = d$lon
lat = d$lat
lat_paso1 = d$lat_paso1
lon_paso1 = d$lon_paso1
d$distance_border1 = (geoDist(lat, lon, lat_paso1, lon_paso1, NAOK = TRUE, DUP = TRUE))/1000

# paso 2
d$lat_paso2 = 3.865200 
d$lon_paso2 = -67.483876
lon = d$lon
lat = d$lat
lat_paso2 = d$lat_paso2
lon_paso2 = d$lon_paso2
d$distance_border2 = (geoDist(lat, lon, lat_paso2, lon_paso2, NAOK = TRUE, DUP = TRUE))/1000

# paso 3
d$lat_paso3 = 3.865200
d$lon_paso3 = -67.926498 
lon = d$lon
lat = d$lat
lat_paso3 = d$lat_paso3
lon_paso3 = d$lon_paso3
d$distance_border3 = (geoDist(lat, lon, lat_paso3, lon_paso3, NAOK = TRUE, DUP = TRUE))/1000

# paso 4
d$lat_paso4 = 11.3616666
d$lon_paso4 = -72.135384 
lon = d$lon
lat = d$lat
lat_paso4 = d$lat_paso4
lon_paso4 = d$lon_paso4
d$distance_border4 = (geoDist(lat, lon, lat_paso4, lon_paso4, NAOK = TRUE, DUP = TRUE))/1000

# paso 5 
d$lat_paso4 = 7.817803
d$lon_paso4 = -72.450352
lon = d$lon
lat = d$lat
lat_paso5 = d$lat_paso5
lon_paso5 = d$lon_paso5
d$distance_border5 = (geoDist(lat, lon, lat_paso5, lon_paso5, NAOK = TRUE, DUP = TRUE))/1000

#paso 6
d$lat_paso4 = 8.364482
d$lon_paso4 = -72.450352
lon = d$lon
lat = d$lat
lat_paso6 = d$lat_paso6
lon_paso6 = d$lon_paso6
d$distance_border6 = (geoDist(lat, lon, lat_paso6, lon_paso6, NAOK = TRUE, DUP = TRUE))/1000

#paso 7
d$lat_paso7 = 8.364482
d$lon_paso7 = -72.450352
lon = d$lon
lat = d$lat
lat_paso7 = d$lat_paso7
lon_paso7 = d$lon_paso7
d$distance_border7 = (geoDist(lat, lon, lat_paso7, lon_paso7, NAOK = TRUE, DUP = TRUE))/1000

# treatment indicator
distances_matrix = cbind(d$distance_border1,d$distance_border2,d$distance_border3,
                         d$distance_border4,d$distance_border5,d$distance_border6,d$distance_border7)
d$min_distance <- rowMins(distances_matrix, na.rm = TRUE)
describe(d$min_distance)

# name closest border
d$min_distance_bordername <- NA
d$min_distance_bordername[d$min_distance == d$distance_border1] = "border 1"
d$min_distance_bordername[d$min_distance == d$distance_border2] = "border 2"
d$min_distance_bordername[d$min_distance == d$distance_border3] = "border 3"
d$min_distance_bordername[d$min_distance == d$distance_border4] = "border 4"
d$min_distance_bordername[d$min_distance == d$distance_border5] = "border 5"
d$min_distance_bordername[d$min_distance == d$distance_border6] = "border 6"
d$min_distance_bordername[d$min_distance == d$distance_border7] = "border 7"
table(d$min_distance_bordername, exclude = NULL)

# gen distance roads
table(d$municipio,d$min_distance_bordername)
d$min_distance_roads = NA
d$min_distance_roads[d$municipio=="barrancabermeja"] = 
d$min_distance_roads[d$municipio=="barranquilla"] = 358

#education 
table(d$ed, exclude = NULL)
table(d$ed)
summary(d$ed)
d$edu=NA
d$edu[d$ed=="ninguno"]=0
d$edu[d$ed=="Ninguno"]=0
d$edu[d$ed=="0"]=0
d$edu[d$ed=="primaria 1"]=1
d$edu[d$ed=="1"]=1
d$edu[d$ed=="primaria 2"]=2
d$edu[d$ed=="2"]=2
d$edu[d$ed=="primaria 3"]=3
d$edu[d$ed=="3"]=3
d$edu[d$ed=="primaria 4"]=4
d$edu[d$ed=="4"]=4
d$edu[d$ed=="primaria 5"]=5
d$edu[d$ed=="5"]=5
d$edu[d$ed=="Primaria 1"]=1
d$edu[d$ed=="Primaria 2"]=2
d$edu[d$ed=="Primaria 3"]=3
d$edu[d$ed=="Primaria 4"]=4
d$edu[d$ed=="Primaria 5"]=5
d$edu[d$ed=="secundaria 1"]=6
d$edu[d$ed=="6"]=6
d$edu[d$ed=="secundaria 2"]=7
d$edu[d$ed=="7"]=7
d$edu[d$ed=="secundaria 3"]=8
d$edu[d$ed=="8"]=8
d$edu[d$ed=="secundaria 4"]=9
d$edu[d$ed=="9"]=9
d$edu[d$ed=="secundaria 5"]=10
d$edu[d$ed=="10"]=10
d$edu[d$ed=="secundaria 6"]=11
d$edu[d$ed=="11"]=11
d$edu[d$ed=="Secundaria 1"]=6
d$edu[d$ed=="Secundaria 2"]=7
d$edu[d$ed=="Secundaria 3"]=8
d$edu[d$ed=="Secundaria 4"]=9
d$edu[d$ed=="Secundaria 5"]=10
d$edu[d$ed=="Secundaria 6"]=11
d$edu[d$ed=="Universitaria 1 o Superior no universit"]=12
d$edu[d$ed=="12"]=12
d$edu[d$ed=="universitaria 1 o superior no universitaria 1"]=12
d$edu[d$ed=="Universitaria 1 o Superior no universitaria 1"]=12
d$edu[d$ed=="Universitaria 1 o Superior no universitaria"]=12
d$edu[d$ed=="Universitaria 2 o Superior no universit"]=13
d$edu[d$ed=="13"]=13
d$edu[d$ed=="universitaria 2 o superior no universitaria 2"]=13
d$edu[d$ed=="Universitaria 2 o Superior no universitaria 2"]=13
d$edu[d$ed=="Universitaria 3 o Superior no universit"]=14
d$edu[d$ed=="14"]=14
d$edu[d$ed=="universitaria 3 o superior no universitaria 3"]=14
d$edu[d$ed=="Universitaria 3 o Superior no universitaria 3"]=14
d$edu[d$ed=="Universitaria 4 o Superior no universit"]=15
d$edu[d$ed=="15"]=15
d$edu[d$ed=="universitaria 4 o superior no universitaria 4"]=15
d$edu[d$ed=="Universitaria 4 o Superior no universitaria 4"]=15
d$edu[d$ed=="Universitaria 5"]=16
d$edu[d$ed=="universitaria 5"]=16
d$edu[d$ed=="16"]=16
d$edu[d$ed=="universitaria 6"]=17
d$edu[d$ed=="17"]=17
d$edu[d$ed=="Universitaria 6 o mas"]=18
d$edu[d$ed=="18"]=18
table(d$edu, exclude = NULL)

#gender
table(d$q1)
d$female = 0
d$female[d$q1=="Mujer"]=1
d$female[d$q1=="mujer"]=1
d$female[d$q1=="Female"]=1
table(d$female)

#age
table(d$q2)
d$age = d$q2
table(d$age, exclude = NULL)

#size
d$tamano = as.character(d$tamano)
table(d$tamano)
d$size = NA
d$size[d$tamano=="<c1>rea rural"] = 0
d$size[d$tamano=="Rural Area"] = 0
d$size[d$tamano=="ciudad peque<f1>a"] = 1
d$size[d$tamano=="ciudad pequena"] = 1
d$size[d$tamano=="Ciudad peque<f1>a"] = 1
d$size[d$tamano=="Ciudad pequena"] = 1
d$size[d$tamano=="Small City"] = 1
d$size[d$tamano=="Medium City"] = 2
d$size[d$tamano=="ciudad mediana"] = 2
d$size[d$tamano=="Ciudad mediana"] = 2
d$size[d$tamano=="Large City"] = 3
d$size[d$tamano=="Ciudad grande"] = 3
d$size[d$tamano=="ciudad grande"] = 3
d$size[d$tamano=="Capital nacional (<e1>rea metropolitana)"] = 4
d$size[d$tamano=="capital nacional (area metropolitana)"] = 4
d$size[d$tamano=="capital nacional (<e1>rea metropolitana)"] = 4
d$size[d$tamano=="Capital nacional (<e1>rea metropolitana)"] = 4
d$size[d$tamano=="Capital Nacional (<e1>rea metropolitana)"] = 4
d$size[d$tamano=="Capital nacional (area metropolitana)"] = 4
d$size[d$tamano=="National Capital (Metropolitan area)"] = 4

# outcome
d$per_ven = 0
d$per_ven[d$for5=="Venezuela"] = 1
table(d$per_ven)

####################
#Armed conflict data
####################

d01 = read.csv("AsesinatosSelectivos1981-2012.csv")
d02 = read.csv("AtaquesPoblaciones1988-2012.csv")
d03 = read.csv("AtentadosTerroristas1988-2012.csv")
d04 = read.csv("Masacres1980-2012.csv")

colnames(d01)
colnames(d02)
colnames(d03)
colnames(d04)

names(d01)[names(d01) == "Ano"] = "year"
names(d02)[names(d02) == "Ano"] = "year"
names(d03)[names(d03) == "Ano"] = "year"
names(d04)[names(d04) == "Ano"] = "year"
names(d01)[names(d01) == "Tipo.de.Implicado..1."] = "actor"
names(d02)[names(d02) == "Tipo.de.Implicado..2."] = "actor"
names(d03)[names(d03) == "Tipo.de.Implicado"] = "actor"
names(d04)[names(d04) == "Tipo.de.Implicado"] = "actor"
names(d01)[names(d01) == "Nº.Victimas"] = "victims"
names(d02)[names(d02) == "No.Victimas.Fatales"] = "victims"
names(d03)[names(d03) == "Victimas.Fatales"] = "victims"
names(d04)[names(d04) == "Nº.Victimas"] = "victims"
names(d01)[names(d01) == "Municipio"] = "municipio"
names(d02)[names(d02) == "Municipio"] = "municipio"
names(d03)[names(d03) == "Municipio"] = "municipio"
names(d04)[names(d04) == "Municipio"] = "municipio"

###########
#subset
###########

d01b = subset(d01, select = c(year,municipio,actor,victims))
d02b = subset(d02, select = c(year,municipio,actor,victims))
d03b = subset(d03, select = c(year,municipio,actor,victims))
d04b = subset(d04, select = c(year,municipio,actor,victims))

dd = rbind(d01b,d02b,d03b,d04b)

############################
#Clean municipality names
############################

dd$municipio
dd$municipio=lapply(dd$municipio, FUN=tolower)
dd$municipio=lapply(dd$municipio, FUN=trimws)
dd$municipio = as.character(dd$municipio)
dd$municipio[dd$municipio=="bogota d.c"]="bogota"
dd$municipio[dd$municipio=="bogota d.c."]="bogota"
dd$victims = as.numeric(dd$victims)

# subset 2012
table(dd$year)
dd2 = dd[dd$year>2009,]
table(dd2$municipio, exclude = NULL)
dd2 = dd2[!is.na(dd2$municipio),]
dd2 = dd2[dd2$municipio!="",]

num_victims = as.numeric(tapply(dd2$victims,dd2$municipio,sum, na.rm=TRUE))
municipio = unique(dd2$municipio)
v = data.frame(municipio, num_victims)

# loop
d$num_victims = 0

for (i in v$municipio) {
  
  d$num_victims[d$municipio == i] = (v$num_victims[v$municipio==i])
  
}

tapply(d$num_victims,d$municipio,mean)
tapply(v$num_victims,v$municipio,mean)

# drop missing
table(d$edu, exclude = NULL)
d = d[!is.na(d$edu),]

table(d$age, exclude = NULL)
d = d[!is.na(d$age),]

table(d$female, exclude = NULL)

d2014 = d[d$year==2014,]

#####################
# Analysis
#####################

# check
describe(d$per_ven)
describe(d$min_distance)
describe(d$edu)
describe(d$age)
describe(d$female)
describe(d$size)
describe(d$num_victims)

m1 <- felm(d$per_ven ~ d$min_distance + d$edu + d$age + d$female + d$size + d$num_victims| 0 | 0 | d$municipio, keepX=TRUE)
summary(m1)
pe1 = m1$beta[2]*100
se1 = as.numeric(m1$cse[2]*100)
pe1
se1

