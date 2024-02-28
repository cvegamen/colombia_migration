########################
# Prepare Colombia
########################

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
#d08 = read.dta13("Colombia_2008.dta")
#d09 = read.dta13("Colombia_2009.dta")
#d10 = read.dta13("Colombia_2010.dta")
#d11 = read.dta13("Colombia_2011.dta")
#d12 = read.dta13("Colombia_2012.dta")
d13 = read.dta13("Colombia_2013.dta")
d14 = read.dta13("Colombia_2014.dta")
d16 = read.dta13("Colombia_2016.dta")
d18 = read.dta13("Colombia_2018.dta")

#############################
# Prepare data
#############################

# variables
#d08$year = 2008
#d09$year = 2009
#d10$year = 2010
#d11$year = 2011
#d12$year = 2012
d13$year = 2013
d14$year = 2014
d16$year = 2016
d18$year = 2018

#d09$municipio = d09$upm
#d10$municipio = d10$colupm
#d11$municipio = d11$codmunicipio

# subset

#d08b=subset(d08, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4))
#d09b=subset(d09, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new))
#d10b=subset(d10, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new))
#d11b=subset(d11, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new))
#d12b=subset(d12, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new))
d13b=subset(d13, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new))
d14b=subset(d14, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new)) 
d16b=subset(d16, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new))
d18b=subset(d18, select = c(year,idnum,municipio,ur,tamano,q1,q2,ed,a4,q10new))

d = rbind(d13b,d14b,d16b,d18b)

# check data
describe(d$year)
describe(d$idnum)
describe(d$municipio)
head(d)

#############################
# Generate variables
#############################

library(tools)
d$a4=lapply(d$a4, FUN=tolower)
d$a4=lapply(d$a4, FUN=trimws)
d$a4 = as.character(d$a4)
table(d$a4,exclude = NULL)

#clean outcome variables
d$a4[d$a4=="agua, falta de"] = "water, lack of"
d$a4[d$a4=="caminos/v<ed>as en mal estado"] = "roads in poor condition"
d$a4[d$a4=="caminos/vias en mal estado"] = "roads in poor condition"
d$a4[d$a4=="conflicto armado"] = "armed conflict"
d$a4[d$a4=="conflicto con pa<ed>ses vecinos"]="conflict with neighbours"
d$a4[d$a4=="corrupci<f3>n"]="corruption"
d$a4[d$a4=="corrupcion"]="corruption"
d$a4[d$a4=="cr<e9>dito, falta de"]="credit, lack of"
d$a4[d$a4=="delincuencia, crimen"]="crime"
d$a4[d$a4=="derechos humanos, violaciones de"]="human rights, violations of"
d$a4[d$a4=="desigualdad"]="inequalty"
d$a4[d$a4=="desnutrici<f3>n"]="malnutrituion"
d$a4[d$a4=="desnutricion"]="malnutrituion"
d$a4[d$a4=="desplazamiento forzado"]="forced displacement"
d$a4[d$a4=="forced displacement of persons"]="forced displacement"
d$a4[d$a4=="deuda externa"]="external debt"
d$a4[d$a4=="discriminaci<f3>n"]="discrimination"
d$a4[d$a4=="discriminacion"]="discrimination"
d$a4[d$a4=="drogadicci<f3>n"]="drogaddiction"
d$a4[d$a4=="drogas, consumo de drogadicci<f3>n"]="drogaddiction"
d$a4[d$a4=="drogadiccion"]="drogaddiction"
d$a4[d$a4=="drug addiction, consumption of drugs"]="drogaddiction"
d$a4[d$a4=="drogas, consumo de, drogadicci<f3>n"]="drogaddiction"
d$a4[d$a4=="econom<ed>a, problemas con, crisis de"]="economy, problems with, crisis of"
d$a4[d$a4=="economia, problemas con, crisis de"]="economy, problems with, crisis of"
d$a4[d$a4=="econom<ed>a, problemas con, crisis de"]="economy, problems with, crisis of"
d$a4[d$a4=="educaci<f3>n, falta de, mala calidad"]="education, lack of, poor quality"
d$a4[d$a4=="educacion, falta de, mala calidad"]="education, lack of, poor quality"
d$a4[d$a4=="electricidad, falta de"]="electricity, lack of"
d$a4[d$a4=="explosi<f3>n demogr<e1>fica"]="population explosion"
d$a4[d$a4=="guerra contra el terrorismo"]="war against terrorism"
d$a4[d$a4=="guerra contra terrorismo"]="war against terrorism"
d$a4[d$a4=="impunidad"]="impunity"
d$a4[d$a4=="inflaci<f3>n, altos precios"]="inflation, high prices"
d$a4[d$a4=="inflacion, altos precios"]="inflation, high prices"
d$a4[d$a4=="la guerrilla"]="guerrillas"
d$a4[d$a4=="la paz, proceso de paz, acuerdos de paz"]="the peace, peace process, peace agreements"
d$a4[d$a4=="los pol<ed>ticos"]="the politicians"
d$a4[d$a4=="mal gobierno"]="bad government"
d$a4[d$a4=="medio ambiente"]="environment"
d$a4[d$a4=="migraci<f3>n"]="migration"
d$a4[d$a4=="narcotr<e1>fico"]="drug trafficking"
d$a4[d$a4=="narcotrafico"]="drug trafficking"
d$a4[d$a4=="nr"]="NA"
d$a4[d$a4=="other"]="NA"
d$a4[d$a4=="otro"]="NA"
d$a4[d$a4=="pandillas"]="gangs"
d$a4[d$a4=="desempleo/falta de empleo"]="unemployment"
d$a4[d$a4=="pobreza"]="poverty"
d$a4[d$a4=="protestas populares (huelgas, cierre de carreteras, paros, etc.)"]="popular protests (strikes, blocking roads, work stoppages, etc.)"
d$a4[d$a4=="protestas: huelgas, paros, cierre v<ed>as"]="popular protests (strikes, blocking roads, work stoppages, etc.)"
d$a4[d$a4=="salud, falta de servicio"]="health services, lack of"
d$a4[d$a4=="secuestro"]="kidnappings"
d$a4[d$a4=="pol<ed>ticos, los"]="the politicians"
d$a4[d$a4=="politicos, los"]="the politicians"
d$a4[d$a4=="seguridad (falta de)"]="security (lack of)"
d$a4[d$a4=="terrorismo"]="terrorism"
d$a4[d$a4=="war against terrorism"]="war against terrorism"
d$a4[d$a4=="vivienda"]="housing"
d$a4[d$a4=="tierra para cultivar, falta de"]="land to farm, lack of"
d$a4[d$a4=="violencia"]="violence"
d$a4[d$a4=="transporte, problemas con el"]="transportation, problems of"
d$a4[d$a4=="pol<ed>tica agraria/campesina"]="agrarian/peasant politics"
d$a4[d$a4=="las bacrim/paramilitares"]="bacrim/paramilitaries"
d$a4[d$a4=="credito, falta de"]="credit, lack of"
unique(d$a4)
table(d$a4)

#water, lack of
d$outcome_lack_water=0
d$outcome_lack_water[d$a4=="water, lack of"]=1
table(d$outcome_lack_water, exclude = NULL)

#outcome roads poor condition
d$outcome_roads=0
d$outcome_roads[d$a4=="roads in poor condition"]=1
table(d$outcome_roads, exclude = NULL)

#outcome armed conflict
d$outcome_conflict=0
d$outcome_conflict[d$a4=="armed conflict"]=1
table(d$outcome_conflict, exclude = NULL)

#outcome corruption
d$outcome_corruption=0
d$outcome_corruption[d$a4=="corruption"]=1
table(d$outcome_corruption, exclude = NULL)

#outcome credit, lack of
d$outcome_credit=0
d$outcome_credit[d$a4=="credit, lack of"]=1
table(d$outcome_credit, exclude = NULL)

# outcome crime 
d$outcome_crime = 0
d$outcome_crime[d$a4=="crime"]=1
table(d$outcome_crime, exclude = NULL)

#outcome human rights violations
d$outcome_human_rights = 0
d$outcome_human_rights[d$a4=="human rights, violations of"]=1
table(d$outcome_human_rights, exclude = NULL)

# outcome unemployment
d$outcome_unemployment=0
d$outcome_unemployment[d$a4=="unemployment"]=1
table(d$outcome_unemployment, exclude = NULL)

#outcome inequality 
d$outcome_inequality=0
d$outcome_inequality[d$a4=="inequality"]=1
table(d$outcome_inequality, exclude = NULL)

#outcome malnutrituion 
d$outcome_malnutrituion=0
d$outcome_malnutrituion[d$a4=="malnutrituion"]=1
table(d$outcome_malnutrituio, exclude = NULL)

#outcome forced displacement
d$outcome_forced_displacement=0
d$outcome_forced_displacement[d$a4=="forced displacement"]=1
table(d$outcome_forced_displacement, exclude = NULL)

#outcome external debt
d$outcome_external_debt=0
d$outcome_external_debt[d$a4=="external debt"]=1
table(d$outcome_external_debt, exclude = NULL)

#outcome discrimination 
d$outcome_discrimination=0
d$outcome_discrimination[d$a4=="discrimination"]=1
table(d$outcome_discrimination, exclude = NULL)

#outcome drogaddiction
d$outcome_drogaddiction=0
d$outcome_drogaddiction[d$a4=="drogaddiction"]=1
table(d$outcome_drogaddiction, exclude = NULL)

#outcome economy, problems of
d$outcome_economy_problems=0
d$outcome_economy_problems[d$a4=="economy, problems with, crisis of"]=1
table(d$outcome_economy_problems, exclude = NULL)

#outcome education, lack of
d$outcome_lack_education=0
d$outcome_lack_education[d$a4=="education, lack of, poor quality"]=1
table(d$outcome_lack_education, exclude = NULL)

#electricity, lack of
d$outcome_lack_electricity=0
d$outcome_lack_electricity[d$a4=="electricity, lack of"]=1
table(d$outcome_lack_electricity, exclude = NULL)

#outcome demographic explosion 
d$outcome_demographic_explosion=0
d$outcome_demographic_explosion[d$a4=="population explosion"]=1
table(d$outcome_demographic_explosion, exclude = NULL)

#outcome war against terrorism
d$outcome_war_terrorism=0
d$outcome_war_terrorism[d$a4=="war against terrorism"]=1
table(d$outcome_war_terrorism, exclude = NULL)

#outcome impunity
d$outcome_impunity=0
d$outcome_impunity[d$a4=="impunity"]=1
table(d$outcome_impunity, exclude = NULL)

#outcome inflation, high prices
d$outcome_inflation=0
d$outcome_inflation[d$a4=="inflation, high prices"]=1
table(d$outcome_inflation, exclude = NULL)

#bad government
d$outcome_bad_government=0
d$outcome_bad_government[d$a4=="bad government"]=1
table(d$outcome_bad_government, exclude = NULL)

#outcome environment
d$outcome_environment=0
d$outcome_environment[d$a4=="environment"]=1
table(d$outcome_bad_government, exclude = NULL)

#outcome migration
d$outcome_migration=0
d$outcome_migration[d$a4=="migration"]=1
table(d$outcome_migration, exclude = NULL)

#outcome drug trafficking
d$outcome_drug_trafficking=0
d$outcome_drug_trafficking[d$a4=="drug trafficking"]=1
table(d$outcome_drug_trafficking, exclude = NULL)

#outcome gangs
d$outcome_gangs=0
d$outcome_gangs[d$a4=="gangs"]=1
table(d$outcome_gangs, exclude = NULL)

#outcome poverty
d$outcome_poverty=0
d$outcome_poverty[d$a4=="poverty"]=1
table(d$outcome_poverty, exclude = NULL)

#outcome politicians
d$outcome_politicians=0
d$outcome_politicians[d$a4=="politicians"]=1
d$outcome_politicians[d$a4=="the politicians"]=1
table(d$outcome_politicians, exclude = NULL)

#outcome popular protests (strikes, blocking roads, work stoppages, etc.)
d$outcome_popular_protests=0
d$outcome_popular_protests[d$a4=="popular protests (strikes, blocking roads, work stoppages, etc.)"]=1
table(d$outcome_popular_protests, exclude = NULL)

#outcome health services, lack of
d$outcome_health_lack=0
d$outcome_health_lack[d$a4=="health services, lack of"]=1
table(d$outcome_health_lack, exclude = NULL)

#outcome kidnappings
d$outcome_kidnappings=0
d$outcome_kidnappings[d$a4=="kidnappings"]=1
table(d$outcome_kidnappings, exclude = NULL)

#outcome security, lack of
d$outcome_lack_security=0
d$outcome_lack_security[d$a4=="security (lack of)"]=1
table(d$outcome_lack_security, exclude = NULL)

#outcome terrorism
d$outcome_terrorism=0
d$outcome_terrorism[d$a4=="terrorism"]=1
table(d$outcome_terrorism, exclude = NULL)

#outcome land to farm, lack of
d$outcome_land_lack=0
d$outcome_land_lack[d$a4=="land to farm, lack of"]=1
table(d$outcome_land_lack, exclude = NULL)

#outcome transportation, problems of
d$outcome_lack_transport=0
d$outcome_lack_transport[d$a4=="transportation, problems of"]=1
table(d$outcome_lack_transport, exclude = NULL)

#outcome violence
d$outcome_violence=0
d$outcome_violence[d$a4=="violence"]=1
table(d$outcome_violence, exclude = NULL)

#outcome housing
d$outcome_lack_housing=0
d$outcome_lack_housing[d$a4=="housing"]=1
table(d$outcome_lack_housing, exclude = NULL)

#outcomes that are not in the questionarie table 
#peace process
d$outcome_peace_process=0
d$outcome_peace_process[d$a4=="the peace, peace process, peace agreements"]=1
table(d$outcome_peace_process, exclude = NULL)

#guerrillas
d$outcome_guerrillas=0
d$outcome_guerrillas[d$a4=="guerrillas"]=1
table(d$outcome_guerrillas, exclude = NULL)

#las bacrim
d$outcome_bacrim=0
d$outcome_bacrim[d$a4=="las bacrim"]=1
table(d$outcome_bacrim, exclude = NULL)

#roads in poor condition 
d$outcome_roads=0
d$outcome_roads[d$a4=="roads in poor condition"]=1
table(d$outcome_roads, exclude = NULL)

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
d$min_distance_roads[d$municipio=="barrancabermeja"] = 309
d$min_distance_roads[d$municipio=="barranquilla"] = 357
d$min_distance_roads[d$municipio=="bogota"] = 558
d$min_distance_roads[d$municipio=="bucaramanga"] = 198
d$min_distance_roads[d$municipio=="buenaventura"] = 157
d$min_distance_roads[d$municipio=="cali"] = 1009
d$min_distance_roads[d$municipio=="cartagena"] = 480
d$min_distance_roads[d$municipio=="caucasia"] = 783
d$min_distance_roads[d$municipio=="chinchina"] = 895
d$min_distance_roads[d$municipio=="chiquinquira"] = 499
d$min_distance_roads[d$municipio=="cienaga"] = 288
d$min_distance_roads[d$municipio=="cienaga de oro"] = 833
d$min_distance_roads[d$municipio=="cucuta"] = 10.2
d$min_distance_roads[d$municipio=="cumbal"] = 1441
d$min_distance_roads[d$municipio=="curumani"] = 377
d$min_distance_roads[d$municipio=="dosquebradas"] = 748
d$min_distance_roads[d$municipio=="el copey"] = 391
d$min_distance_roads[d$municipio=="facatativa"] = 589
d$min_distance_roads[d$municipio=="filadelfia"] = 747
d$min_distance_roads[d$municipio=="florencia"] = 1102
d$min_distance_roads[d$municipio=="florida"] = 973
d$min_distance_roads[d$municipio=="ginebra"] = 923
d$min_distance_roads[d$municipio=="girardot"] = 837
d$min_distance_roads[d$municipio=="ibague"] = 758
d$min_distance_roads[d$municipio=="ipiales"] = 1419
d$min_distance_roads[d$municipio=="la union"] = 505
d$min_distance_roads[d$municipio=="magangue"] = 508
d$min_distance_roads[d$municipio=="manati"] = 422
d$min_distance_roads[d$municipio=="manizales"] = 839
d$min_distance_roads[d$municipio=="marinilla"] = 765
d$min_distance_roads[d$municipio=="medellin"] = 720
d$min_distance_roads[d$municipio=="moniquira"] = 416
d$min_distance_roads[d$municipio=="montelibano"] = 777
d$min_distance_roads[d$municipio=="monteria"] = 714
d$min_distance_roads[d$municipio=="neiva"] = 1002
d$min_distance_roads[d$municipio=="nimaima"] = 641
d$min_distance_roads[d$municipio=="popayan"] = 1139
d$min_distance_roads[d$municipio=="puerto asis"] = 1273
d$min_distance_roads[d$municipio=="puerto lleras"] = 808
d$min_distance_roads[d$municipio=="purace"] = 1098
d$min_distance_roads[d$municipio=="sampues"] = 610
d$min_distance_roads[d$municipio=="san martin"] = 735
d$min_distance_roads[d$municipio=="san pedro de uraba"] = 830
d$min_distance_roads[d$municipio=="san vicente de chucuri"] = 280
d$min_distance_roads[d$municipio=="santa marta"] = 264
d$min_distance_roads[d$municipio=="santa rosa de osos"] = 598
d$min_distance_roads[d$municipio=="sincelejo"] = 596
d$min_distance_roads[d$municipio=="tumaco"] = 1614
d$min_distance_roads[d$municipio=="uraba"] = 798
d$min_distance_roads[d$municipio=="villavicencio"] = 678
d$min_distance_roads[d$municipio=="viota"] = 703
table(d$min_distance_roads)
describe(d$min_distance_roads)
describe(d$min_distance)
cor(d$min_distance_roads,d$min_distance)

# post
table(d$year)
d$post = 0
d$post[d$year > 2014] = 1
table(d$post)

d$lat
d$lon

# covariates 

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
#d$tamano=lapply(d$tamano, FUN=tolower)
#d$tamano=lapply(d$tamano, FUN=trimws)
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

describe(d$size)

# income
table(d$q10new)
d$income = NA
d$income[d$q10new == "[00] Ning<fa>n ingreso"] = 0
d$income[d$q10new == "[01] Menos de 90.000"] = 1
d$income[d$q10new == "[02] Entre 90.000 - 180.000"] = 2
d$income[d$q10new == "[03] Entre 180.000 - 270.000"] = 3
d$income[d$q10new == "[04] Entre 270.000 - 360.000"] = 4
d$income[d$q10new == "[05] Entre 360.000 - 450.000"] = 5
d$income[d$q10new == "[06] Entre 450.000 - 540.000"] = 6
d$income[d$q10new == "[07] Entre 540.000 - 630.000"] = 7
d$income[d$q10new == "[08] Entre 630.000 - 710.000"] = 8
d$income[d$q10new == "[09] Entre 710.000 - 800.000"] = 9
d$income[d$q10new == "[10] Entre 800.000 - 940.000"] = 10
d$income[d$q10new == "[11] Entre 940.000 - 1.100.000"] = 11
d$income[d$q10new == "[12] Entre 1.100.000 - 1.600.00"] = 12
d$income[d$q10new == "[13] Entre 1.600.000 - 2.100.000"] = 13
d$income[d$q10new == "[14] Entre 2.100.000 - 3.200.000"] = 14
d$income[d$q10new == "[15] Entre 3.200.000 - 4.300.000"] = 15
d$income[d$q10new == "[16] M<e1>s de 4.300.000"] = 16
d$income[d$q10new == "ns"] = NA
d$income[d$q10new == "nr"] = NA
d$income[d$q10new == "Ning<fa>n ingreso"] =0 
d$income[d$q10new == "Menos de 160.000"] = 1
d$income[d$q10new == "Entre 160.000 <96> 250.000"] = 2
d$income[d$q10new == "Entre 250.001 <96> 340.000"] = 3
d$income[d$q10new == "Entre 340.001 <96> 420.000"] = 4
d$income[d$q10new == "Entre 420.001 <96> 480.000"] = 5
d$income[d$q10new == "Entre 480.001 <96> 540.000"] = 6
d$income[d$q10new == "Entre 540.001 <96> 590.000"] = 7
d$income[d$q10new == "Entre 590.001 <96> 650.000"] = 8
d$income[d$q10new == "Entre 650.001 <96> 720.000"] = 9
d$income[d$q10new == "Entre 720.001 <96> 810.000"] = 10
d$income[d$q10new == "Entre 810.001 <96> 960.00"] = 11
d$income[d$q10new == "Entre 960.001 <96> 1.100.000"] = 12
d$income[d$q10new == "Entre 1.100.001 <96> 1.400.000"] = 13
d$income[d$q10new == "Entre 1.400.001 <96> 1.900.000"] = 14
d$income[d$q10new == "Entre 1.900.001 <96> 3.200.000"] =  15
d$income[d$q10new == "M<e1>s de 3.200.000"] = 16
d$income[d$q10new == "No Sabe"] = NA 
d$income[d$q10new == "No Responde"] = NA
d$income[d$q10new == "No income"] = 0 
d$income[d$q10new == "Less than 225,000"] = 1 
d$income[d$q10new == "Between 225,001 and 325,000"] = 2 
d$income[d$q10new == "Between 325,001 and 425,00"] = 3
d$income[d$q10new == "Between 425,001 and 545,000"] = 4
d$income[d$q10new == "Between 545,001 and 620,000"] = 5
d$income[d$q10new == "Between 620,001 and 660,000"] = 6
d$income[d$q10new == "Between 660,001 and 700,000"] = 7
d$income[d$q10new == "Between 700,001 and 750,000"] = 8
d$income[d$q10new == "Between 750,001 and 840,000"] = 9
d$income[d$q10new == "Between 840,001 and 980,000"] = 10
d$income[d$q10new == "Between 980,001 and 1,200,000"] = 11
d$income[d$q10new == "Between 1,200,001 and 1,300,000"] = 12
d$income[d$q10new == "Between 1,300,001 and 1,600,000"] = 13
d$income[d$q10new == "Between 1,600,001 and 2,000,000"] = 14
d$income[d$q10new == "Between 2,000,001 and 3,250,000"] = 15
d$income[d$q10new == "More than 3,250,000"] =  16
d$income[d$q10new == "Don't Know"] = NA
d$income[d$q10new == "No Response"] = NA
d$income[d$q10new == "Menos de 205,000"] = 1
d$income[d$q10new == "Entre 205,000 y 325,000"] = 2
d$income[d$q10new == "Entre 325,001 y 440,000"] = 3
d$income[d$q10new == "Entre 440,001 y 565,000"] = 4
d$income[d$q10new == "Entre 565,001 y 650,000"] = 5
d$income[d$q10new == "Entre 650,001 y 710,000"] = 6
d$income[d$q10new == "Entre 710,001 y 750,000"] = 7
d$income[d$q10new == "Entre 750,001 y 810,000"] = 8
d$income[d$q10new == "Entre 810,001 y 915,000"] = 9
d$income[d$q10new == "Entre 915,001 y 1,000,000"] = 10
d$income[d$q10new == "Entre 1,000,001 y 1,250,000"] = 11
d$income[d$q10new == "Entre 1,250,001 y 1,365,000"] = 12
d$income[d$q10new == "Entre 1,365,001 y 1,600,000"] = 13
d$income[d$q10new == "Entre 1,600,001 y 2,000,000"] = 14
d$income[d$q10new == "Entre 2,000,001 y 3,150,000"] = 15
d$income[d$q10new == "M<e1>s de 3,150,000"] = 16
d$income[d$q10new == "No sabe"] = NA
d$income[d$q10new == "No responde"] = NA
d$income[d$q10new == "No Aplica"] = NA
describe(d$income)
sum(is.na(d$income))/nrow(d)

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

#####################
# Save
#####################

save(d,file = "~/Dropbox/Migration Colombia/01_data/clean_data/clean_colombia_2021march2.RData")
#save(d,file = "C:/Users/catis/Dropbox/Migration Colombia/01_data/clean_data/clean_colombia_2020jun29.RData")


