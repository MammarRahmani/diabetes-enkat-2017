library(plyr)
library(dplyr)
library(tidyr)
source("/Users/mammarrahmani/Projects/diabetesforbund/transform/GetObjectStorage.R")

Sys.setlocale(category = "LC_ALL", locale = "sv_SE.UTF-8")

survey_data_path <- "/Users/mammarrahmani/Projects/diabetesforbund/SURVEY"


#Load datasets
#patient_df_m   <-  read.csv(file = getObjectStorageFileWithCredentials_2b960019ae7a47dd9601026211e617af("Diabetesfrbundet", "patients_m.csv"))
#patient_df_sm  <-  read.csv(file = getObjectStorageFileWithCredentials_2b960019ae7a47dd9601026211e617af("Diabetesfrbundet", "patients_sm.csv"))

patient_df_m  <- read.csv(paste(survey_data_path, "patients_m.csv", sep="/"), stringsAsFactors = FALSE) 
patient_df_sm <- read.csv(paste(survey_data_path, "patients_sm.csv", sep="/"), stringsAsFactors = FALSE) 

#Fyll i resten av namnen i rätt ordning, skriv colnames() för att få ut alla namn i rätt ordning i noteooken
patient_kolumnanmn_forbund <- c("Timestamp", "Födelseår", "ArDeFickDiabetes", "Kön", "Landsting/Sjukhus",
                                "Landsting/Sjukhus_Saknas", "Restid", "StödUtanför", "KontaktMedDiabetiker", 
                                "FlexibelResemöjlighet", "AndraSjukdomar", "HBAC1","LättAttKontakta", "Förtroende",
                                "FöljerRåd", "NöjdMedBemötande", "BemötandeVidBesök","HurInformerad", "DelaktighetIBeslut",
                                "ErbjudnaKontaktvägar","AnvändaKontaktvägar", "FöredragnaKontaktvägar", "VillHaCGM", 
                                "ErbjudenCGM", "BetalatFörCGM","VillHaFGM", "ErbjudenFGM","BetalatFörFGM", 
                                "VillHaInsulinpump", "ErbjudenInsulinpump", "BetalatFörInsulinpump", 
                                "CGMLivskvalitet","HjälpAttTolkaCGM", "HjälpMedCGM", "FGMLivskvalitet", 
                                "HjälpAttTolkaFGM", "HjälpMedFGM","InsulinpumpLivskvalitet", "HjälpAttTolkaInsulinpump",
                                "HjälpMedInsulinpump","ErbjudenStöd", "UtnyttjatStöd", "FrågatOmStöd", "BehovAvStöd",
                                "HarStödetPåverkatPositivt","NöjdMedStödet")

patient_kolumnanmn_socialamedier <- c("Timestamp","Födelseår", "ArDeFickDiabetes", "Kön", "Medlem", "Landsting/Sjukhus", "Landsting/Sjukhus_Saknas",
                                      "Restid", "StödUtanför", "KontaktMedDiabetiker", "FlexibelResemöjlighet","AndraSjukdomar", 
                                      "HBAC1", "LättAttKontakta", "Förtroende", "FöljerRåd","NöjdMedBemötande", "BemötandeVidBesök",
                                      "HurInformerad", "DelaktighetIBeslut", "ErbjudnaKontaktvägar","AnvändaKontaktvägar", "FöredragnaKontaktvägar",
                                      "VillHaCGM","ErbjudenCGM", "BetalatFörCGM","VillHaFGM", "ErbjudenFGM","BetalatFörFGM","VillHaInsulinpump", 
                                      "ErbjudenInsulinpump", "BetalatFörInsulinpump","CGMLivskvalitet","HjälpAttTolkaCGM", "HjälpMedCGM", 
                                      "FGMLivskvalitet","HjälpAttTolkaFGM", "HjälpMedFGM","InsulinpumpLivskvalitet", "HjälpAttTolkaInsulinpump",
                                      "HjälpMedInsulinpump","ErbjudenStöd", "UtnyttjatStöd", "FrågatOmStöd", "BehovAvStöd",
                                      "HarStödetPåverkatPositivt","NöjdMedStödet")

#Uppdatera Kolumnnamn
colnames(patient_df_m)  <- patient_kolumnanmn_forbund
colnames(patient_df_sm) <- patient_kolumnanmn_socialamedier

#Lägg till medlems-kolumn.
patient_df_m$Medlem <- "Ja"

#Lägg till kolumner för survey-kanal
patient_df_m$SurveyChannel <- "Mail"
patient_df_sm$SurveyChannel <- "Sociala Medier"

#Bind ihop datan
patient_df <- plyr::rbind.fill(patient_df_m, patient_df_sm)

#Skapa en kolumn för sjukhus, en för landsting
patient_df$Landsting <- sapply(as.character(patient_df$"Landsting/Sjukhus"),  
                               function(x) strsplit(x, split = "\\-")[[1]][1])

patient_df$Sjukhus <- sapply(as.character(patient_df$"Landsting/Sjukhus"),  
                             function(x) strsplit(x, split = "\\-")[[1]][2])

#Räkna ut åldrar 
patient_df$Age <- 2017-as.integer(as.character(patient_df$Födelseår))
patient_df$AntalÅrMedDiabetes <- 2017-as.integer(as.character(patient_df$ArDeFickDiabetes))
patient_df$AgeDåDeFickDiabetes <- patient_df$Age-patient_df$AntalÅrMedDiabetes

patient_df[is.na(patient_df$Age), "Age"] <- "99"
patient_df$Age <- as.integer(patient_df$Age)

#Bin åldrar
levels <- c(-Inf, 9,19,29,39,49,59,Inf)
labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+")

patient_df$ageGroup <- cut(patient_df$Age, breaks=levels, labels=labels)

#Recalculate HBA1C that have used the old system (NGSP to IFCC)
patient_df$HBAC1 <- as.numeric(gsub("[^0-9\\.\\,]", "", patient_df$HBAC1))
patient_df[patient_df$HBAC1 < 30 & !is.na(patient_df$HBAC1), "HBAC1"] <- round((10.93*patient_df[patient_df$HBAC1 < 30 & !is.na(patient_df$HBAC1), "HBAC1"]) - 23.50, 0)

#Remove timestamp duplicates in dataset
patient_df$key_col <- paste(patient_df$Timestamp, patient_df$Födelseår, patient_df$ArDeFickDiabetes, sep="")
patient_df         <- subset(patient_df,!duplicated(patient_df$key_col)) %>% select(-key_col)
patient_df$ID      <- c(1:nrow(patient_df))

#Split the multiple choice column "Utnyttjat Stöd" into several columns.
patient_df_tmp <- patient_df %>% select(ID, UtnyttjatStöd)

patient_df_tmp$split_responses <- sapply(patient_df_tmp$UtnyttjatStöd,
                                         function(x) unlist(strsplit(as.character(x),';')))

patient_df_tmp = do.call(rbind,apply(patient_df_tmp,1,function(x) data.frame(x$ID, x$split_responses)))
patient_df_tmp$value = "Ja"
patient_df_tmp <- spread(patient_df_tmp,key=x.split_responses,value=value)
patient_df_tmp[is.na(patient_df_tmp)] <- "Nej"
colnames(patient_df_tmp) <- c("ID", "UtnyttjatKurator","UtnyttjatPsykolog","UtnyttjatEjFåttMölighet",
                              "UtnyttjatEjKäntBehov", "UtnyttjatBeteendevetare", "UtnyttjatAnnan" )

patient_df <- patient_df %>% left_join(patient_df_tmp, by="ID")

#Skapa en ny kolumn som säger "Ja" om vill ha FGM ELLER CGM.
patient_df <- transform(patient_df, 
          VillHaCGMFGM=ifelse(!(patient_df$VillHaCGM %in% c("Nej", "Vet ej")) | !(patient_df$VillHaFGM %in% c("Nej", "Vet ej")),
                              "Ja", "Nej/Vet ej"))

#Skapa tre nya värden för KontaktMedAndra: Ofta, Sällan, Aldrig
patient_df <- transform(patient_df, 
                        KontaktMedAndraDiabetiker_BIN=ifelse(patient_df$KontaktMedDiabetiker %in% c("Aldrig", "Någon gång per år"), "Aldrig",
                                                             ifelse(patient_df$KontaktMedDiabetiker=="Någon gång i månaden", "Sällan", "Ofta")))

#join with NDR data
ndr_data <- read.csv("county_df.csv", stringsAsFactors = FALSE)
patient_df <- patient_df %>% left_join(ndr_data, by=c("Landsting", "ageGroup"))

#join with landsting survey data
landsting_df_lt <- read.csv("/Users/mammarrahmani/Projects/diabetesforbund/SURVEY/landsting_df_lt.csv")
landsting_df_sj <- read.csv("/Users/mammarrahmani/Projects/diabetesforbund/SURVEY/landsting_df_sj.csv")

patient_df <- patient_df %>% left_join(landsting_df_lt, by=c("Landsting"="Sjukhus.Landsting"))
patient_df <- patient_df %>% left_join(landsting_df_sj, by=c("Sjukhus"="Sjukhus.Sjukhus"))

#Export
patient_df_main <- patient_df[,c(4, 7:19, 23:131)]
#patient_df_kv   <- patient_df[,c(4, 7:19, 23:131)]
 
colnames(patient_df_main)[1:55] <- paste("Patient", colnames(patient_df_main)[1:55], sep=".")

write.csv(patient_df_main, "/Users/mammarrahmani/Projects/diabetesforbund/dashboard/patient_df.csv", row.names = FALSE)
