library(dplyr)
library(tidyr)
Sys.setlocale(category = "LC_ALL", locale = "sv_SE.UTF-8")


landsting_df <- read.csv("/Users/mammarrahmani/Projects/diabetesforbund/SURVEY/landsting.csv")

df_colnames <- c("Timestamp", "Enhet", "EnhetSaknas", "RegistreradePatienter", 
                 "diabetologer100", "diabetologer99", "diabetologer79", "diabetologer49", "diabetologer29",
                 "antalhÖvertid", "antalPatienterAnsvar", "antalPsykologer", "antalBeteendevetare", "antalKuratorer",
                 "finnsBarnmottagning", "barnmottagningSammarbete", "initiativBarnÖvergång", "nuvarandeKontaktvägar",
                 "nyaKontaktvägar", "nyaKontaktvägarFree", "tillgänglighetTelefonmöte", "tillgänglighetJourtelefon",
                 "tillgänglighetMail", "tillgänglighetSMS", "tillgänglighetChat", "tillgänglighetChatApp", 
                 "tillgängligheVideoBesök", "tillgänglighetDropIn", "mestEffektivaKontaktvägar", "antalPatienterCGM",
                 "antalPatienterFGM", "antalPatienterInsulinpumo", "riktlinjerCGM", "riktlinjerFGM", "riktlinjerInsulinpump",
                 "utbildningCGM", "utbildningFGM", "utbilningInsulinpump", "efterfråganCGM", "efterfråganFGM", "efterfråganInsulinpump",
                 "budgetBelastningTHM", "erbjuderPsykosocialt", "efterfråganPsykoSocialtStöd", "strategiPsykoSocialtStöd",
                 "strategiPsykoSocialtStöd_free", "specifikBudgetPsykoSocialtStöd", "patientutbildingPsykoSocialtStöd", 
                 "patientutbildingPsykoSocialtStöd_free")

colnames(landsting_df) <- df_colnames
landsting_df$"ID"      <- c(1:nrow(landsting_df))

#skapa sjukhus/landsting-kolumner
landsting_df$Landsting <- sapply(as.character(landsting_df$"Enhet"),  
                               function(x) strsplit(x, split = "\\-")[[1]][1])
landsting_df$Sjukhus <- sapply(as.character(landsting_df$"Enhet"),  
                             function(x) strsplit(x, split = "\\-")[[1]][2])

#Räkna ut FTE och skapa features kring detta
FTE_data <- read.csv2("/Users/mammarrahmani/Projects/diabetesforbund/SURVEY/FTE-data.csv", encoding = "UTF-8")

FTE_data[is.na(FTE_data)] <- 0
FTE_data <- FTE_data %>% mutate(FTE=(diabetologer100*40*1)+(diabetologer99*40*0.9)+(diabetologer79*40*0.65)+(diabetologer49*40*0.4)+
                                (diabetologer29*40*0.15), övertidProcent=(antalhÌÐvertid/FTE)*100, 
                                FTEperPatient=(FTE*52)/RegistreradePatienter)

FTE_data <- FTE_data %>% mutate(andelPatienterCGM_clean=antalPatienterCGM_clean/RegistreradePatienter,
                                andelPatienterFGM_clean=antalPatienterFGM_clean/RegistreradePatienter,
                                andelPatienterInsulinpump_clean=antalPatienterInsulinpump_clean/RegistreradePatienter)

FTE_data <- FTE_data %>% select(Timestamp, RegistreradePatienter, antalPatienterAnsvar_clean, FTE, övertidProcent, FTEperPatient, antalPatienterCGM_clean, 
                                antalPatienterCGM_clean, antalPatienterFGM_clean, antalPatienterInsulinpump_clean,
                                andelPatienterCGM_clean, andelPatienterFGM_clean, andelPatienterInsulinpump_clean)

landsting_df <- landsting_df %>% select(-RegistreradePatienter, -antalPatienterAnsvar)
landsting_df <- landsting_df %>% left_join(FTE_data, by="Timestamp")

#Skapa nya kolumner för om de har PSS knutet till mottagningen
landsting_df <- landsting_df %>% mutate(knutenPsykolog=ifelse(antalPsykologer=="Det finns inga psykologer knutna till vår mottagning", "Nej", "Ja"),
                                        knutenBeteendevetare=ifelse(antalBeteendevetare=="Det finns inga beteendevetare knutna till vår mottagning", "Nej", "Ja"),
                                        knutenKurator=ifelse(antalKuratorer=="Det finns inga kuratorer knutna till vår mottagning", "Nej", "Ja"))
#Spread Erbjudande Tekniska Hjälpmedel
landsting_df_tmp <- landsting_df %>% select(ID, erbjuderPsykosocialt)

landsting_df_tmp$split_responses <- sapply(landsting_df_tmp$erbjuderPsykosocialt,
                                         function(x) unlist(strsplit(as.character(x),';')))

landsting_df_tmp = do.call(rbind,apply(landsting_df_tmp,1,function(x) data.frame(x$ID, x$split_responses)))
landsting_df_tmp$value = "Ja"
landsting_df_tmp <- spread(landsting_df_tmp,key=x.split_responses,value=value)
landsting_df_tmp[is.na(landsting_df_tmp)] <- "Nej"
colnames(landsting_df_tmp) <- c("ID", "erbjuderKurator","erbjuderPsykolog","erbjuderBeteendevetare",
                                "erbjuderAnnat","erbjuderInget")

landsting_df <- landsting_df %>% left_join(landsting_df_tmp, by="ID")

################BIN COLUMNS

#andelCGM 0-5%, 5-10%, 10-15%, 15-25%, 25%+
levels_CGM <- c(-Inf, 0.05,0.10,0.15,0.25,Inf)
labels_CGM <- c("0-5%", "6-10%", "11-15%", "16-25%", "26%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% dplyr::mutate(Landsting.andelCGM=mean(andelPatienterCGM_clean))

landsting_df <- landsting_df %>% mutate(Landsting.andelCGM_grupp = cut(Landsting.andelCGM, breaks = levels_CGM, labels=labels_CGM),
                                        Sjukhus.andelCGM_grupp = cut(andelPatienterCGM_clean, breaks = levels_CGM, labels=labels_CGM))

#andelFGM 0-30%, 30-40%, 40-50%, 50-60%, 60%+
levels_FGM <- c(-Inf, 0.30,0.40,0.50,0.60,Inf)
labels_FGM <- c("0-30%", "31-40%", "41-50%", "51-60%", "61%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% dplyr::mutate(Landsting.andelFGM=mean(andelPatienterFGM_clean))

landsting_df <- landsting_df %>% mutate(Landsting.andelFGM_grupp = cut(Landsting.andelFGM, breaks = levels_FGM, labels=labels_FGM),
                                        Sjukhus.andelFGM_grupp = cut(andelPatienterCGM_clean, breaks = levels_FGM, labels=labels_FGM))

#andelInsulinPump 0-15%, 15-20, 20-25%, 25-30%, 30%+
levels_IP <- c(-Inf, 0.15,0.20,0.25,0.30,Inf)
labels_IP <- c("0-15%", "16-20%", "21-25%", "25-30%", "31%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% dplyr::mutate(Landsting.andelInsulinpump=mean(andelPatienterInsulinpump_clean))

landsting_df <- landsting_df %>% mutate(Landsting.andelInsulinpump_grupp = cut(Landsting.andelInsulinpump, breaks = levels_IP, labels=labels_IP),
                                        Sjukhus.andelInsulinpump_grupp = cut(andelPatienterInsulinpump_clean, breaks = levels_IP, labels=labels_IP))

#Landsting Registrerade patienter: 0-250, 250-500, 500-1000, 1000+
levels_REG <- c(-Inf, 250,500,1000,Inf)
labels_REG <- c("0-250", "251-500", "501-1000", "1000+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% dplyr::mutate(Landsting.regitreradePatienter=mean(RegistreradePatienter))

landsting_df <- landsting_df %>% mutate(Landsting.registreradePatienter_grupp = cut(Landsting.regitreradePatienter, breaks = levels_REG, labels=labels_REG),
                                        Sjukhus.registreradePatienter_grupp = cut(RegistreradePatienter, breaks = levels_REG, labels=labels_REG))

#Landsting antalPatienteranscar: 0-100, 100-200, 200-300, 300-400, 400+
levels_APA <- c(-Inf, 100,200,300,400,Inf)
labels_APA <- c("0-100", "101-200", "201-300", "301-400", "401+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% dplyr::mutate(Landsting.antalPatientsvar=mean(antalPatienterAnsvar_clean))

landsting_df <- landsting_df %>% mutate(Landsting.registreradePatienterAnsvar_grupp = cut(Landsting.antalPatientsvar, breaks = levels_APA, labels=labels_APA),
                                        Sjukhus.registreradePatienterAnsvar_grupp = cut(antalPatienterAnsvar_clean, breaks = levels_APA, labels=labels_APA))

#Landsting övertid-procent: 0-50, 50-100, 100-200, 200+
levels_ÖVR <- c(-Inf, 50,100,200,Inf)
labels_ÖVR <- c("0-50%", "51-100%", "101-200%", "201%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% dplyr::mutate(Landsting.övertidProcent=mean(övertidProcent))

landsting_df <- landsting_df %>% mutate(Landsting.övertidProcent_grupp = cut(Landsting.övertidProcent, breaks = levels_ÖVR, labels=labels_ÖVR),
                                        Sjukhus.övertidProcent_grupp = cut(övertidProcent, breaks = levels_ÖVR, labels=labels_ÖVR))

#Landsting FTEPerPatient:0-5, 5-10, 10-15, 15+
levels_FTE <- c(-Inf, 5,10,15,Inf)
labels_FTE <- c("0-5", "6-10", "11-15","16+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% dplyr::mutate(Landsting.FTEperPatient=mean(FTEperPatient))

landsting_df <- landsting_df %>% mutate(Landsting.FTEperPatient_grupp = cut(Landsting.FTEperPatient, breaks = levels_FTE, labels=labels_FTE),
                                        Sjukhus.FTEperPatient_grupp = cut(FTEperPatient, breaks = levels_FTE, labels=labels_FTE))

############EXPORT
colnames(landsting_df)[1:69] <- paste("Sjukhus", colnames(landsting_df)[1:69], sep=".")

landsting_df_exp <- landsting_df[, c(13:15, 31:47, 49:90)]
landsting_df_kv  <- landsting_df[, c(49,50, 16:27)]

landsting_df_lt <- landsting_df_exp[, grep("Landsting", colnames(landsting_df_exp), perl = TRUE)]
landsting_df_lt <- subset(landsting_df_lt, !duplicated(landsting_df_lt$Sjukhus.Landsting))

landsting_df_sj <- landsting_df_exp[, grep("Sjukhus", colnames(landsting_df_exp), perl = TRUE)]
landsting_df_sj$Sjukhus.Landsting <- NULL

write.csv(landsting_df_lt, "/Users/mammarrahmani/Projects/diabetesforbund/SURVEY/landsting_df_lt.csv", row.names = FALSE)
write.csv(landsting_df_sj, "/Users/mammarrahmani/Projects/diabetesforbund/SURVEY/landsting_df_sj.csv", row.names = FALSE)