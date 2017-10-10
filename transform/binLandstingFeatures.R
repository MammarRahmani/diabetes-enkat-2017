library(plotly)

p <- plot_ly(
  x = landsting_df_exp$Sjukhus,
  y = landsting_df_exp$FTEperPatient,
  name = "SF Zoo",
  type = "bar"
)


p

landsting_df$andelPatienterCGM_clean


tst <- landsting_df_exp %>% group_by(Landsting) %>% summarise(medel=mean(andelPatienterInsulinpump_clean))



#andelCGM 0-5%, 5-10%, 10-15%, 15-25%, 25%+
levels_CGM <- c(-Inf, 0.05,0.10,0.15,0.25,Inf)
labels_CGM <- c("0-5%", "6-10%", "11-15%", "16-25%", "26%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% mutate(Landsting.andelCGM=mean(andelPatienterCGM_clean))

landsting_df <- landsting_df %>% mutate(Landsting.andelCGM_grupp = cut(Landsting.andelCGM, breaks = levels_CGM, labels=labels_CGM),
                                         Sjukhus.andelCGM_grupp = cut(andelPatienterCGM_clean, breaks = levels_CGM, labels=labels_CGM))

#andelFGM 0-30%, 30-40%, 40-50%, 50-60%, 60%+
levels_FGM <- c(-Inf, 0.30,0.40,0.50,0.60,Inf)
labels_FGM <- c("0-30%", "31-40%", "41-50%", "51-60%", "61%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% mutate(Landsting.andelFGM=mean(andelPatienterFGM_clean))

landsting_df <- landsting_df %>% mutate(Landsting.andelFGM_grupp = cut(Landsting.andelFGM, breaks = levels_FGM, labels=labels_FGM),
                                        Sjukhus.andelFGM_grupp = cut(andelPatienterCGM_clean, breaks = levels_FGM, labels=labels_FGM))

#andelInsulinPump 0-15%, 15-20, 20-25%, 25-30%, 30%+
levels_IP <- c(-Inf, 0.15,0.20,0.25,0.30,Inf)
labels_IP <- c("0-15%", "16-20%", "21-25%", "25-30%", "31%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% mutate(Landsting.andelInsulinpump=mean(andelPatienterInsulinpump_clean))

landsting_df <- landsting_df %>% mutate(Landsting.andelInsulinpump_grupp = cut(Landsting.andelInsulinpump, breaks = levels_IP, labels=labels_IP),
                                        Sjukhus.andelInsulinpump_grupp = cut(andelPatienterInsulinpump_clean, breaks = levels_IP, labels=labels_IP))

#Landsting Registrerade patienter: 0-250, 250-500, 500-1000, 1000+
levels_REG <- c(-Inf, 250,500,1000,Inf)
labels_REG <- c("0-250", "251-500", "501-1000", "1000+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% mutate(Landsting.regitreradePatienter=mean(RegistreradePatienter))

landsting_df <- landsting_df %>% mutate(Landsting.registreradePatienter_grupp = cut(Landsting.regitreradePatienter, breaks = levels_REG, labels=labels_REG),
                                        Sjukhus.registreradePatienter_grupp = cut(RegistreradePatienter, breaks = levels_REG, labels=labels_REG))

#Landsting antalPatienteranscar: 0-100, 100-200, 200-300, 300-400, 400+
levels_APA <- c(-Inf, 100,200,300,400,Inf)
labels_APA <- c("0-100", "101-200", "201-300", "301-400", "401+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% mutate(Landsting.antalPatientsvar=mean(antalPatienterAnsvar_clean))

landsting_df <- landsting_df %>% mutate(Landsting.registreradePatienterAnsvar_grupp = cut(Landsting.antalPatientsvar, breaks = levels_APA, labels=labels_APA),
                                        Sjukhus.registreradePatienterAnsvar_grupp = cut(antalPatienterAnsvar_clean, breaks = levels_APA, labels=labels_APA))

#Landsting övertid-procent: 0-50, 50-100, 100-200, 200+
levels_ÖVR <- c(-Inf, 50,100,200,Inf)
labels_ÖVR <- c("0-50%", "51-100%", "101-200%", "201%+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% mutate(Landsting.övertidProcent=mean(övertidProcent))

landsting_df <- landsting_df %>% mutate(Landsting.övertidProcent_grupp = cut(Landsting.övertidProcent, breaks = levels_ÖVR, labels=labels_ÖVR),
                                        Sjukhus.övertidProcent_grupp = cut(övertidProcent, breaks = levels_ÖVR, labels=labels_ÖVR))

#Landsting FTEPerPatient:0-5, 5-10, 10-15, 15+
levels_FTE <- c(-Inf, 5,10,15,Inf)
labels_FTE <- c("0-5", "6-10", "11-15","16+")

landsting_df <- landsting_df %>% group_by(Landsting) %>% mutate(Landsting.FTEperPatient=mean(FTEperPatient))

landsting_df <- landsting_df %>% mutate(Landsting.FTEperPatient_grupp = cut(Landsting.FTEperPatient, breaks = levels_FTE, labels=labels_FTE),
                                        Sjukhus.FTEperPatient_grupp = cut(FTEperPatient, breaks = levels_FTE, labels=labels_FTE))

