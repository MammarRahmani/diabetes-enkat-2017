library(plyr)
library(dplyr)
library(tidyr)
library(rpart)

#install.packages("FSelector")

patient_df <- read.csv("/Users/mammarrahmani/Projects/diabetesforbund/dashboard/patient_df.csv", stringsAsFactors = FALSE)

patient_df_mod <- patient_df %>% select(Kön, Restid, StödUtanför, KontaktMedAndraDiabetiker_BIN, AndraSjukdomar, LättAttKontakta, 
                                        VillHaCGM, VillHaFGM, ErbjudenFGM, ErbjudenCGM, ErbjudenStöd, BehovAvStöd, Medlem, Landsting, ageGroup,
                                        AntalÅrMedDiabetes, UtnyttjatKurator, UtnyttjatBeteendevetare ,UtnyttjatKurator, UtnyttjatPsykolog,
                                        UtnyttjatEjFåttMölighet, AntalÅrMedDiabetes, AgeDåDeFickDiabetes, HbA1c..52..Andel..., HbA1c..70..Andel...,
                                        patientTillväxt....)

patient_df_mod <- patient_df %>% select(Kön, Restid, StödUtanför, KontaktMedAndraDiabetiker_BIN, AndraSjukdomar, LättAttKontakta, 
                                        Förtroende, VillHaCGM, VillHaFGM, ErbjudenFGM, ErbjudenCGM, ErbjudenStöd, BehovAvStöd, Medlem, Landsting, ageGroup,
                                        AntalÅrMedDiabetes, UtnyttjatKurator, UtnyttjatBeteendevetare ,UtnyttjatKurator, UtnyttjatPsykolog,
                                        UtnyttjatEjFåttMölighet, AntalÅrMedDiabetes, AgeDåDeFickDiabetes, HbA1c..52..Andel..., HbA1c..70..Andel...,
                                        patientTillväxt....)

fortroende_labels <- c("Ej Förtroende", "Förtroende")
fortroende_levels <- c(-Inf,3,Inf)

patient_df_mod$"Fortroende" <- cut(patient_df$"Förtroende", breaks=fortroende_levels, labels=fortroende_labels)

formula_tree <- paste(names(patient_df_mod)[1:(length(patient_df_mod)-1)], collapse='+')
formula_tree <- paste("Fortroende ~ ", formula_tree)

tree_mod <- rpart(formula_tree, data=patient_df_mod, method="class")

printcp(tree_mod)

plot(tree_mod, uniform=TRUE,
     main="Classification Tree Förtroende/Ej Förtroende")
text(tree_mod, use.n=TRUE, all=TRUE, cex=.8)


#Target Variables
Förtroende, Bemötande Vid Besök, HBAC1
colnames(patient_df_mod)


unique(patient_df_mod$ErbjudenCGM)
