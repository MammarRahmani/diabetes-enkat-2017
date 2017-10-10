library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)
Sys.setlocale(category = "LC_ALL", locale = "sv_SE.UTF-8")

#Read data for diabetesforbondet dashboard
patient_df <- read.csv("patient_df.csv")

#Select questions to display in the Focus Group area
quest_cols <- patient_df %>% colnames()

#Plot for Age Distrubution
age_distr     <- patient_df %>% group_by(Patient.Age) %>% dplyr::summarise(antal=n()) %>% filter(Patient.Age!=99)
plot_age_distrub <- plot_ly(
  x = age_distr$Patient.Age,
  y = age_distr$antal,
  name = "Age Distr.",
  type = "bar"
)

#Plot for Landsting Distrubution
landst_distr  <- patient_df %>% group_by(Patient.Landsting) %>% dplyr::summarise(antal=n())
plot_landst_distrub <- plot_ly(
  x = landst_distr$Patient.Landsting,
  y = landst_distr$antal,
  name = "Landst. Distr.",
  type = "bar"
)

#Plot for "St?d utanf?r v?rden"
stodutanfor   <- patient_df %>% group_by(Patient.StödUtanför) %>% dplyr::summarise(antal=n())
plot_stod <- plot_ly(stodutanfor, labels = ~Patient.StödUtanför, values = ~antal, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~antal,
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Plot for "Kontakt med andra personer med diabetes"
kontakt1d     <- patient_df %>% group_by(Patient.KontaktMedDiabetiker) %>% dplyr::summarise(antal=n())
plot_kontakt <- plot_ly(kontakt1d, labels = ~Patient.KontaktMedDiabetiker, values = ~antal, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~antal,
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = TRUE) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Plot for F?rtroende
informerad  <- patient_df %>% group_by(Patient.HurInformerad) %>% dplyr::summarise(antal=n())
delaktighet <- patient_df %>% group_by(Patient.DelaktighetIBeslut) %>% dplyr::summarise(antal=n())
foljerrad   <- patient_df %>% group_by(Patient.FöljerRåd) %>% dplyr::summarise(antal=n())
fortroende  <- patient_df %>% group_by(Patient.Förtroende) %>% dplyr::summarise(antal=n())

fortroende_df <- data.frame(informerad, delaktighet[,2],foljerrad[,2],fortroende[,2])
colnames(fortroende_df) <- c("svar", "Patient.HurInformerad", "Patient.DelaktighetBeslut", "Patient.FöljerRåd", "Patient.Förtroende")

plot_fortroende <- plot_ly(fortroende_df, x = ~svar, y = ~Patient.HurInformerad, type = 'bar', name = 'Patient.HurInformerad') %>%
  add_trace(y = ~Patient.DelaktighetBeslut, name = 'Patient.DelaktighetBeslut',  marker = list(color = 'rgb(195, 240, 255)')) %>%
  add_trace(y = ~Patient.FöljerRåd, name = 'Patient.FöljerRåd', marker = list(color = 'rgb(167, 88, 162)')) %>%
  add_trace(y = ~Patient.Förtroende, name = 'Patient.Förtroende', marker = list(color = 'rgb(222, 155, 222)')) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

#Plot f?r bem?tande
bemotande     <- patient_df %>% group_by(Patient.BemötandeVidBesök) %>% dplyr::summarise(antal=n())

plot_bemotande <- plot_ly(
  x = bemotande$Patient.BemötandeVidBesök,
  y = bemotande$antal,
  name = "Bemotande",
  type = "bar"
)

#Skapa dataset f?r "Totala distrubutioner"
agegroup_total    <- patient_df %>% group_by(Patient.ageGroup) %>% dplyr::summarise(Total=n()/nrow(patient_df))
ladsting_total    <- patient_df %>% group_by(Patient.Landsting) %>% dplyr::summarise(Total=n()/nrow(patient_df))
informerad_total  <- patient_df %>% group_by(Patient.HurInformerad) %>% dplyr::summarise(Total=n()/nrow(patient_df))
fortroende_total  <- patient_df %>% group_by(Patient.Förtroende) %>% dplyr::summarise(Total=n()/nrow(patient_df))
kontakt_total     <- patient_df %>% group_by(Patient.KontaktMedDiabetiker) %>% dplyr::summarise(Total=n()/nrow(patient_df))
stod_total        <- patient_df %>% group_by(Patient.StödUtanför) %>% dplyr::summarise(Total=n()/nrow(patient_df))