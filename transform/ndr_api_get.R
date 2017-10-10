library(jsonlite)
library(plyr)
library(dplyr)

Sys.setlocale(category = "LC_ALL", locale = "sv_SE.UTF-8")
setwd("/Users/mammarrahmani/Projects/diabetesforbund/NDR")

enheter         <- fromJSON("enhet.JSON")
landsting       <- fromJSON("landsting.JSON")
indikator       <- fromJSON("indikator.JSON")
indikator[nrow(indikator),c("id", "name")] <- c(2052, "Insulinbehandlade med CGM")
indikator$id <- as.integer(indikator$id)

#V?lj vilka indikatorer som ska exporteras
indikator_selected <- indikator %>% filter(id %in% c(201, 221, 224, 101, 2052))


url_list          <- indikator_selected %>% select(id, name)
generate_url_list <- function(a1, a2) {
  
  url_list$start_age    <- a1 
  url_list$end_age      <- a2
  
  url_list$url          <- paste("https://www.ndr.nu/IFrameKnappen/api/County/Indicator?includeNDR=true",
                                 "&indicator=", url_list$id,
                                 "&diabetesType=1",
                                 "&sex=0",
                                 "&careunitType=2",
                                 "&fromYear=2016&fromQuartal=0&toYear=2016&toQuartal=0",
                                 "&fromAge=", url_list$start_age,
                                 "&toAge=", url_list$end_age,
                                 "&APIKey=k94GnjflHf4dFG6HdsFe",
                                 sep="")
  
  return(url_list)
} 

url_list <- rbind(generate_url_list(20, 29),
             generate_url_list(30, 39),
             generate_url_list(40, 49),
             generate_url_list(50, 59),
             generate_url_list(60, 110))

ndr_data <- list()

for (i in 1:nrow(url_list)) {
  
  ndr_data[[i]] <- fromJSON(url_list[i, 5])[[2]]
  ndr_data[[i]]$indicatorCode <- paste(url_list[i, 1])
  ndr_data[[i]]$ageGroup <- paste(url_list[i, 3], url_list[i, 4], sep="-")
  
  ndr_data[[i]] <- data.frame(ndr_data[[i]][[1]], ndr_data[[i]][[2]], ndr_data[[i]][[4]], ndr_data[[i]][[5]])
  
}

ndr_data_df <- plyr::ldply(ndr_data, rbind.fill)
colnames(ndr_data_df) <- c("CountyID", "CountyName", "UnitLevel", "UnitType", "AntalPatienter", "crepInd", "AntalRapporterade", "indicatorValue", "lnkonf",
"unkonf", "avgAge", "Oklar", "indicatorCode", "ageGroup")

county_df   <- ndr_data_df %>% filter(indicatorCode==101) %>% select(CountyID, CountyName, ageGroup, AntalPatienter)

#Joina med indicator-tabell
ndr_data_df$indicatorCode <- as.integer(as.character(ndr_data_df$indicatorCode))
ndr_data_df <- ndr_data_df %>% left_join(indikator[,c("id", "name", "unit")], by=c("indicatorCode"="id"))

#spread
ndr_data_spread <- ndr_data_df %>% select(CountyID, CountyName, ageGroup, indicatorCode, name, indicatorValue, unit) %>% 
  arrange(indicatorCode, CountyID)

ndr_data_indicators <- data.frame(ndr_data_spread[1:105, ], ndr_data_spread[106:210,4:7], 
                                  ndr_data_spread[211:315,4:7], ndr_data_spread[316:420,4:7], ndr_data_spread[421:525,4:7])  

colnames_ind <- c(paste(unique(ndr_data_indicators$name), " (" , unique(ndr_data_indicators$unit), ")", sep=""),
paste(unique(ndr_data_indicators$name.1), " (" , unique(ndr_data_indicators$unit.1), ")", sep=""),
paste(unique(ndr_data_indicators$name.2), " (" , unique(ndr_data_indicators$unit.2), ")", sep=""),
paste(unique(ndr_data_indicators$name.3), " (" , unique(ndr_data_indicators$unit.3), ")", sep=""),
paste(unique(ndr_data_indicators$name.4), " (" , unique(ndr_data_indicators$unit.4), ")", sep=""))

colnames(ndr_data_indicators)[c(6,10,14,18,22)] <- colnames_ind
county_indicators <- ndr_data_indicators[,c(1:3,6,10,14,18,22)]
county_df <- county_df %>% left_join(county_indicators, by=c("CountyID","CountyName", "ageGroup"))

antal_patienter <- read.csv2("landsting_alder.csv", stringsAsFactors = FALSE)
colnames(antal_patienter) <- c("CountyName", "n_patienter_2013", "n_patienter_2016", "ageGroup")

county_df$ageGroup <- as.character(county_df$ageGroup)
county_df[county_df$ageGroup=="60-110", "ageGroup"] <- "60+"

#TRIM
county_df$CountyName        <- gsub("^\\s*|\\s*$", "", county_df$CountyName)
antal_patienter$CountyName  <- gsub("^\\s*|\\s*$", "", antal_patienter$CountyName)

#Join no of patients data
county_df <- county_df %>% left_join(antal_patienter, by=c("CountyName", "ageGroup")) %>% select(-AntalPatienter)
county_df <- county_df %>% mutate("patientTillväxt (%)"=(n_patienter_2016/n_patienter_2013-1)*100)

#Change county name to survey names
landst_codebook <- read.csv2("landsting_codebook.csv", stringsAsFactors = FALSE)
landst_codebook$CountyName        <- gsub("^\\s*|\\s*$", "", landst_codebook$CountyName)
county_df <- county_df %>% left_join(landst_codebook, by="CountyName")

colnames(county_df)[4:11] <- paste("NDR", colnames(county_df)[4:11], sep=".")
county_df <- county_df[,3:12]

#Output
write.csv(county_df, "county_df.csv", row.names = FALSE)