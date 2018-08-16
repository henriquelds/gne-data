library(mice)

setwd("/home/henrique/Documents/gne-data/")
data <- read.csv("bxpeso_ml_mod.csv",sep = ",", encoding="UTF-8", na.strings = "NA")


names(data)

#remove second evaluation variables
cdata <- data[c(-80:-111)] 

#replace NAs with 0s
cdata[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")][is.na(cdata[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")])] <- 0
cdata[c("Traqueostomia")][is.na(cdata[c("Traqueostomia")])] <- 0

#replace NA on Extubado based on Traqueostomia
cdata[c("Extubado")][is.na(cdata[c("Extubado")])] <- ifelse(cdata$Traqueostomia[is.na(cdata[c("Extubado")])] > 0, 0, 1)


#fix TempoVM based on VM1 and TempoDeUTI 
cdata[c("TempoVM")][is.na(cdata[c("TempoVM")]) & cdata$VM1 == 1] <- cdata[c("TempoDeUTI")][is.na(cdata[c("TempoVM")]) & cdata$VM1 == 1]
cdata[c("TempoVM")][is.na(cdata[c("TempoVM")]) & cdata$VM1 == 0] <- 0

#nrow(data[data$VM1==0,])
#nrow(data[data$VM1==0 & !is.na(data$TempoVM),])

#fix morbidade and fr_sind_realimentacao variables to be the OR of its variables
cdata$Morbidade <- ifelse(cdata$Cirrose + cdata$ICC + cdata$IRC_HD 
                         + cdata$SIDA + cdata$Cancer + cdata$Cancer_hematologico
                         +cdata$Pneumopatias + cdata$DM > 0, 1, 0)

cdata$FR_sind_realimentacao <- ifelse(cdata$Anorexia + cdata$Marasmo + cdata$Institucionalizado 
                                     + cdata$Caquexia + cdata$Etilismo + cdata$Sem_dieta > 0, 1, 0)

cdata$Morb_imuno <- ifelse(cdata$SIDA + cdata$Cancer + cdata$Cancer_hematologico > 0, 1, 0)

#modify variable Alta_UTI to contain death trajectory if any
temp <- cdata$Alta_UTI - cdata$Obito
cdata$Alta_UTI <- mapvalues(temp, c("-1", "0", "1"), c("Obito", "AltaSeqObito", "Alta"))

cdata$Tempo_internacao_total <- abs(cdata$Tempo_internacao_total)


#melina classification

pink <- c("Idade", "MI_hospital", "DG_principal", "Morbidade", "Morb_imuno", "IMC",
          "SAPS3", "APACHEII", "SOFA_admissao", "SOFA", "Nutric", "VM1", "HD1", "Vasopressor1",
          "Sedoanalgesia1", "Tempo_internacao_total", "TempoVM", "TempoDeUTI")

green <- c("Previo_UTI", "MI_UTI", "FR_sind_realimentacao", "PCR1", "Lactato1",
           "Glicemia_max1", "Glicemia_min1", "Dieta")

endVars <- c("Alta_UTI", "Extubado", "Traqueostomia", "Obito", "Prontuario")

#remove diet variables for now....
cdata <- cdata[c(-85:-153)] 

cdata <- cdata[c(pink, green, endVars)]

regData <- cdata[c("SOFA_admissao", "SAPS3", "APACHEII", "PCR1", "Lactato1", "Glicemia_min1")]
imputed_Data <- mice(regData, m=5, maxit = 50, method = 'pmm', seed = 500, print=FALSE)

prepare_copies <- function(cdata, imputed_Data) {
  copies <- list(data.frame(cdata), data.frame(cdata), data.frame(cdata),
                 data.frame(cdata), data.frame(cdata))
  imp_data <- list()
  for (i in 1:length(imputed_Data$imp)) {
    imp_data[[i]] <- complete(imputed_Data, i)
  }
  
  for (j in 1:length(copies)) {
    copies[[j]]$PCR1 <- imp_data[[j]]$PCR1
    copies[[j]]$Lactato1 <- imp_data[[j]]$Lactato1
    copies[[j]]$Glicemia_min1 <- imp_data[[j]]$Glicemia_min1
  }
  
  copies
}

datasets <- prepare_copies(cdata, imputed_Data)



write.csv(datasets[[1]], "test.csv")
