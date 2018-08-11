library(ggplot2)
library(scales)
library(OneR)
library(dummies)
library(plyr)


setwd("/home/henrique/Documents/gne-data/")
data <- read.csv("bxpeso_ml.csv",sep = ";", encoding="UTF-8")
dietas <- read.csv("desc_dietas.csv", sep=",", encoding="UTF-8")
names(data)

###########VARIAVEIS DE ADMISSAO######################

data$Perda_de_peso <- NULL
data$Perda_ponderal <- NULL
data$Edema <- NULL
data$Perda_de_massa_temporal <- NULL

#replace NAs with 0s
data[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")][is.na(data[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")])] <- 0
#fix morbidade and fr_sind_realimentacao variables to be the OR of its variables
data$Morbidade <- ifelse(data$Cirrose + data$ICC + data$IRC_HD 
                         + data$SIDA + data$Cancer + data$Cancer_hematologico
                         +data$Pneumopatias + data$DM > 0, 1, 0)

data$FR_sind_realimentacao <- ifelse(data$Anorexia + data$Marasmo + data$Institucionalizado 
                         + data$Caquexia + data$Etilismo + data$Sem_dieta > 0, 1, 0)

#rearrange IMC into levels
data$IMC <- cut( data$IMC, c(-Inf,16,17,18.5,25), right = FALSE, labels=c("Grave", "Moderada", "Leve", "Saudavel"))

#Scores to levels
data$SAPS3 <- bin(data$SAPS3, method = "content", labels=c("Ate58","Ac58Ate65", "Ac65Ate72", "Ac72Ate79", "Ac79Ate115"))
data$APACHEII <- bin(data$APACHEII, method= "content", labels=c("Ate18","Ac18Ate22", "Ac22Ate26.6", "Ac26.6Ate31", "Ac31Ate53"))
data$SOFA_admissao <- bin(data$SOFA_admissao, method = "content", na.omit = FALSE, labels = c("Ate4", "Ac4Ate6", "Ac6Ate7", "Ac7Ate10", "Ac10Ate17"))
data$Nutric <- bin(data$Nutric, method="content", labels=c("Ate3", "Ac3Ate4", "Ac4Ate5", "Ac5Ate6", "Ac6Ate8"))

#TODO: transform factors into dummies

#####################################

########################VARIAVEIS DE DESFECHO#########################
data$DA_UTI <- NULL
data$DI_obito_alta <- NULL
data$Data_extubacao <- NULL
data$Data_TQ <- NULL
data$Reinternacao_UTI <- NULL
data$N_reinternacao <- NULL
data$Sind_realimenta <- NULL


#modify variable Alta_UTI to contain death trajectory if any
temp <- data$Alta_UTI - data$Obito
data$Alta_UTI <- mapvalues(temp, c("-1", "0", "1"), c("Obito", "AltaSeqObito", "Alta"))


#replace NAs with 0s
#TODO: the same with Extubado????
data[c("Traqueostomia")][is.na(data[c("Traqueostomia")])] <- 0

##############################

##################VARIAVEIS AVAL1####################
data$Corticoide1 <- NULL

data[c("Dietas_nome")][is.na(data[c("Dietas_nome")])] <- 999
#Rearrange SOFA aval1 to match same bins as SOFA_admissao
data$SOFA <- cut( data$SOFA, c(-Inf,4,6,7,10,17), right = FALSE, labels = c("Ate4", "Ac4Ate6", "Ac6Ate7", "Ac7Ate10", "Ac10Ate17"))
#Replace NAs in PCR1 with the global mean
meanPCR1 <- mean(data$PCR1, na.rm = TRUE)
meanPCR1
data[c("PCR1")][is.na(data[c("PCR1")])] <- meanPCR1
data$PCR1 <- bin(data$PCR1, method="content", labels=c("Ate57.3", "Ac57.3Ate109", "Ac109Ate131", "Ac131Ate193", "Ac193Ate450"))
table(data$HD1)

#left outer join on dietas
data <- merge(data, dietas, by.x="Dietas_nome", by.y="Codigo", all.x = TRUE)

table(data$Extubado)
sum(is.na(data$Extubado))

class(data$Perda_de_massa_temporal)
write.csv(data[c("SOFA_admissao", "SOFA", "PCR1")], "test.csv")
table(data$Edema)
sum(is.na(data$Perda_de_massa_temporal))

