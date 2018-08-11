library(ggplot2)
library(scales)
library(OneR)
library(dummies)


setwd("/home/henrique/Documents/gne-data/")
data <- read.csv("bxpeso_ml.csv",sep = ";", encoding="UTF-8")
names(data)
data$Ficha <- NULL
data$Hospital <- NULL
data$DIH <- NULL
data$DIUTI <- NULL
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
data$SAPS3 <- bin(data$SAPS3, method = "content", labels=c("Ate58","De58Ate65", "De65Ate72", "De72Ate79", "De79Ate115"))
data$APACHEII <- bin(data$APACHEII, method= "content", labels=c("Ate18","De18Ate22", "De22Ate26.6", "De26.6Ate31", "De31Ate53"))
data$SOFA_admissao <- bin(data$SOFA_admissao, method = "content", na.omit = FALSE, labels = c("Ate4", "De4Ate6", "De6Ate7", "De7Ate10", "De10Ate17"))
data$Nutric <- bin(data$Nutric, method="content")

table(temp)

class(data$Perda_de_massa_temporal)
write.csv(data, "test.csv")
table(data$Edema)
sum(is.na(data$Perda_de_massa_temporal))
sum(is.na(data$N_Reinternação))
