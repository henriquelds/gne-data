setwd("C:\\Users\\henri\\OneDrive\\Documents\\gne-data\\")
data <- read.csv("bxpeso_ml.csv",sep = ";", encoding="UTF-8", dec= ",")
dietas <- read.csv("desc_dietas.csv", sep=",", encoding="UTF-8", dec=".")
names(data)
data[c("Dietas_nome")]

pcts <- c(22510516,42144825,43258786,44231156,19448058,28601238)

for(i in 1:length(pcts)){
  data[which(data$Prontuario == pcts[i]),]$Dietas_nome <- 999
  
}


data[c("Dietas_nome")][is.na(data[c("Dietas_nome")])] <- 999
data[c("Dietas_nome")]

#replace NAs with 0s
data[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")][is.na(data[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")])] <- 0
data[c("Traqueostomia")][is.na(data[c("Traqueostomia")])] <- 0

#replace NA on Extubado based on Traqueostomia
data[c("Extubado")][is.na(data[c("Extubado")])] <- ifelse(data$Traqueostomia[is.na(data[c("Extubado")])] > 0, 0, 1)

#replace NA on SOFA (1st eval) with SOFA_admissao
data[c("SOFA")][is.na(data[c("SOFA")])] <- data[c("SOFA_admissao")][is.na(data[c("SOFA")])]

#fix TempoVM based on VM1 and TempoDeUTI 
data[c("TempoVM")][is.na(data[c("TempoVM")]) & data$VM1 == 1] <- data[c("TempoDeUTI")][is.na(data[c("TempoVM")]) & data$VM1 == 1]
data[c("TempoVM")][is.na(data[c("TempoVM")]) & data$VM1 == 0] <- 0

#fix morbidade and fr_sind_realimentacao variables to be the OR of its variables
data$Morbidade <- ifelse(data$Cirrose + data$ICC + data$IRC_HD 
                          + data$SIDA + data$Cancer + data$Cancer_hematologico
                          +data$Pneumopatias + data$DM > 0, 1, 0)

data$FR_sind_realimentacao <- ifelse(data$Anorexia + data$Marasmo + data$Institucionalizado 
                                      + data$Caquexia + data$Etilismo + data$Sem_dieta > 0, 1, 0)

data$Morb_imuno <- ifelse(data$SIDA + data$Cancer + data$Cancer_hematologico > 0, 1, 0)

temp <- data$Alta_UTI - data$Obito
data$Alta_UTI <- mapvalues(temp, c("-1", "0", "1"), c("Obito", "AltaSeqObito", "Alta"))

data$Tempo_internacao_total <- abs(data$Tempo_internacao_total)


data <- merge(data, dietas, by.x="Dietas_nome", by.y="Codigo", all.x = TRUE, suffixes = c("", ".D1"))
names(data)
names(data)[c(141:208)] <- paste("A1D", colnames(data[141:208]), sep = "_")

names(data)
data <- merge(data, dietas, by.x="Dieta2_nome_comercial", by.y="Codigo", all.x = TRUE, suffixes = c("", ".D2"))

names(data)[c(209:276)] <- paste("A2D", colnames(data[209:276]), sep = "_")

names(data)[c(3:40)] <- paste("A0", colnames(data[3:40]), sep = "_")
names(data)[c(139:140)] <- paste("A0", colnames(data[139:140]), sep = "_")

names(data)[c(2)] <- paste("A1", colnames(data[2]), sep = "_")
names(data)[c(52:92)] <- paste("A1", colnames(data[52:92]), sep = "_")

names(data)[c(1)] <- paste("A2", colnames(data[1]), sep = "_")
names(data)[c(93:132)] <- paste("A2", colnames(data[93:132]), sep = "_")

names(data)[c(41:50)] <- paste("F1", colnames(data[41:50]), sep = "_")
names(data)[c(133:138)] <- paste("F1", colnames(data[133:138]), sep = "_")

names(data)
data <-data[,order(colnames(data),decreasing=FALSE)]
names(data)
data[, 83:150][is.na(data[, 83:150])] <- 0
data[, 192:259][is.na(data[, 192:259])] <- 0


data$A1_Repo_vitaminas <- ifelse(data$A1_Tiamina1 + data$A1_Vitaminas_outras1 > 0, 1, 0)
data$A2_Repo_vitaminas <- ifelse(data$A2_Tiamina2 + data$A2_Vitaminas_outras2 > 0, 1, 0)
data <-data[,order(colnames(data),decreasing=FALSE)]
names(data)
 data$DI_obito_alta <- NULL
write.csv(data, "bxpeso_ml_assoc.csv", row.names = FALSE)
