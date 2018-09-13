library(OneR)

replace_with_bins <- function(dt) {
  dt <- bin(dt, method="content", na.omit=FALSE)
  dt
  
}


setwd("C:\\Users\\henri\\OneDrive\\Documents\\gne-data\\")
data <- read.csv("bxpeso_ml_assoc.csv",sep = ",", encoding="UTF-8", na.strings = "NA")

names(data)


#melina classification

pink <- c("A0_Idade", "A0_MI_hospital", "A0_DG_principal", "A0_Morbidade", "A0_Morb_imuno", "A0_IMC",
          "A0_SAPS3", "A0_APACHEII", "A0_SOFA_admissao",  "A0_Nutric", "A1_SOFA", "A1_VM1", "A1_HD1", "A1_Vasopressor1",
          "A1_Sedoanalgesia1", "A2_SOFA2", "A2_VM2", "A2_HD", "A2_Vasopressor", "A2_Sedacao2",
          "F1_Tempo_internacao_total", "F1_TempoVM", "F1_TempoDeUTI")

green <- c("A0_Previo_UTI", "A0_MI_UTI", "A0_FR_sind_realimentacao", "A1_PCR1", "A1_Lactato1",
           "A1_Glicemia_max1", "A1_Glicemia_min1", "A1_Dieta", 
           "A2_PCR2", "A2_Lactato2",
           "A2_GC_max", "A2_GC_min", "A2_Dieta2")

yellow <- c("A1_Temp_max1", "A1_Potassio1", "A1_Repo_vitaminas","A1_Total_kcal_peso1",
            "A2_Temp_max2", "A2_Potassio2", "A2_Repo_vitaminas","A2_Kcal_peso2")

endVars <- c("A2_Nao_aval", "F1_Alta_UTI", "F1_Extubado", "F1_Traqueostomia", "F1_Obito", "A0_Prontuario")

dietVars1 <- names(data)[c(84:151)]

dietVars2 <- names(data)[c(194:261)]

cdata <- data[c(pink , green, yellow, dietVars1, dietVars2, endVars)]
names(cdata)[60] <- "A1D_MicroFerro"
names(cdata)[128] <- "A2D_MicroFerro"
cdata <-cdata[,order(colnames(cdata),decreasing=FALSE)]


cdata$A1D_Fibras.1 <- NULL
cdata$A1D_Soluveis <- NULL
cdata$A1D_Insoluveis <- NULL
cdata$A2D_Fibras.1 <- NULL
cdata$A2D_Soluveis <- NULL
cdata$A2D_Insoluveis <- NULL

cdata$A1D_Carotenoides <- NULL
cdata$A1D_Fluor <- NULL
cdata$A1D_Taurina <- NULL
cdata$A1D_L_Carnitina <- NULL
cdata$A1D_Arginina <- NULL
cdata$A2D_Carotenoides <- NULL
cdata$A2D_Fluor <- NULL
cdata$A2D_Taurina <- NULL
cdata$A2D_L_Carnitina <- NULL
cdata$A2D_Arginina <- NULL
names(cdata)


toDummyD1 <- c(40:59)
toDummyD2 <- c(115:134)
cdata[toDummyD1] <- ifelse(cdata[toDummyD1] > 0, "1", "0")
cdata[toDummyD1] <- lapply(cdata[toDummyD1], factor)
cdata[toDummyD2] <- ifelse(cdata[toDummyD2] > 0, "1", "0")
cdata[toDummyD2] <- lapply(cdata[toDummyD2], factor)

cdata$A0_IMC <- cut( cdata$A0_IMC, c(-Inf,16,17,18.5,25), right = FALSE, labels=c("Grave", "Moderada", "Leve", "Saudavel"))

#,10,13:14,16:17,19:21, 24:26,29:39, 60:88,
total <- c(1:170)
numerical <- c(1,4,10,13:14,16:17,19:21, 24:26,29:39, 60:88,90:91,93:94, 96:97, 100:101, 104:114,135:163, 167:169)


tofactor <- setdiff(total, numerical)
cdata[tofactor] <- lapply(cdata[tofactor], factor)
cdata[tofactor] <- mapply(function(n, f) {
  levels(f) <- paste(n, levels(f), sep=":")
  f
}, names(cdata)[tofactor], cdata[tofactor])


names(cdata)[numerical]
cdata[numerical] <- replace_with_bins(cdata[numerical])

cdata[numerical] <- mapply(function(n, f) {
  levels(f) <- paste(n, levels(f), sep=":")
  f
}, names(cdata)[numerical], cdata[numerical])

lapply(cdata, class)

write.csv(cdata, file="df1_assoc_new.csv", row.names=FALSE)
