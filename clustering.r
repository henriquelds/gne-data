library(mice)
library(plyr)
library(cluster)
library(Rtsne)
library(dplyr)
library(ggplot2)

setwd("C:\\Users\\henri\\OneDrive\\Documents\\gne-data")
data <- read.csv("bxpeso_ml_mod.csv",sep = ",", encoding="UTF-8", na.strings = "NA")


names(data)

#remove second evaluation variables
cdata <- data[c(-80:-111)] 

#replace NAs with 0s
cdata[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")][is.na(cdata[c("DM", "ICC", "Anorexia", "Marasmo", "Institucionalizado", "Caquexia", "Etilismo", "Sem_dieta")])] <- 0
cdata[c("Traqueostomia")][is.na(cdata[c("Traqueostomia")])] <- 0

#replace NA on Extubado based on Traqueostomia
cdata[c("Extubado")][is.na(cdata[c("Extubado")])] <- ifelse(cdata$Traqueostomia[is.na(cdata[c("Extubado")])] > 0, 0, 1)

#replace NA on SOFA (1st eval) with SOFA_admissao
cdata[c("SOFA")][is.na(cdata[c("SOFA")])] <- cdata[c("SOFA_admissao")][is.na(cdata[c("SOFA")])]

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
#create list of datasets each one with some answer from mice imṕutation method
datasets <- prepare_copies(cdata, imputed_Data)

factorize_and_print <- function(datasets){
  #all these attributes are actually categorical/factor
  factors <- c("MI_hospital", "DG_principal", "Morbidade", "Morb_imuno",
               "VM1", "HD1", "Vasopressor1", "Sedoanalgesia1", "Previo_UTI",
               "MI_UTI", "FR_sind_realimentacao", "Dieta", "Alta_UTI", "Extubado",
               "Traqueostomia", "Obito")
  #so convert them into factors!
  for (k in 1:length(datasets)){
    datasets[[k]][,factors] <- data.frame(apply(datasets[[k]][,factors], 2, as.factor))
    write.csv(datasets[[k]], paste0("ds_", k, "_cluster.csv"))
  }
  
  datasets
}

datasets <- factorize_and_print(datasets)

ds1 <- datasets[[1]]
gower_dist <- daisy(ds1[,-ncol(ds1)],
                    metric = "gower")
summary(gower_dist)

pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

pam_results <- ds1 %>%
  dplyr::select(-Prontuario) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = ds1$Prontuario)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#ds1 <- read.csv("ds_1_cluster.csv",sep = ",", encoding="UTF-8", na.strings = "NA")
#ds1 <- ds1[c("Prontuario", "PCR1", "Lactato1", "Glicemia_min1")]

#verify classes of each attribute
#lapply(datasets[[1]], class)


