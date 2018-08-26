library(mice)
library(plyr)
library(cluster)
library(Rtsne)
library(dplyr)
library(ggplot2)

summary.numeric <- function (v, ...) {
  s <- summary.default(v)
  s <- signif(c(s,sd(v)),3)
  names(s)[length(s)] <- "sd"
  s
}

setwd("C:\\Users\\henri\\OneDrive\\Documents\\gne-data\\with_nutritional_facts")
data <- read.csv("..\\bxpeso_ml_mod.csv",sep = ",", encoding="UTF-8", na.strings = "NA")

names(data)

#remove second evaluation variables

cdata <- data[c(-1, -80:-111)] 
class(cdata$Dieta)
#cdata <- cdata[which(cdata$Dieta != 3),]
names(cdata)

#adjust some variables to be one-hot encoded factors
cdata <- cdata[c(-117:-152)]
cdata$D1_FL_GorduraLactea <- NULL
names(cdata)
toDummy <- c(88:104)
cdata[toDummy] <- ifelse(cdata[toDummy] > 0, "1", "0")
cdata[toDummy] <- lapply(cdata[toDummy], factor)

#remove duplicate fibras variables
cdata <- cdata[c(-84:-106, -115)]

names(cdata)

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

cdata$Repo_vitaminas <- ifelse(cdata$Tiamina1 + cdata$Vitaminas_outras1 > 0, 1, 0)

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

yellow <- c("Temp_max1", "Potassio1", "Repo_vitaminas","Total_kcal_peso1")

endVars <- c("Alta_UTI", "Extubado", "Traqueostomia", "Obito", "Prontuario", "Dietas_nome")

names(cdata)
#remove diet variables for now....
dietVars <- names(cdata)[c(84:91)]
#cdata <- cdata[c(-84:-145)] 

cdata <- cdata[c(pink, green, dietVars, endVars )]
names(cdata)
cdata[which(cdata$Dietas_nome==999), dietVars] <- NA

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
#create list of datasets each one with some answer from mice imputation method
datasets <- prepare_copies(cdata, imputed_Data)

factorize_and_print <- function(datasets){
  #all these attributes are actually categorical/factor
  factors <- c("MI_hospital", "DG_principal", "Morbidade", "Morb_imuno",
               "VM1", "HD1", "Vasopressor1", "Sedoanalgesia1", "Previo_UTI",
               "MI_UTI", "FR_sind_realimentacao", "Dieta", "Alta_UTI", "Extubado",
               "Traqueostomia", "Obito", "Dietas_nome")
  #so convert them into factors!
  for (k in 1:length(datasets)){
    datasets[[k]][,factors] <- data.frame(apply(datasets[[k]][,factors], 2, as.factor))
    write.csv(datasets[[k]], paste0("ds_", k, "_cluster.csv"))
  }
  
  datasets
}

datasets <- factorize_and_print(datasets)




ds1 <- datasets[[3]]
#to remove target attributes so the algorithm is not aware of them a priori
targetVars = c("Alta_UTI", "Obito", "Prontuario", "Dietas_nome")
#keep extubado and traqueostomia
myvars <- names(ds1) %in% targetVars
names(ds1[!myvars])

gower_dist <- daisy(ds1[!myvars],
                    metric = "gower")
summary(gower_dist)

# sil_width <- c(NA)

# for(i in 2:10){
#
#   pam_fit <- pam(gower_dist,
#                  diss = TRUE,
#                  k = i)
#
#   sil_width[i] <- pam_fit$silinfo$avg.width
#
# }
#
# # Plot sihouette width (higher is better)
#
# plot(1:10, sil_width,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:10, sil_width)

pam_fit <- pam(gower_dist, diss = TRUE, k = 5)
pam_fit$clustering


pam_results <- ds1 %>%
  dplyr::select(-one_of(targetVars)) %>%
  dplyr::select_if(is.numeric) %>%
  mutate(cluster = pam_fit$clustering, obito = ds1$Obito, altauti = ds1$Alta_UTI, dieta_cod = ds1$Dietas_nome) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

write.csv(na.omit(data.frame(pam_results$the_summary)), "ds_k5_removing_NPO_ObitoAltaUTI_sumario_onlynumeric.csv")

cdata[pam_fit$medoids[1],]

class(pam_results)

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = ds1$Prontuario, obito = ds1$Obito, dietas_cod = ds1$Dietas_nome)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster, shape = obito, size = 2))


#ds1 <- read.csv("ds_1_cluster.csv",sep = ",", encoding="UTF-8", na.strings = "NA")
#ds1 <- ds1[c("Prontuario", "PCR1", "Lactato1", "Glicemia_min1")]

#verify classes of each attribute
#lapply(datasets[[1]], class)


