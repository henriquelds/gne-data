setwd("/home/henrique/Documents/gne-data/")
data <- read.csv("bxpeso_ml.csv",sep = ";", encoding="UTF-8")
dietas <- read.csv("desc_dietas.csv", sep=",", encoding="UTF-8")
names(data)
data[c("Dietas_nome")]

data[c("Dietas_nome")][is.na(data[c("Dietas_nome")])] <- 999
data[c("Dietas_nome")]
data$Morbidade <- ifelse(data$Cirrose + data$ICC + data$IRC_HD 
                         + data$SIDA + data$Cancer + data$Cancer_hematologico
                         +data$Pneumopatias + data$DM > 0, 1, 0)

data <- merge(data, dietas, by.x="Dietas_nome", by.y="Codigo", all.x = TRUE, suffixes = c("", ".D1"))

data[,(ncol(data)-4-1):ncol(data)]
names(data)[c(116:184)] <- paste("D1", colnames(data[116:184]), sep = "_")

write.csv(data, "bxpeso_ml_mod.csv")

data[c("Dietas_nome")]
