install.packages(c("sysfonts", "showtext", "BiocManager", "gtable", "caTools",  "glmnet", "igraph", "smotefamily", "gtsummary",  "pbkrtest", "alookr", "ggfortify", "randomForest", "FactoMineR", "Boruta", "factoextra", "ggofortify", "randomForest", "praznik", "mboost", "mltools", "VIM", "pheatmap", "neuroCombat", "praznik", "caretEnsemble"))
install.packages("gdtools", type = "source")
devtools::install_github("dreamRs/esquisse")
install.packages("devtools", force = TRUE)
install.packages("gtsummary", force = TRUE)
install.packages("survMisc", force = TRUE)

library(devtools)
install_github("jfortin1/neuroCombat_Rpackage", force = TRUE)
BiocManager::install('RegParallel', force = TRUE)
devtools::install_github("RomeroBarata/bimba", force = TRUE)

#library(gtsummary)
library(smotefamily)
library(caTools)
library (gtable)
library("FactoMineR")
library("factoextra")
library(e1071)
library(survival)
library(RegParallel)
library(bimba)
library(purrr)
library(glmnet)
library(dplyr)  
library(magrittr)
library(knitr)
library(survMisc)
library(ggplot2)
library(ggfortify)
library(caTools)
library(data.table)
library(caret)
library(boot)
library(class)
library(randomForest)
library(ROCR)
library(pROC)
library(Boruta)
library(praznik)
library(xgboost)
library(mboost)
library(RColorBrewer)
library(mltools)
library(VIM)
library(readxl)
library(mice)
library(corrplot)
library(pheatmap)
library(alookr)
library(Boruta)
library(neuroCombat)
library(praznik)
library(caretEnsemble)
library(glmnet)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(readxl)    

'%ni%' <- Negate('%in%')

#######################Import Main Data#############

#Set to your working directory
setwd("D:/Workspace")

#Home
datapath <- "ICdata.xlsx"

lcdata <- read_excel(datapath, sheet = "ICPETt")
lcdata$Performance <- as.numeric (lcdata$PS)

#Rename AUC columns
names(lcdata)[names(lcdata) == "AUC-CSH"] <- "AUC_CSH"
names(lcdata)[names(lcdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(lcdata)[names(lcdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(lcdata)[names(lcdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(lcdata)[names(lcdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata)[names(lcdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(lcdata)[names(lcdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(lcdata)[names(lcdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(lcdata)[names(lcdata) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(lcdata)[names(lcdata) == "N voxels"] <- "N_voxels"

#Start of the feature columns
st_col_lcdata <- 35

for (i in 1:nrow(lcdata)){
  lcdata$Stage[i] <- substring(lcdata$Tstage[i], 1, 1)
  lcdata$N[i] <- substring(lcdata$Nstage[i], 1, 1)
  lcdata$M[i] <- substring(lcdata$Mstage[i], 1, 1)
}

#Import shell data
lcdata_shell <- read_excel(datapath, 
                           sheet = "ICPETs")

names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_shell)[names(lcdata_shell) == "N voxels"] <- "N_voxels"

#Import patch data
lcdata_patch <- read_excel(datapath, 
                           sheet = "ICPETp")

names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_patch)[names(lcdata_patch) == "N voxels"] <- "N_voxels"

#Combine the three PET masks
lcdata_interim <- merge(lcdata, lcdata_shell, by = "Scan name", all=TRUE)
lcdata_allfeatures_PET <- merge(lcdata_interim, lcdata_patch, by = "Scan name", all=TRUE)

rm(lcdata_interim, lcdata_shell, lcdata_patch)

#x: lesion, .y: shell, .: patch

############################CT Data##############################################

lcdata <- read_excel(datapath, sheet = "ICCTt")

#Rename AUC columns
names(lcdata)[names(lcdata) == "AUC-CSH"] <- "AUC_CSH"
names(lcdata)[names(lcdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(lcdata)[names(lcdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(lcdata)[names(lcdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(lcdata)[names(lcdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata)[names(lcdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(lcdata)[names(lcdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(lcdata)[names(lcdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(lcdata)[names(lcdata) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(lcdata)[names(lcdata) == "N voxels"] <- "N_voxels"

#Import shell data
lcdata_shell <- read_excel(datapath, 
                           sheet = "ICCTs")

names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(lcdata_shell)[names(lcdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_shell)[names(lcdata_shell) == "N voxels"] <- "N_voxels"

#Import patch data
lcdata_patch <- read_excel(datapath, 
                           sheet = "ICCTp")

names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(lcdata_patch)[names(lcdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(lcdata_patch)[names(lcdata_patch) == "N voxels"] <- "N_voxels"


#Combine the three CT masks
lcdata_interim <- merge(lcdata, lcdata_shell, by = "Scan name", all=TRUE)
lcdata_allfeatures_CT <- merge(lcdata_interim, lcdata_patch, by = "Scan name", all=TRUE)

rm(lcdata_interim, lcdata_shell, lcdata_patch)

#x: lesion, .y: shell, .: patch

#y: ct, x: pet
lcdata_allfeatures <- merge(lcdata_allfeatures_PET, lcdata_allfeatures_CT,by = "Scan name", all=TRUE)

lcdata <- subset(lcdata_allfeatures, !is.na(OS.Event))
rm(lcdata_allfeatures)

###################Import external validation sets############

#KCL

#Main lesion data
datapath <- "KCLdata.xlsx"

KCLdata <- read_excel(datapath, sheet = "KCLPETt")

names(KCLdata)[names(KCLdata) == "AUC-CSH"] <- "AUC_CSH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(KCLdata)[names(KCLdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(KCLdata)[names(KCLdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

KCLdata <- subset (KCLdata, Mask == 40)

KCLdata_shell <- read_excel(datapath, sheet = "KCLPETs")

names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(KCLdata_shell)[names(KCLdata_shell) == "N voxels"] <- "N_voxels"

KCLdata_shell <- subset (KCLdata_shell, Mask == 40)

KCLdata_patch <- read_excel(datapath, sheet = "KCLPETp")

names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(KCLdata_patch)[names(KCLdata_patch) == "N voxels"] <- "N_voxels"

KCLdata_interim <- merge(KCLdata, KCLdata_shell, by = "Scan name", all=TRUE)
KCLdata_allfeatures_PET <- merge(KCLdata_interim, KCLdata_patch, by = "Scan name", all=TRUE)

rm(KCLdata, KCLdata_interim, KCLdata_shell, KCLdata_patch)


KCLdata <- read_excel(datapath, sheet = "KCLCTt")

names(KCLdata)[names(KCLdata) == "AUC-CSH"] <- "AUC_CSH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(KCLdata)[names(KCLdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(KCLdata)[names(KCLdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(KCLdata)[names(KCLdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

KCLdata <- subset (KCLdata, Mask == 40)

KCLdata_shell <- read_excel(datapath, sheet = "KCLCTs")

names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(KCLdata_shell)[names(KCLdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(KCLdata_shell)[names(KCLdata_shell) == "N voxels"] <- "N_voxels"

KCLdata_shell <- subset (KCLdata_shell, Mask == 40)

KCLdata_patch <- read_excel(datapath, sheet = "KCLCTp")

names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(KCLdata_patch)[names(KCLdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(KCLdata_patch)[names(KCLdata_patch) == "N voxels"] <- "N_voxels"

KCLdata_interim <- merge(KCLdata, KCLdata_shell, by = "Scan name", all=TRUE)
KCLdata_allfeatures_CT <- merge(KCLdata_interim, KCLdata_patch, by = "Scan name", all=TRUE)

rm(KCLdata, KCLdata_interim, KCLdata_shell, KCLdata_patch)

#x: ct, .: pet
KCLdata_allfeatures <- merge(KCLdata_allfeatures_PET, KCLdata_allfeatures_CT,  by = "Scan name", all=TRUE)

KCLdata <- subset(KCLdata_allfeatures, !is.na(OS.Event))
rm(KCLdata_allfeatures)

st_col_KCLdata <- 18

########Marsden#################

datapath <- "Marsdendata.xlsx"

Marsdendata <- read_excel(datapath, sheet = "MarsdenPETt")

names(Marsdendata)[names(Marsdendata) == "AUC-CSH"] <- "AUC_CSH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

Marsdendata <- subset (Marsdendata, Mask == 40)

Marsdendata_shell <- read_excel(datapath, sheet = "MarsdenPETs")

names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "N voxels"] <- "N_voxels"

Marsdendata_shell <- subset (Marsdendata_shell, Mask == 40)

Marsdendata_patch <- read_excel(datapath, sheet = "MarsdenPETp")

names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "N voxels"] <- "N_voxels"

Marsdendata_interim <- merge(Marsdendata, Marsdendata_shell, by = "Scan name", all=TRUE)
Marsdendata_allfeatures_PET <- merge(Marsdendata_interim, Marsdendata_patch, by = "Scan name", all=TRUE)

rm(Marsdendata, Marsdendata_interim, Marsdendata_shell, Marsdendata_patch)


Marsdendata <- read_excel(datapath, sheet = "MarsdenCTt")

names(Marsdendata)[names(Marsdendata) == "AUC-CSH"] <- "AUC_CSH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Marsdendata)[names(Marsdendata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

Marsdendata <- subset (Marsdendata, Mask == 40)

Marsdendata_shell <- read_excel(datapath, sheet = "MarsdenCTs")

names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Marsdendata_shell)[names(Marsdendata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Marsdendata_shell)[names(Marsdendata_shell) == "N voxels"] <- "N_voxels"

Marsdendata_shell <- subset (Marsdendata_shell, Mask == 40)

Marsdendata_patch <- read_excel(datapath, sheet = "MarsdenCTp")

names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Marsdendata_patch)[names(Marsdendata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Marsdendata_patch)[names(Marsdendata_patch) == "N voxels"] <- "N_voxels"

Marsdendata_interim <- merge(Marsdendata, Marsdendata_shell, by = "Scan name", all=TRUE)
Marsdendata_allfeatures_CT <- merge(Marsdendata_interim, Marsdendata_patch, by = "Scan name", all=TRUE)

rm(Marsdendata, Marsdendata_interim, Marsdendata_shell, Marsdendata_patch)

#.: ct, x.: pet
Marsdendata_allfeatures <- merge(Marsdendata_allfeatures_PET, Marsdendata_allfeatures_CT, by = "Scan name", all=TRUE)

Marsdendata <- subset(Marsdendata_allfeatures, !is.na(OS.Event))
rm(Marsdendata_allfeatures)

st_col_Marsdendata <- 17

######Mount Vernon###################

datapath <- "MVdata.xlsx"

MVdata <- read_excel(datapath, sheet = "MVPETt")

names(MVdata)[names(MVdata) == "AUC-CSH"] <- "AUC_CSH"
names(MVdata)[names(MVdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(MVdata)[names(MVdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(MVdata)[names(MVdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(MVdata)[names(MVdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(MVdata)[names(MVdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(MVdata)[names(MVdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(MVdata)[names(MVdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

MVdata <- subset (MVdata, Mask == 40)

MVdata_shell <- read_excel(datapath, sheet = "MVPETs")

names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(MVdata_shell)[names(MVdata_shell) == "N voxels"] <- "N_voxels"

MVdata_shell <- subset (MVdata_shell, Mask == 40)

MVdata_patch <- read_excel(datapath, sheet = "MVPETp")

names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(MVdata_patch)[names(MVdata_patch) == "N voxels"] <- "N_voxels"

MVdata_interim <- merge(MVdata, MVdata_shell, by = "Scan name", all=TRUE)
MVdata_allfeatures_PET <- merge(MVdata_interim, MVdata_patch, by = "Scan name", all=TRUE)

rm(MVdata, MVdata_interim, MVdata_shell, MVdata_patch)


MVdata <- read_excel(datapath, sheet = "MVCTt")

names(MVdata)[names(MVdata) == "AUC-CSH"] <- "AUC_CSH"
names(MVdata)[names(MVdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(MVdata)[names(MVdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(MVdata)[names(MVdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(MVdata)[names(MVdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(MVdata)[names(MVdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(MVdata)[names(MVdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(MVdata)[names(MVdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

MVdata <- subset (MVdata, Mask == 40)

MVdata_shell <- read_excel(datapath, sheet = "MVCTs")

names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(MVdata_shell)[names(MVdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(MVdata_shell)[names(MVdata_shell) == "N voxels"] <- "N_voxels"

MVdata_shell <- subset (MVdata_shell, Mask == 40)

MVdata_patch <- read_excel(datapath, sheet = "MVCTp")

names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(MVdata_patch)[names(MVdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(MVdata_patch)[names(MVdata_patch) == "N voxels"] <- "N_voxels"

MVdata_interim <- merge(MVdata, MVdata_shell, by = "Scan name", all=TRUE)
MVdata_allfeatures_CT <- merge(MVdata_interim, MVdata_patch, by = "Scan name", all=TRUE)

rm(MVdata, MVdata_interim, MVdata_shell, MVdata_patch)

#.: ct, x: pet
MVdata_allfeatures <- merge(MVdata_allfeatures_PET, MVdata_allfeatures_CT, by = "Scan name", all=TRUE)

MVdata <- subset(MVdata_allfeatures, !is.na(OS.Event))
rm(MVdata_allfeatures)

st_col_MVdata <- 16

###Nottingham###############

datapath <- "Nottdata.xlsx"

Nottdata <- read_excel(datapath, sheet = "NottPETt")

names(Nottdata)[names(Nottdata) == "AUC-CSH"] <- "AUC_CSH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Nottdata)[names(Nottdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Nottdata)[names(Nottdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

Nottdata <- subset (Nottdata, Mask == 40)

Nottdata_shell <- read_excel(datapath, sheet = "NottPETs")

names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Nottdata_shell)[names(Nottdata_shell) == "N voxels"] <- "N_voxels"

Nottdata_shell <- subset (Nottdata_shell, Mask == 40)

Nottdata_patch <- read_excel(datapath, sheet = "NottPETp")

names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Nottdata_patch)[names(Nottdata_patch) == "N voxels"] <- "N_voxels"

Nottdata_interim <- merge(Nottdata, Nottdata_shell, by = "Scan name", all=TRUE)
Nottdata_allfeatures_PET <- merge(Nottdata_interim, Nottdata_patch, by = "Scan name", all=TRUE)

rm(Nottdata, Nottdata_interim, Nottdata_shell, Nottdata_patch)


Nottdata <- read_excel(datapath, sheet = "NottCTt")

names(Nottdata)[names(Nottdata) == "AUC-CSH"] <- "AUC_CSH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Nottdata)[names(Nottdata) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Nottdata)[names(Nottdata) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Nottdata)[names(Nottdata) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"

Nottdata <- subset (Nottdata, Mask == 40)

Nottdata_shell <- read_excel(datapath, sheet = "NottCTs")

names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH"] <- "AUC_CSH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Nottdata_shell)[names(Nottdata_shell) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Nottdata_shell)[names(Nottdata_shell) == "N voxels"] <- "N_voxels"

Nottdata_shell <- subset (Nottdata_shell, Mask == 40)

Nottdata_patch <- read_excel(datapath, sheet = "NottCTp")

names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH"] <- "AUC_CSH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LHL"] <- "AUC_CSH_LHL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LHH"] <- "AUC_CSH_LHH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LLH"] <- "AUC_CSH_LLH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_LLL"] <- "AUC_CSH_LLL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HLL"] <- "AUC_CSH_HLL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HLH"] <- "AUC_CSH_HLH"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HHL"] <- "AUC_CSH_HHL"
names(Nottdata_patch)[names(Nottdata_patch) == "AUC-CSH_HHH"] <- "AUC_CSH_HHH"
names(Nottdata_patch)[names(Nottdata_patch) == "N voxels"] <- "N_voxels"

Nottdata_interim <- merge(Nottdata, Nottdata_shell, by = "Scan name", all=TRUE)
Nottdata_allfeatures_CT <- merge(Nottdata_interim, Nottdata_patch, by = "Scan name", all=TRUE)

rm(Nottdata, Nottdata_interim, Nottdata_shell, Nottdata_patch)

#y.: ct, x: pet
Nottdata_allfeatures <- merge(Nottdata_allfeatures_PET, Nottdata_allfeatures_CT, by = "Scan name", all=TRUE)

Nottdata <- subset(Nottdata_allfeatures, !is.na(OS.Event))
rm(Nottdata_allfeatures)

st_col_Nottdata <- 16


#######Patient demographics - summary statistics######################

#Summary statistics
mean (as.numeric(lcdata$Age))
sd (as.numeric(lcdata$Age))
max (as.numeric(lcdata$Age))
min (as.numeric(lcdata$Age))

tbl <- table(lcdata$Sex)
cbind(tbl,prop.table(tbl))

tbl <- table(lcdata$Stage)
cbind(tbl,prop.table(tbl))

tbl <- table(lcdata$N)
cbind(tbl,prop.table(tbl))

tbl <- table(lcdata$M)
cbind(tbl,prop.table(tbl))

tbl <- table(lcdata$Histology)
cbind(tbl,prop.table(tbl))

tbl <- table(lcdata$PS)
cbind(tbl,prop.table(tbl))

#Summary statistics - KCL
mean (as.numeric(KCLdata$Age))
sd (as.numeric(KCLdata$Age))
max (as.numeric(KCLdata$Age))
min (as.numeric(KCLdata$Age))

tbl <- table(KCLdata$Sex)
cbind(tbl,prop.table(tbl))

tbl <- table(KCLdata$PS)
cbind(tbl,prop.table(tbl))

tbl <- table(KCLdata$Tstage)
cbind(tbl,prop.table(tbl))

tbl <- table(KCLdata$Nstage)
cbind(tbl,prop.table(tbl))

tbl <- table(KCLdata$Mstage)
cbind(tbl,prop.table(tbl))

tbl <- table(KCLdata$Histo)
cbind(tbl,prop.table(tbl))

wilcox.test(x = lcdata$Age, y = as.numeric(KCLdata$Age),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = lcdata$Sex, y = KCLdata$Sex,
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Stage), y = as.numeric(KCLdata$Tstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$N), y = as.numeric(KCLdata$Nstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$M), y = as.numeric(KCLdata$Mstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Histology), y = as.numeric(KCLdata$Histo),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

#Summary statistics - Marsden

Marsdendata_new <- Marsdendata
Marsdendata <- Marsdendata_new

Marsdendata <- Marsdendata[-33,]
Marsdendata <- Marsdendata[-12,]
Marsdendata <- Marsdendata[-37,]

Marsdendata <- Marsdendata[!is.na(Marsdendata$NumericStage),]

mean(as.numeric(Marsdendata$Age), na.rm = TRUE)
sd (as.numeric(Marsdendata$Age), na.rm = TRUE)
max (as.numeric(Marsdendata$Age), na.rm = TRUE)
min (as.numeric(Marsdendata$Age), na.rm = TRUE)

tbl <- table(Marsdendata$Sex)
cbind(tbl,prop.table(tbl))

Marsdendata$Stage <- as.numeric(gsub("\\D", "", Marsdendata$Tstage))


tbl <- table(Marsdendata$Stage)
cbind(tbl,prop.table(tbl))

tbl <- table(Marsdendata$Nstage)
cbind(tbl,prop.table(tbl))

tbl <- table(Marsdendata$Mstage)
cbind(tbl,prop.table(tbl))

tbl <- table(Marsdendata$Histo)
cbind(tbl,prop.table(tbl))

wilcox.test(x = lcdata$Age, y = as.numeric(Marsdendata$Age),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = lcdata$Sex, y = Marsdendata$Sex,
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Stage), y = as.numeric(Marsdendata$Stage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$N), y = as.numeric(Marsdendata$Nstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$M), y = as.numeric(Marsdendata$Mstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Histology), y = as.numeric(Marsdendata$Histo),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

#Summary statistics - Mount Vernon

mean(as.numeric(MVdata$Age), na.rm = TRUE)
sd (as.numeric(MVdata$Age), na.rm = TRUE)
max (as.numeric(MVdata$Age), na.rm = TRUE)
min (as.numeric(MVdata$Age), na.rm = TRUE)

tbl <- table(MVdata$Sex)
cbind(tbl,prop.table(tbl))

MVdata$Stage <- as.numeric(gsub("\\D", "", MVdata$Tstage))

tbl <- table(MVdata$Stage)
cbind(tbl,prop.table(tbl))

tbl <- table(MVdata$Nstage)
cbind(tbl,prop.table(tbl))

tbl <- table(MVdata$Mstage)
cbind(tbl,prop.table(tbl))

tbl <- table(MVdata$Histo)
cbind(tbl,prop.table(tbl))

wilcox.test(x = lcdata$Age, y = as.numeric(MVdata$Age),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = lcdata$Sex, y = MVdata$Sex,
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Stage), y = as.numeric(MVdata$Stage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$N), y = as.numeric(MVdata$Nstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$M), y = as.numeric(MVdata$Mstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Histology), y = as.numeric(MVdata$Histo),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

#Summary statistics - Nottingham

mean(as.numeric(Nottdata$Age), na.rm = TRUE)
sd (as.numeric(Nottdata$Age), na.rm = TRUE)
max (as.numeric(Nottdata$Age), na.rm = TRUE)
min (as.numeric(Nottdata$Age), na.rm = TRUE)

tbl <- table(Nottdata$Sex)
cbind(tbl,prop.table(tbl))

Nottdata$Stage <- as.numeric(gsub("\\D", "", Nottdata$Tstage))

tbl <- table(Nottdata$Stage)
cbind(tbl,prop.table(tbl))

tbl <- table(Nottdata$Nstage)
cbind(tbl,prop.table(tbl))

tbl <- table(Nottdata$Mstage)
cbind(tbl,prop.table(tbl))

tbl <- table(Nottdata$Histo)
cbind(tbl,prop.table(tbl))

wilcox.test(x = lcdata$Age, y = as.numeric(Nottdata$Age),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = lcdata$Sex, y = Nottdata$Sex,
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Stage), y = as.numeric(Nottdata$Stage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$N), y = as.numeric(Nottdata$Nstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$M), y = as.numeric(Nottdata$Mstage),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)

wilcox.test(x = as.numeric(lcdata$Histology), y = as.numeric(Nottdata$Histo),
            mu=0, alt="two.sided", conf.int=T, conf.level=0.95,
            paired=FALSE, exact=T, correct=T)


#####Convert all feature columns to numeric########

lcdata[, st_col_lcdata:ncol(lcdata)] <- sapply(lcdata[, st_col_lcdata:ncol(lcdata)], as.factor)
lcdata[, st_col_lcdata:ncol(lcdata)] <- sapply(lcdata[, st_col_lcdata:ncol(lcdata)], as.numeric)

lcdata$Numeral <- as.factor(lcdata$Numeral)

KCLdata[, st_col_KCLdata:ncol(KCLdata)] <- sapply(KCLdata[, st_col_KCLdata:ncol(KCLdata)], as.factor)
KCLdata[, st_col_KCLdata:ncol(KCLdata)] <- sapply(KCLdata[, st_col_KCLdata:ncol(KCLdata)], as.numeric)

KCLdata$Numeral <- as.factor(KCLdata$NumericStage)

Marsdendata[, st_col_Marsdendata:ncol(Marsdendata)] <- sapply(Marsdendata[, st_col_Marsdendata:ncol(Marsdendata)], as.factor)
Marsdendata[, st_col_Marsdendata:ncol(Marsdendata)] <- sapply(Marsdendata[, st_col_Marsdendata:ncol(Marsdendata)], as.numeric)

Marsdendata$Numeral <- as.factor(Marsdendata$NumericStage)

MVdata[, st_col_MVdata:ncol(MVdata)] <- sapply(MVdata[, st_col_MVdata:ncol(MVdata)], as.factor)
MVdata[, st_col_MVdata:ncol(MVdata)] <- sapply(MVdata[, st_col_MVdata:ncol(MVdata)], as.numeric)

MVdata$Numeral <- as.factor(MVdata$NumericStage)

Nottdata[, st_col_Nottdata:ncol(Nottdata)] <- sapply(Nottdata[, st_col_Nottdata:ncol(Nottdata)], as.factor)
Nottdata[, st_col_Nottdata:ncol(Nottdata)] <- sapply(Nottdata[, st_col_Nottdata:ncol(Nottdata)], as.numeric)

Nottdata$Numeral <- as.factor(Nottdata$NumericStage)

covariates <- c(colnames(lcdata))
covariates <- covariates[st_col_lcdata:ncol(lcdata)]

#############
### SPLIT DATA INTO TRAINING AND EXTERNAL VALIDATION 
set.seed(43)

data <- lcdata

split = sample.split(data$OS, data$OS.Event, SplitRatio = 0.7)
train_set_full <- subset(data, split == T)
val_set_full <- subset(data, split == F)
train_set_full <- as.data.frame(train_set_full)
val_set_full <- as.data.frame(val_set_full)

st_col <- st_col_lcdata

#SCALE DATA

r_train_mean <- colMeans(train_set_full[st_col:ncol(train_set_full)]) ## calulate the mean of the numeric features, note not the categorical features
r_train_std <- sapply(train_set_full[st_col:ncol(train_set_full)], sd, na.rm = TRUE) ## Standard deviation
r_scaled_train = as.data.frame(scale(train_set_full[st_col:ncol(train_set_full)], center=r_train_mean, scale= r_train_std))
r_scaled_val  = as.data.frame(scale(val_set_full[st_col:ncol(train_set_full)], center= r_train_mean, scale= r_train_std)) # scale the external valdiation using training data set statistics

LC_train <- cbind(train_set_full[, 1:st_col_lcdata], r_scaled_train)
LC_val <- cbind(val_set_full[, 1:st_col_lcdata], r_scaled_val)

KCL_mean <- colMeans(KCLdata[st_col_KCLdata:ncol(KCLdata)]) ## calulate the mean of the numeric features, note not the categorical features
KCL_std <- sapply(KCLdata[st_col_KCLdata:ncol(KCLdata)], sd, na.rm = TRUE) ## Standard deviation
r_scaled_KCL  = as.data.frame(scale(KCLdata[st_col_KCLdata:ncol(KCLdata)], center= KCL_mean, scale= KCL_std)) 

KCL_nor <-cbind(KCLdata[, 1:st_col_KCLdata], r_scaled_KCL)

Marsden_mean <- colMeans(Marsdendata[st_col_Marsdendata:ncol(Marsdendata)]) ## calulate the mean of the numeric features, note not the categorical features
Marsden_std <- sapply(Marsdendata[st_col_Marsdendata:ncol(Marsdendata)], sd, na.rm = TRUE) ## Standard deviation
r_scaled_Marsden  = as.data.frame(scale(Marsdendata[st_col_Marsdendata:ncol(Marsdendata)], center= Marsden_mean, scale= Marsden_std)) 

Marsden_nor <-cbind(Marsdendata[, 1:st_col_Marsdendata], r_scaled_Marsden)

MV_mean <- colMeans(MVdata[st_col_MVdata:ncol(MVdata)]) ## calulate the mean of the numeric features, note not the categorical features
MV_std <- sapply(MVdata[st_col_MVdata:ncol(MVdata)], sd, na.rm = TRUE) ## Standard deviation
r_scaled_MV  = as.data.frame(scale(MVdata[st_col_MVdata:ncol(MVdata)], center= MV_mean, scale= MV_std)) 

MV_nor <-cbind(MVdata[, 1:st_col_MVdata], r_scaled_MV)

Nott_mean <- colMeans(Nottdata[st_col_Nottdata:ncol(Nottdata)]) ## calulate the mean of the numeric features, note not the categorical features
Nott_std <- sapply(Nottdata[st_col_Nottdata:ncol(Nottdata)], sd, na.rm = TRUE) ## Standard deviation
r_scaled_Nott  = as.data.frame(scale(Nottdata[st_col_Nottdata:ncol(Nottdata)], center= Nott_mean, scale= Nott_std)) 

Nott_nor <-cbind(Nottdata[, 1:st_col_Nottdata], r_scaled_Nott)

##############Univariable FDR Step############################

library("survival")
library("survminer")

LC_train_nor <- LC_train

covariates_new <- covariates
removal <- ""

for (x in st_col_lcdata:ncol(LC_train_nor)) {
  if ((LC_train_nor[2,x] == "NaN") || (is.na(LC_train_nor[2,x]))){
      print(colnames(LC_train_nor)[x])
      #print(x)
      removal <- append(removal, x-35) 
  }
}

covariates_new = covariates_new[-c(as.numeric(removal)[2:length(removal)])]

univ_formulas <- sapply(covariates_new, function(x) as.formula(paste('Surv(OS, OS.Event)~', x)))
univ_models <- lapply(univ_formulas, function(x){coxph(x, data = LC_train_nor)})

univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=5)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coefficient beta
                         HR <-signif(x$coef[2], digits=5);#exp(beta)
                         HR_num <- HR
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR_num, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                       })

res <- t(as.data.frame(univ_results, check.names = FALSE))
Coxresult <- as.data.frame(res)

x <- as.numeric(Coxresult$HR)
y <- as.numeric(Coxresult$p.value)

plot(x, y, xlab = "HR", ylab = "p-value", col=rgb(0.4,0.4,0.8,0.6),pch=16 , cex=0.1); 

abline(v=1,  lty = 2, col="red")
text(x=0.98, y=0.5, "HR = 1.0", srt = 90)

abline(h=0.05, lty = 2, col="red")
text(x=1.0, y=0.09, "FDR = 0.05", srt = 0)

rm(res)
rm(univ_models)
rm(univ_results)
rm(univ_formulas)

#####Remove NA cases####################
LC_train=LC_train[complete.cases(LC_train[,c("OS","OS.Event")]),]
LC_val=LC_val[complete.cases(LC_val[,c("OS","OS.Event")]),]

#replace NAs with 0s
LC_train[is.na(LC_train)] <- 0
LC_val[is.na(LC_val)] <- 0

#############################################################
#LASSO

p_thres <- 0.00000001

forlasso <- LC_train[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]

nFolds <- 50
foldid <- sample(rep(seq(nFolds), length.out = nrow(LC_train)))

cvfit <- cv.glmnet(x=as.matrix(forlasso), y= Surv(LC_train$"OS", LC_train$"OS.Event"), family="cox",alpha=1,  nfolds = nFolds, foldid = foldid)
plot(cvfit)

#Get cross validated R squared for goodness of fit
rsq = 1 - cvfit$cvm/var(y)
plot(cvfit$lambda,rsq)

fit <- glmnet(x=as.matrix(forlasso), y= Surv(LC_train$"OS", LC_train$"OS.Event"), family="cox",alpha=1,nfolds = nFolds, lambda=cvfit$lambda.min, foldid = foldid)
fit$beta[,1]

coef(fit)
tidy(fit)

#Predicting RPV

#training
prediction_model_tr <- predict(fit, newx=as.matrix(LC_train[ ,row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]), cvfit$lambda.min)
LC_train$RPV <- prediction_model_tr[,1]

#interval validation
prediction_model_val <- predict(fit, newx=as.matrix(LC_val[ ,row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]), cvfit$lambda.min)
LC_val$RPV <- prediction_model_val[,1]

#external testing
prediction_KCL <- predict(fit, newx=as.matrix(KCL_nor[ ,row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]), cvfit$lambda.min)
KCL_nor$RPV <- prediction_KCL[,1]

prediction_Marsden <- predict(fit, newx=as.matrix(Marsden_nor[ ,row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]), cvfit$lambda.min)
Marsden_nor$RPV <- prediction_Marsden [,1]

prediction_MV <- predict(fit, newx=as.matrix(MV_nor[ ,row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]), cvfit$lambda.min)
MV_nor$RPV <- prediction_MV[,1]

prediction_Nott <- predict(fit, newx=as.matrix(Nott_nor[ ,row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]), cvfit$lambda.min)
Nott_nor$RPV <- prediction_Nott[,1]

##########Correlation Plots######################################
install.packages("corrplot")
library(corrplot)

install.packages("GGally")
library(GGally)

#Merge training and internal val
LC_combined <- rbind(LC_train, LC_val)

#MTV40, SUVmax, NHOC, RPV
MTV40 <- as.numeric(LC_combined$MTVvolume)
SUVmax <- as.numeric(LC_combined$SUVmax)
NHOC <- as.numeric(LC_combined$NHOC)
RPV <- as.numeric(LC_combined$RPV)

data <- data.frame(NHOC, RPV, MTV40, SUVmax)

ggpairs(data,
        lower = list(continuous = wrap("smooth", method = "lm", se = FALSE)),  # Regression lines
        upper = list(continuous = wrap("cor", size = 5)),  # Correlation coefficients
        diag = list(continuous = wrap("densityDiag")))  # Density plots on diagonal

##############################Multivariable Cox Model#######################

################Multivariable Predictor###############################

#Do LASSO With RPV, NHOC and Clinical (Age, Performance Status and Stage)
install.packages("survival")
library(survival)

LC_train$NHOC <- as.numeric(LC_train$NHOC)
LC_train$PS <- as.numeric(LC_train$PS)
LC_train$Numeral <- as.factor(LC_train$Numeral)
fit2 <- coxph(Surv(OS, OS.Event) ~ Age + NHOC + Numeral + RPV,
             data=LC_train)

LC_train$composite <- predict(fit2, newdata=LC_train, type = "lp")

LC_val$NHOC <- as.numeric(LC_val$NHOC)
LC_val$PS <- as.numeric(LC_val$PS)
LC_val$Numeral <- as.factor(LC_val$Numeral)
LC_val$composite <- predict(fit2, newdata=LC_val, type = "lp")

KCL_nor$composite <- predict(fit2, newdata=KCL_nor, type = "lp")
Marsden_nor$composite <- predict(fit2, newdata=Marsden_nor, type = "lp")
MV_nor$composite <- predict(fit2, newdata=MV_nor, type = "lp")
Nott_nor$composite <- predict(fit2, newdata=Nott_nor, type = "lp")

#################Draw Forest Plot for Multivariable Regression##################
library(forestplot)
library(caret)

LC_dis <- rbind (LC_train, LC_val)

LC_dis$PS <- as.factor(LC_dis$PS)

set.seed(100)

LC_dis$Age <- as.numeric(LC_dis$Age)
LC_dis$`Performance Status` <- as.numeric(LC_dis$PS)
LC_dis$Stage <- as.numeric(LC_dis$Numeral)
LC_dis$RPV <- as.numeric(LC_dis$RPV)
LC_dis$NHOC <- as.numeric(LC_dis$NHOC)

logitmod1 <- coxph(Surv(OS, OS.Event) ~ Age + `Performance Status` + Stage + RPV + NHOC, data = LC_dis) 

summary(logitmod1)
confint(logitmod1)

library(tidyverse)
library(forestmodel)

plot1 <- forest_model(logitmod1)
plot1

logitmod2 <- coxph(Surv(OS, OS.Event) ~ Age, data = LC_dis) 
logitmod3 <- coxph(Surv(OS, OS.Event) ~ `Performance Status`, data = LC_dis) 
logitmod4 <- coxph(Surv(OS, OS.Event) ~ Stage, data = LC_dis) 
logitmod5 <- coxph(Surv(OS, OS.Event) ~ RPV, data = LC_dis) 
logitmod6 <- coxph(Surv(OS, OS.Event) ~ NHOC, data = LC_dis) 

plot2 <- forest_model(logitmod2)
plot3 <- forest_model(logitmod3)
plot4 <- forest_model(logitmod4)
plot5 <- forest_model(logitmod5)
plot6 <- forest_model(logitmod6)

logitmod <- glm(formula = His ~ Gender + Ethnicity + `EGFR-RPV`, family = binomial(link = "logit"), data = T_roc)

library(ggpubr)

ggarrange(plot2, plot3, plot4, plot5, plot6, ncol=1, nrow=5)

##################################AUROC Plots###################################

library(ROCit)

#Plot SUVmax
cate_dis <- as.numeric(LC_combined$OS.Event)

cate_dis <- as.numeric(LC_val$OS.Event)

for (i in 1:length(cate_dis)) {
  if (cate_dis[i] == "1"){
    cate_dis[i] <- '+'
  } else {
    cate_dis[i] <- '-'
  }
}

#Plot SUVmax
score_dis <- as.numeric(LC_combined$MTVvolume)

score_dis <- as.numeric(LC_combined$composite)
score_dis <- as.numeric(LC_val$SUVmax)

#Plot MTV40
score_dis <- as.numeric(LC_val$MTVvolume)

score_dis <-  as.numeric(LC_val$composite)
score_dis <-  as.numeric(LC_val$NHOC)
score_dis <-  as.numeric(LC_val$RPV)

ROCit_obj <- rocit(score=score_dis, class=cate_dis, negref = "-", method ="bin")

AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
#title("NHOC: 3 Year Overall Survival - Discovery")
#title("RPV: 3 Year Overall Survival - KCL")
#title("SUVmax: 3 Year Overall Survival - Internal Validation")
#title("nLCEV: 3 Year Overall Survival - Internal Validation")
#title("NHOC: 3 Year Overall Survival - Internal Validation")
title("RPV: 3 Year Overall Survival - Internal Validation")

#KCL
cate_dis <- as.numeric(KCL_nor$OS.Event)

for (i in 1:length(cate_dis)) {
  if (cate_dis[i] == "0"){
    cate_dis[i] <- '+'
  } else {
    cate_dis[i] <- '-'
  }
}

score_dis <- KCL_nor$composite
score_dis <- KCL_nor$RPV
score_dis <- as.numeric(KCL_nor$NHOC)

#Marsden
cate_dis <- as.numeric(Marsden_nor$OS.Event)

for (i in 1:length(cate_dis)) {
  if (cate_dis[i] == "1"){
    cate_dis[i] <- '+'
  } else {
    cate_dis[i] <- '-'
  }
}

score_dis <- Marsden_nor$composite
score_dis <- Marsden_nor$RPV
score_dis <- as.numeric(Marsden_nor$NHOC)

#MV
cate_dis <- as.numeric(MV_nor$OS.Event)

for (i in 1:length(cate_dis)) {
  if (cate_dis[i] == "1"){
    cate_dis[i] <- '+'
  } else {
    cate_dis[i] <- '-'
  }
}

score_dis <- MV_nor$composite
score_dis <- MV_nor$RPV
score_dis <- as.numeric(MV_nor$NHOC)

#Nott
cate_dis <- as.numeric(Nott_nor$OS.Event)

for (i in 1:length(cate_dis)) {
  if (cate_dis[i] == "1"){
    cate_dis[i] <- '+'
  } else {
    cate_dis[i] <- '-'
  }
}

score_dis <- Nott_nor$composite
score_dis <- Nott_nor$RPV
score_dis <- as.numeric(Nott_nor$NHOC)

ROCit_obj <- rocit(score=score_dis, class=cate_dis, negref = "-", method ="bin")

AUC_obj <- ciAUC(ROCit_obj, level = 0.95)
p <- plot(ROCit_obj)
text(0.95, 0.2, paste0("AUC=", round(AUC_obj$AUC, 2), ", 95% CI [", round(AUC_obj$lower, 2), ",", round(AUC_obj$upper, 2), "]"), adj = 1, font = 4, cex=1.0)
#title("NHOC: 3 Year Overall Survival - Discovery")
#title("RPV: 3 Year Overall Survival - KCL")
title("Composite - RPV: 3 Year Overall Survival - KCL")

##################################Survival Plots################################

############prediction k means function######################
predict.kmeans <- function(object, newdata){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

########################Clustering #################

library(ggkm)
library(survival)
library(survminer)

install.packages("glue")
# Install dplyr (contains %>%)
install.packages("dplyr")
library(dplyr)

no_partition <- 5


####Combined Discovery Cohort######################
lassomat <- LC_combined[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, LC_combined$composite)
sig_array <- fit$beta@i
postlasso <- lassomat[, fit$beta@i]

set.seed(20)
km.res <- kmeans(postlasso, no_partition)
LC_combined$RPV_Grouping <- km.res$cluster
Dis_final <- LC_combined

#KCL
lassomat <- KCL_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, KCL_nor$composite)
postlasso <- lassomat[, fit$beta@i]
set.seed(300)
no_partition <- 7
km.res <- kmeans(postlasso, no_partition, nstart = 1)
KCL_nor$RPV_Grouping <- km.res$cluster
Dis_final <- KCL_nor

#Marsden

Marsden_nor <- Marsden_nor[!is.na(Marsden_nor$NumericStage),]

lassomat <- Marsden_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, Marsden_nor$composite)
postlasso <- lassomat[, fit$beta@i]
set.seed(500)
no_partition <- 5
km.res <- kmeans(postlasso, no_partition, nstart = 1)
Marsden_nor$RPV_Grouping <- km.res$cluster
Dis_final <- Marsden_nor

#Mount Vernon
lassomat <- MV_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, MV_nor$composite)
postlasso <- lassomat[, fit$beta@i]
set.seed(300)
no_partition <- 7
#set.seed(250)
#no_partition <- 3
km.res <- kmeans(postlasso, no_partition, nstart = 1)
MV_nor$RPV_Grouping <- km.res$cluster
Dis_final <- MV_nor

#Nottingham
lassomat <- Nott_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, Nott_nor$composite)
postlasso <- lassomat[, fit$beta@i]
set.seed(300)
no_partition <- 7
km.res <- kmeans(postlasso, no_partition, nstart = 1)
Nott_nor$RPV_Grouping <- km.res$cluster
Dis_final <- Nott_nor

#############For Supplementary Figures################

#Discovery
lassomat <- LC_combined[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, LC_combined$SUVmax)
sig_array <- fit$beta@i
postlasso <- lassomat[, fit$beta@i]

set.seed(20)
km.res <- kmeans(postlasso, no_partition)
LC_combined$RPV_Grouping <- km.res$cluster

Dis_final <- LC_combined

#KCL
lassomat <- KCL_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, KCL_nor$SUVmax)

postlasso <- lassomat[, fit$beta@i]
set.seed(300)
no_partition <- 7
km.res <- kmeans(postlasso, no_partition, nstart = 1)
KCL_nor$RPV_Grouping <- km.res$cluster
Dis_final <- KCL_nor

#Marsden

lassomat <- Marsden_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, Marsden_nor$SUVmax)
postlasso <- lassomat[, fit$beta@i]
set.seed(300)
no_partition <- 5
km.res <- kmeans(postlasso, no_partition, nstart = 1)
Marsden_nor$RPV_Grouping <- km.res$cluster
Dis_final <- Marsden_nor

#Mount Vernon
lassomat <- MV_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, MV_nor$SUVmax)
postlasso <- lassomat[, fit$beta@i]
set.seed(300)
no_partition <- 7

set.seed(250)
no_partition <- 3
km.res <- kmeans(postlasso, no_partition, nstart = 1)
MV_nor$RPV_Grouping <- km.res$cluster
Dis_final <- MV_nor

#Nottingham
lassomat <- Nott_nor[ , row.names(subset(Coxresult, as.numeric(p.value)<p_thres))]
lassomat <- cbind(lassomat, Nott_nor$SUVmax)
postlasso <- lassomat[, fit$beta@i]
set.seed(300)
no_partition <- 7
km.res <- kmeans(postlasso, no_partition, nstart = 1)
Nott_nor$RPV_Grouping <- km.res$cluster
Dis_final <- Nott_nor

#Plot

ggkm(survfit(Surv(OS,OS.Event)~RPV_Grouping,data= Dis_final),main = "Kaplan-Meier Plots of Groups Stratified Using \n LC-RPV - Discovery Cohort", margins=c(12,15), pval=T,table=T, legend = T, legendposition = c(0.85, 0.15), ystrataname = "Group", xlab = "Time-to-Event (Days)", cex = 0.8, marks=T)

#re-sort groups

for (i in 1:nrow(Dis_final)){
  #Discovery Cohort
  #if ((Dis_final$RPV_Grouping[i] == 1) |(Dis_final$RPV_Grouping[i] == 3) | (Dis_final$RPV_Grouping[i] == 7)) {
  #KCL
  #if ((Dis_final$RPV_Grouping[i] == 2) | (Dis_final$RPV_Grouping[i] == 7)) {  
  #if ((Dis_final$RPV_Grouping[i] == 2) | (Dis_final$RPV_Grouping[i] == 7) | (Dis_final$RPV_Grouping[i] == 3)) {  
  #Marsden
  #if ((Dis_final$RPV_Grouping[i] == 2) | (Dis_final$RPV_Grouping[i] == 5)) {  
  if ((Dis_final$RPV_Grouping[i] == 5)) {  
  #MV
  #if  ((Dis_final$RPV_Grouping[i] == 4) | (Dis_final$RPV_Grouping[i] == 7) | (Dis_final$RPV_Grouping[i] == 2)) {  
  #if ((Dis_final$RPV_Grouping[i] == 4) | (Dis_final$RPV_Grouping[i] == 7)) {
  #MV
  #if  ((Dis_final$RPV_Grouping[i] == 2)) {  
  #if ((Dis_final$RPV_Grouping[i] == 1) | (Dis_final$RPV_Grouping[i] == 2)) {
    Dis_final$RPV_Grouping_Final[i] <- 2
  }
  else{
    Dis_final$RPV_Grouping_Final[i] <- 1
  }
}

Dis_final$`nLCEV`<-Dis_final$RPV_Grouping_Final
coxph(Surv(OS,OS.Event) ~ `nLCEV`, data= Dis_final) %>% 
  gtsummary::tbl_regression(exp = TRUE) 

Dis_final$`SUVmax`<-Dis_final$RPV_Grouping_Final
coxph(Surv(OS,OS.Event) ~ `SUVmax`, data= Dis_final) %>% 
  gtsummary::tbl_regression(exp = TRUE) 


theme <- theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = "white"),
               panel.grid.minor = element_line(colour = "white"),
               panel.border = element_blank(),
               panel.background = element_blank()) 

#Fancy KM Plot
a<-ggsurvplot(
  survfit(Surv(OS, OS.Event) ~ RPV_Grouping_Final, data= Dis_final),     # survfit object with calculated statistics.
  data = Dis_final,               # data used to fit survival curves. 
  pval = FALSE,             # show p-value of log-rank test.
  conf.int = FALSE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,1095),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 100,     # break X axis in time intervals by 100.
  ggtheme = theme,         # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = T, # show bars instead of names in text annotations
  # in legend of risk table
  legend.labs=c("Low Risk", "High Risk"),
  risk.table = T,
  palette="jco",
  tables.theme = theme,
  #title = "Kaplan-Meier Plots of Stratified Groups By nLCEV - Discovery",
  #title = "Kaplan-Meier Plots of Stratified Groups By SUVmax - Discovery",
  #title = "Kaplan-Meier Plots of Stratified Groups By nLCEV - Marsden",
  title = "Kaplan-Meier Plots of Stratified Groups By SUVmax - Marsden",
  #title = "Kaplan-Meier Plots of Stratified Groups By nLCEV - Mount Vernon",
  #title = "Kaplan-Meier Plots of Stratified Groups By SUVmax - Mount Vernon",
  #title = "Kaplan-Meier Plots of Stratified Groups By nLCEV - Nottingham",
  #title = "Kaplan-Meier Plots of Stratified Groups By SUVmax - Nottingham",
  xlab="Time in Days",
  ylab="Probability of Overall Survival",
  surv.median.line = "v",
  ylim=c(0,1),
  cumevents=F,
  surv.scale="percent"
)

# extract ggplot object from ggsurvplot
p <- a$plot 
p <- p + scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1095))

# extract table object from ggsurvplot
tab <- a$table
tab$layers = NULL # clear labels
tab <- tab + 
  geom_text(aes(x = time, y = rev(strata), label = llabels), data = tab$data[tab$data$time %in% c(0, 200, 400, 600, 800, 1000),]) +
  scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000))

# extract cumevents object from ggsurvplot
tab2 <- a$cumevents
tab2$layers = NULL # clear labels
tab2 <- tab2 + 
  geom_text(aes(x = time, y = rev(strata), label = cum.n.event), data = tab$data[tab$data$time %in% c(0, 200, 400, 600, 800, 1000),]) +
  scale_x_continuous(breaks = c(0, 200, 400, 600, 800, 1000))

# Add plots back
a$plot <- p
a$table <- tab
a$cumevents <- tab2

a
