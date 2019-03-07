# Persons 1 to 10 of Plate06 were run at a different dilution than the other persons on that plate,
# Therefore I rund these 10 persons again with the right dilution and now replace the count results for these persons here
# saving finally a new "20190228_DIRECT_Plate06_results.rds".


# - load packages -
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(lubridate)
library(viridis)
library(reshape2)
# --


# - load functions/colors -
functionpath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/DIRECT/Functions_FC/"
function_file <- "20190103_FlowCytometry_Functions.R"
source(file.path(functionpath, function_file))
# --



# - check what data files (plates) are already run and restrict IDsOnPlate  -
datapath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/DIRECT/DIRECT_result_rds_files/"
files <- list.files(datapath)
files <- c("20190228_DIRECT_Plate06_results.rds", "20190228_DIRECT_Plate06_1to10_results.rds")
# --



# - load in the data and link it to the Person IDs from IDsOnPlate -
DF_list <- list()

for(i in 1:length(files)){
        sampleName <- files[i]
        
        # -- load the res_DF --
        resL <- readRDS(file = file.path(datapath, sampleName))
        res_DF <- resL[["res_DF"]]
        # ----
        
        # --
        
        DF_list[[i]] <- res_DF
        names(DF_list)[i] <- sampleName
}
# --

# - replace the rows -
df_before <- DF_list[[1]]
df_corrected <- DF_list[[2]]

df_before[match(df_corrected$`TUBE NAME`, df_before$`TUBE NAME`), ] <- df_corrected

resL <- readRDS(file = file.path(datapath, "20190228_DIRECT_Plate06_results.rds"))
resL[["res_DF"]] <- df_before
saveRDS(resL, file = file.path(datapath, "20190228_DIRECT_Plate06_results.rds"))
# --
