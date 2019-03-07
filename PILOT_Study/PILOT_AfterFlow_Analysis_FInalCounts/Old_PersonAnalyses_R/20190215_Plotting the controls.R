# - load packages -
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(lubridate)
library(viridis)
# --

# - load functions you only use the colors from -
functionpath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/Functions_FC"
function_file <- "20190103_FlowCytometry_Functions.R"
source(file.path(functionpath, function_file))
# --


# - load data, and only extract the Control Data into DFControls for plotting -
datapath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/Result_rds_files/20190214_PilotStudy/"
files <- list.files(datapath)
files <- files[grepl(pattern = "CountAnalysis", files)]

resDFs <- list()

for (i in 1:length(files)){
        resList <- readRDS(file.path(datapath, files[i]))
        resDFs[[i]] <- resList[["DF_control"]]
}



# -- restrict to shared columns since Person 5 was made with a different protocol: USE Reduce!! --
colnamesList <- lapply(resDFs, colnames)
keepColnames <- Reduce(intersect, colnamesList)
# ----

resDFs <- lapply(resDFs, dplyr::select, keepColnames)

DFControls <- do.call("rbind", resDFs)

DFControls <- dplyr::filter(DFControls, Stain == "stained")
# --



# - plot the Cells_per_g_feces values of the control aliquots that were used on the different dates - 

aliquot_colors <- c(viridis(1), "#FF7F50")

Tr <- ggplot(DFControls, aes(x = FromPerson, y = Cells_per_g_feces_FR))
Tr <- Tr +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(aes(col = Aliquot), width = 0.2, height = 0, size = 3) +
        theme_bw() +
        scale_color_manual("", values = aliquot_colors) +
        scale_y_continuous(limits = c(0, max(DFControls$Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces] (Volume from set flow rate)")


pdf(file = "CountValues_ControlAliquots.pdf", width = 5, height = 4.5)
Tr
dev.off()

# --


# - if yo plotted it from the beads -
# Tr <- ggplot(DFControls, aes(x = FromPerson, y = Cells_per_g_feces))
# Tr <- Tr +
#         geom_boxplot(outlier.color = NA) +
#         geom_jitter(aes(col = Aliquot), width = 0.2, height = 0, size = 3) +
#         theme_bw() +
#         scale_color_manual("", values = aliquot_colors) +
#         scale_y_continuous(limits = c(0, max(DFControls$Cells_per_g_feces))) +
#         xlab("") +
#         ylab("Cells [per g feces] (Volume from set flow rate)")



# --




# - also plot against the events per seconds -
person_colors <- QuantColors15[1:5]
names(person_colors) <- c("P1", "P2", "P3", "P4", "P5")
Tr1 <- ggplot(DFControls, aes(x = Events_slope, y = Cells_per_g_feces_FR))
Tr1 <- Tr1 +
        geom_point(aes(col = FromPerson), size = 3) +
        theme_bw() +
        scale_color_manual("", values = person_colors) +
        scale_y_continuous(limits = c(0, max(DFControls$Cells_per_g_feces_FR))) +
        xlab("Events per second") +
        ylab("Cells [per g feces] (Volume from set flow rate)")

# --




