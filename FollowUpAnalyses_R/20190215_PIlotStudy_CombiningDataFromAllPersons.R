# - load packages -
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(lubridate)
library(viridis)
# --


# - load functions/colors and data -
functionpath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/Functions_FC"
function_file <- "20190103_FlowCytometry_Functions.R"
source(file.path(functionpath, function_file))

datapath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/Result_rds_files/20190214_PilotStudy/"
files <- list.files(datapath)
files <- files[grepl(pattern = "CountAnalysis", files)]
files <- files[files != "2019-02-15_CountAnalysis_P3.rds"]


resDFs <- list()

for (i in 1:length(files)){
        resList <- readRDS(file.path(datapath, files[i]))
        resDFs[[i]] <- resList[["DF_Plot_summary_summary"]]
}


# -- restrict to shared columns since Person 5 was made with a different protocol: USE Reduce!! --
colnamesList <- lapply(resDFs, colnames)
keepColnames <- Reduce(intersect, colnamesList)
# ----

resDFs <- lapply(resDFs, dplyr::select, keepColnames)

DF <- do.call("rbind", resDFs)
# --




person_colors <- QuantColors15[1:5]
names(person_colors) <- c("P1", "P2", "P3", "P4", "P5")

DF$Person <- factor(DF$Person, levels = c("P1", "P2", "P3", "P4", "P5"), ordered = T)

Tr <- ggplot(DF, aes(x = Date_Time, y = Mean_Mean_Cells_per_g_feces_FR))

Tr <- Tr +
        geom_errorbar(aes(ymin = Mean_Mean_Cells_per_g_feces_FR-SE_Cells_per_g_feces_FR, ymax = Mean_Mean_Cells_per_g_feces_FR+SE_Cells_per_g_feces_FR, col = Person), width = 30000) +
        # geom_line(aes(col = Person, group = Thaw)) +
        geom_point(aes(col = Person, shape = Thaw), size = 3, alpha = 0.9) +
        theme_bw() +
        facet_grid(Person ~ .) +
        scale_color_manual("", values = person_colors) +
        scale_y_continuous(limits = c(0, max(DF$Mean_Mean_Cells_per_g_feces_FR + DF$SE_Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces]")


pdf("Combined_Persons_FTvsST.pdf", width = 7, height = 5.5)
Tr
dev.off()




Tr1 <- ggplot(DF, aes(x = Date_Time, y = Mean_Mean_Cells_per_g_feces_FR))

Tr1 <- Tr1 +
        geom_errorbar(aes(ymin = Mean_Mean_Cells_per_g_feces_FR-SE_Cells_per_g_feces_FR, ymax = Mean_Mean_Cells_per_g_feces_FR+SE_Cells_per_g_feces_FR, col = Person), width = 30000) +
        geom_line(aes(col = Person)) +
        geom_point(aes(col = Person, shape = Thaw), size = 3, alpha = 0.9) +
        theme_bw() +
        facet_grid(Thaw ~ .) +
        scale_color_manual("", values = person_colors) +
        scale_y_continuous(limits = c(0, max(DF$Mean_Mean_Cells_per_g_feces_FR + DF$SE_Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces]")


pdf("Combined_Persons_PersonvsPersons.pdf", width = 7, height = 5.5)
Tr1
dev.off()



Tr2 <- ggplot(DF, aes(x = Timepoint, y = Mean_Mean_Cells_per_g_feces_FR))

Tr2 <- Tr2 +
        geom_errorbar(aes(ymin = Mean_Mean_Cells_per_g_feces_FR-SE_Cells_per_g_feces_FR, ymax = Mean_Mean_Cells_per_g_feces_FR+SE_Cells_per_g_feces_FR, col = Person), width = .2) +
        # geom_line(aes(col = Person, group = Thaw)) +
        geom_point(aes(col = Person, shape = Thaw), size = 3, alpha = 0.9) +
        theme_bw() +
        facet_grid(Person ~ .) +
        scale_color_manual("", values = person_colors) +
        scale_y_continuous(limits = c(0, max(DF$Mean_Mean_Cells_per_g_feces_FR + DF$SE_Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces]")



pdf("Combined_Persons_FTvsST_timepoints.pdf", width = 7, height = 5.5)
Tr2
dev.off()


Tr3 <- ggplot(DF, aes(x = Timepoint, y = Mean_Mean_Cells_per_g_feces_FR))

Tr3 <- Tr3 +
        geom_errorbar(aes(ymin = Mean_Mean_Cells_per_g_feces_FR-SE_Cells_per_g_feces_FR, ymax = Mean_Mean_Cells_per_g_feces_FR+SE_Cells_per_g_feces_FR, col = Person), width = .2) +
        geom_line(aes(col = Person, group = Person)) +
        geom_point(aes(col = Person, shape = Thaw), size = 3, alpha = 0.9) +
        theme_bw() +
        facet_grid(Thaw ~ .) +
        scale_color_manual("", values = person_colors) +
        scale_y_continuous(limits = c(0, max(DF$Mean_Mean_Cells_per_g_feces_FR + DF$SE_Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces]")





