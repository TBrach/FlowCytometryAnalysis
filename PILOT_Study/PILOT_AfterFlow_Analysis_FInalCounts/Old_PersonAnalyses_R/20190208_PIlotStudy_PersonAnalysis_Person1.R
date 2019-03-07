library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(lubridate)

functionpath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/PILOT_Study/Functions_FC/"
function_file <- "20190103_FlowCytometry_Functions.R"
source(file.path(functionpath, function_file))

datapath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/PILOT_Study/PILOT_result_rds_files/"
files <- list.files(datapath)
sampleName <- "20190213_PilotStudy_Person1_results.rds"


resL <- readRDS(file = file.path(datapath, sampleName))
res_DF <- resL[["res_DF"]]



Person <- "P1"
Experiment_Date <- "2019-02-13"

LookUp <- data.frame(Tube = c(paste0("A", 1:5),
                              paste0("A", 11:12),
                              paste0("B", 1:12),
                              paste0("C", 1:12),
                              paste0("D", 1:12),
                              paste0("E", 1:12), 
                              paste0("F", 1:12), 
                              paste0("G", 1:12),
                              paste0("H", 1:3),
                              paste0("H", 9:11)), 
                     Person = c(rep("Control", 2), 
                                rep(Person, 2),
                                rep("Control", 1),
                                rep(Person, 74),
                                rep("Control", 2),
                                "Buffer",
                                rep(Person, 3)),
                     Timepoint = c(NA, NA, "t1", "t4", NA, "t3", "t3",
                                   rep(c(rep("t1", 3), rep("t2", 3), rep("t1", 3), rep("t2", 3)), 3),
                                   rep(c(rep("t3", 3), rep("t4", 3), rep("t3", 3), rep("t4", 3)), 3),
                                   NA, NA, NA, "t3", "t3", "t3"),
                     Thaw = c(NA, NA, "FT", "ST", NA, "FT", "FT",
                              rep(c("FT", "FT", "ST", "FT", "FT", "ST", "FT", "ST", "ST", "FT", "ST", "ST"), 6), 
                              NA, NA, NA, "FT", "FT", "FT"),
                     Aliquot = c("Aliquot 1", "Aliquot 2", "Aliquot 1", "Aliquot 3", "Aliquot 1", "Aliquot 2", "Aliquot 2",
                               rep(c(rep(c("Aliquot 1", "Aliquot 3", "Aliquot 2"), 2), rep(c("Aliquot 2", "Aliquot 1", "Aliquot 3"), 2)), 6),
                               "Aliquot 1", "Aliquot 2", NA, "Aliquot 2", "Aliquot 2", "Aliquot 2"),
                     Replicate = c(NA, NA, NA, NA, "r1", NA, NA,
                                   rep("r1", 12),
                                   rep("r2", 12),
                                   rep("r3", 12),
                                   rep("r1", 12),
                                   rep("r2", 12),
                                   rep("r3", 12),
                                   "r2", "r2", NA, "r1", "r2", "r3"),
                     Stain = c(rep("unstained", 4), "stained", "unstained", "unstained", rep("stained", 78)),
                     Experimentor = c(rep(c("Liwei", "Sandra"), 3), "Sandra", 
                                      rep(c(rep("Liwei", 6), rep("Sandra", 6)), 6),
                                      "Liwei", "Sandra", "Liwei", "Sandra", "Sandra", "Sandra"),
                     ExperimentDate = Experiment_Date)







# - Load Date and Time of time points and add that information -
stool_data <- read.csv2(file = file.path(datapath, "00_Template_SampleCollectionSheet_P1.csv"), header = TRUE, sep = ";",
                        stringsAsFactors = FALSE)
stool_data$Date <- lubridate::parse_date_time(stool_data$Date, orders = "dmy", tz = "CET")
stool_data$Time <- lubridate::parse_date_time(stool_data$Time, orders = "HM", tz = "CET")
stool_data$Date_Time <-  update(stool_data$Date, hour = hour(stool_data$Time), minute = minute(stool_data$Time))

colnames(stool_data) %in% colnames(DF)

DF <- merge(DF, stool_data, by = "Timepoint", all = TRUE)
# --



# - set factors -
DF$Experimentor <- factor(DF$Experimentor, c("Liwei", "Sandra"), ordered = T)
DF$Aliquot <- factor(DF$Aliquot, c("Aliquot 1", "Aliquot 2", "Aliquot 3"), ordered = T)
# --


# - Set Colors -
aliquot_colors <- QuantColors15[1:3]
names(aliquot_colors) <- levels(DF$Aliquot)

experimentor_shapes <- c(16, 17)
names(experimentor_shapes) <- levels(DF$Experimentor)

# person_colors <- c(cbPalette[2:4], cbPalette[7:8])
# names(person_colors) <- levels(DF$Person)
thaw_colors <- c("#d3d3d3", cbPalette[1])
names(thaw_colors) <- c("FT", "ST")

# --





# - plot the counts using beads as volume -
DF_Plot <- DF[grepl(pattern = "B|C|D|E|F|G", DF$Tube), ]


# First plot the number from the beads that might go up for t3 and t3
Tr <- ggplot(DF_Plot, aes(x = Timepoint, y = Cells_per_g_feces))

Tr <- Tr +
        geom_boxplot(aes(fill = Thaw), outlier.color = NA) +
        geom_point(aes(fill = Thaw, col = Aliquot, shape = Experimentor), position = position_jitterdodge(jitter.height = 0, jitter.width = 0.05), size = 3, alpha = 0.9) +
        theme_bw() +
        scale_color_manual("", values = aliquot_colors) +
        scale_fill_manual("", values = thaw_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        scale_y_continuous(limits = c(0, max(DF_Plot$Cells_per_g_feces))) +
        xlab("") +
        ylab("Cells [per g feces] (Volume from beads)") 


# pdf(file = "TimePoint_Plot_VolumeFromBeads_Person1.pdf", width = 6, height = 4.5)
# Tr
# dev.off()
# --


# - plot the counts using flow rate determined volume -
Tr <- ggplot(DF_Plot, aes(x = Timepoint, y = Cells_per_g_feces_FR))

Tr <- Tr +
        geom_boxplot(aes(fill = Thaw), outlier.color = NA) +
        geom_point(aes(fill = Thaw, col = Aliquot, shape = Experimentor), position = position_jitterdodge(jitter.height = 0, jitter.width = 0.05), size = 3, alpha = 0.9) +
        theme_bw() +
        scale_color_manual("", values = aliquot_colors) +
        scale_fill_manual("", values = thaw_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        scale_y_continuous(limits = c(0, max(DF_Plot$Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces] (Volume from set flow rate)") 


# pdf(file = "TimePoint_Plot_VolumeFromFR.pdf", width = 6, height = 4.5)
# Tr
# dev.off()
# --



# - plot today the comparison plastic to glass -
DF_Plot2 <- DF[DF$Tube %in% c("E7", "F7", "G7", "H9", "H10", "H11"), ]
DF_Plot2$Material <- "Glass"
DF_Plot2$Material[1:3] <- "Plastic"
DF_Plot2$Material <- factor(DF_Plot2$Material, levels = c("Plastic", "Glass"), ordered = TRUE)





Tr <- ggplot(DF_Plot2, aes(x = Material, y = Cells_per_g_feces_FR))

Tr <- Tr +
        geom_boxplot(aes(fill = Thaw), outlier.color = NA) +
        geom_point(aes(fill = Thaw, col = Aliquot, shape = Experimentor), position = position_jitterdodge(jitter.height = 0, jitter.width = 0.05), size = 3, alpha = 0.9) +
        theme_bw() +
        scale_color_manual("", values = aliquot_colors) +
        scale_fill_manual("", values = thaw_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        scale_y_continuous(limits = c(0, max(DF_Plot$Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces] (Volume from set flow rate)") 

Tr <- Tr + stat_compare_means(method = "t.test", hide.ns = FALSE)


# pdf(file = "Person1_t3_FT_Aliquot2_PlasticvsGlass2.pdf", width = 5, height = 4.5)
# Tr
# dev.off()
# --


# - plot Date_Time vs the counts using flow rate determined volume -
# -- calculate the mean counts for each Timepoint, Thaw, and Aliquot --
DF_Plot <- group_by(DF_Plot, Person, Timepoint, Thaw, Aliquot) 

DF_Plot_summary <- summarise(DF_Plot, Mean_Cells_per_g_feces = mean(Cells_per_g_feces), 
                             SD_Cells_per_g_feces = sd(Cells_per_g_feces),
                             Mean_Cells_per_g_feces_FR = mean(Cells_per_g_feces_FR),
                             SD_Cells_per_g_feces_FR = sd(Cells_per_g_feces_FR))

# --- add further information back ---
DF_keep <- unique(dplyr::select(DF_Plot,  Person, Timepoint, Thaw, Aliquot, ExperimentDate:Date_Time))
DF_Plot_summary <- merge(DF_Plot_summary, DF_keep)
# ----
# --

# -- calculate the mean counts for each Timepoint and Thaw (ignoring the SD of the aliquots) --
DF_Plot_summary_summary <- group_by(DF_Plot_summary, Person, Timepoint, Thaw)
DF_Plot_summary_summary <- summarise(DF_Plot_summary_summary, Mean_Mean_Cells_per_g_feces = mean(Mean_Cells_per_g_feces), 
                                     SD_Cells_per_g_feces = sd(Mean_Cells_per_g_feces),
                                     SE_Cells_per_g_feces = SD_Cells_per_g_feces/sqrt(n()),
                                     Mean_Mean_Cells_per_g_feces_FR = mean(Mean_Cells_per_g_feces_FR),
                                     SD_Cells_per_g_feces_FR = sd(Mean_Cells_per_g_feces_FR),
                                     SE_Cells_per_g_feces_FR = SD_Cells_per_g_feces_FR/sqrt(n()))

DF_keep <- unique(dplyr::select(DF_Plot_summary,  Person, Timepoint, Thaw, ExperimentDate:Date_Time))
DF_Plot_summary_summary <- merge(DF_Plot_summary_summary, DF_keep)
# ----



Tr <- ggplot(DF_Plot_summary_summary, aes(x = Date_Time, y = Mean_Mean_Cells_per_g_feces_FR))

Tr <- Tr +
        geom_errorbar(aes(ymin = Mean_Mean_Cells_per_g_feces_FR-SE_Cells_per_g_feces_FR, ymax = Mean_Mean_Cells_per_g_feces_FR+SE_Cells_per_g_feces_FR, col = Thaw), width = 30000) +
        geom_point(aes(col = Thaw), size = 3, alpha = 0.9) +
        theme_bw() +
        scale_color_manual("", values = thaw_colors) +
        scale_y_continuous(limits = c(0, max(DF_Plot_summary_summary$Mean_Mean_Cells_per_g_feces_FR + DF_Plot_summary_summary$SE_Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces] (Volume from set flow rate)") 


# pdf(file = "Person5_TimePoint_Plot_VolumeFromFR.pdf", width = 6, height = 4.5)
# Tr
# dev.off()
# --


# - save the control data to compare between the runs -
DF_control <- dplyr::filter(dplyr::select(DF, Timepoint:`EXPORT TIME`, RecTime_sec:ExperimentDate), Person == "Control")
DF_control$FromPerson <- Person
# --


# - Save the data -
resList <- list(DF_Plot = DF_Plot, DF_Plot_summary = DF_Plot_summary, DF_Plot_summary_summary = DF_Plot_summary_summary, DF_control = DF_control)

savename <- paste0(as.Date(now()), "_CountAnalysis_", Person, ".rds")

saveRDS(resList, file = file.path(datapath, savename))
# --


