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
symnum.args = list(cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# --


# - load the Person LookUp -
look_up_path <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/DIRECT/DIRECT_R_AfterFlow_Analysis_FinalCounts/"
look_up_name <- "20190228_DIRECTStudy_PlateOutline_LookUp.csv"
IDsOnPlate <- read.csv2(file = file.path(look_up_path, look_up_name), header = TRUE, sep = ";", stringsAsFactors = F)
colnames(IDsOnPlate)[2] <- "Person" # for later merge with flow data
# --


# - check what data files (plates) are already run and restrict IDsOnPlate  -
datapath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/DIRECT/DIRECT_result_rds_files/"
files <- list.files(datapath, pattern = ".rds")
Plates <- sapply(strsplit(x = files, split = "_"), `[[`, 3 )
Plates <- as.integer(sapply(strsplit(x = Plates, split = "Plate"), `[[`, 2))
IDsOnPlate <- dplyr::filter(IDsOnPlate, Plate %in% Plates)
IDsOnPlate$Weight_mg <- as.numeric(gsub(IDsOnPlate$Weight_mg, pattern = ",", replacement = "."))
if (length(files) != length(unique(IDsOnPlate$Plate))) {stop("number of files does not fit to the number of plates in IDsOnPlate")}
# --


# - generate a general LookUp Table -
# NB: THIS DEMANDS THAT EVERY PLATE WAS RUN WITH THE SAME LAYOUT!
LookUp <- data.frame(Tube = c(paste0("A", 1:12),
                              paste0("B", 1:12),
                              paste0("C", 1:12),
                              paste0("D", 1:12),
                              paste0("E", 1:12), 
                              paste0("F", 1:12), 
                              paste0("G", 1:12),
                              paste0("H", 1:12)), 
                     Person = c("Control", 1, "Control", 25:30, 25:27,
                                1:12,
                                1:12,
                                1:12,
                                13:24,
                                13:24,
                                13:24,
                                28:30, 25:30, "Control", "Control", "Buffer"),
                     Replicate = c(NA, NA, rep("r1", 7), rep("r2", 3),
                                   rep("r1", 12),
                                   rep("r2", 12),
                                   rep("r3", 12),
                                   rep("r1", 12),
                                   rep("r2", 12),
                                   rep("r3", 12),
                                   rep("r2", 3), rep("r3", 6), "r2", "r3", NA),
                     Stain = c(rep("unstained", 2), rep("stained", 94)))
# --


# - load in the data and link it to the Person IDs from IDsOnPlate -
DF_list <- list()

for(i in 1:length(files)){
        sampleName <- files[i]
        
        # -- load the res_DF --
        resL <- readRDS(file = file.path(datapath, sampleName))
        res_DF <- resL[["res_DF"]]
        # ----
        
        # -- get plate number from sampleName >> current IDsOnPlate --
        Plate_no <- strsplit(x = sampleName, split = "_")[[1]][3]
        Plate_no <- as.integer(strsplit(x = Plate_no, split = "Plate")[[1]][2])
        
        if(! Plate_no %in% IDsOnPlate$Plate){stop("recovered plate number was not found in IDsOnPlate")}
        IDsOnPlate_current <- dplyr::filter(IDsOnPlate,  Plate == Plate_no)
        # --
        
        # -- Combine LookUp with IDsOnPlate_current--
        LookUp_current <- merge(LookUp, IDsOnPlate_current, by = "Person", all = T)
        # ----
        
        # - Combine res_DF with LookUp_current -
        colnames(res_DF)[5] <- "Tube"
        DF <- merge(LookUp_current, res_DF, by = "Tube")
        # --
        
        DF_list[[i]] <- DF
        Plate_no <- strsplit(x = sampleName, split = "_")[[1]][3]
        names(DF_list)[i] <- Plate_no
}
# --


# - combine DFs and plot -
DF_all <- do.call("rbind", DF_list)

# -- set factors --
DF_all$Experimentor <- factor(DF_all$Experimentor, c("Liwei", "Sandra"), ordered = T)
DF_all$Replicate <- factor(DF_all$Replicate, c("r1", "r2", "r3"), ordered = T)
DF_all$ID <- factor(DF_all$ID, levels = IDsOnPlate$ID, ordered = T)
DF_all$Person <- factor(DF_all$Person, levels = c(1:30, "Control", "Buffer"), ordered = T)
DF_all$Moisture <- factor(DF_all$Moisture, levels = 1:5, ordered = T)
# ----

# - NB: NEW: Set too low counts to NA, i.e. counts that are like buffer samples -
# sometimes filtering did not work for Sandra and Liwei then one replicate was only buffer, I want to kick those replicates out!
# alternatively you could have kicked the fcs file of that replicate out
DF_all$Cells_per_g_feces[DF_all$Cells_per_g_feces <= 1e8] <- NA
DF_all$Cells_per_g_feces_FR[DF_all$Cells_per_g_feces_FR <= 1e8] <- NA
# --


# -- set the counts of emtpy samples to NA --
DF_all$Cells_per_g_feces[is.na(DF_all$Weight_mg)] <- NA
DF_all$Cells_per_g_feces_FR[is.na(DF_all$Weight_mg)] <- NA
# --

# -- Set Colors --
replicate_colors <- QuantColors15[1:3]
names(replicate_colors) <- levels(DF_all$Replicate)


experimentor_colors <- cbPalette[6:7]
names(experimentor_colors) <- levels(DF_all$Experimentor)

experimentor_shapes <- c(16, 17)
names(experimentor_shapes) <- levels(DF_all$Experimentor)
# ----

# -- remove unstained and buffer samples --
DF_Plot <- DF_all[(DF_all$Stain == "stained") & (DF_all$Person != "Buffer"),]
DF_Plot$Plate_Show <- paste0("Plate_", sprintf('%0.2d', DF_Plot$Plate))
# ----

# -- Plot Numbers from beads --
Tr <- ggplot(DF_Plot, aes(x = Person, y = Cells_per_g_feces))

Tr <- Tr +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(aes(col = Replicate, shape = Experimentor), width = 0.2, height = 0, size = 2, alpha = 0.9) +
        facet_grid(Plate_Show ~ .) +
        theme_bw() +
        scale_color_manual("", values = replicate_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        scale_y_continuous(limits = c(0, max(DF_Plot$Cells_per_g_feces))) +
        xlab("") +
        ylab("Cells [per g feces] (Volume from beads)") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# pdf(file = "Plate_Counts_CountsFromBeads.pdf", width = 7, height = 6)
# Tr
# dev.off()
# ----


# -- Plot Numbers from flow rate --
Tr <- ggplot(DF_Plot, aes(x = Person, y = Cells_per_g_feces_FR))

Tr <- Tr +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(aes(col = Replicate, shape = Experimentor), width = 0.2, height = 0, size = 2, alpha = 0.9) +
        facet_grid(Plate_Show ~ .) +
        theme_bw() +
        scale_color_manual("", values = replicate_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        scale_y_continuous(limits = c(0, max(DF_Plot$Cells_per_g_feces_FR))) +
        xlab("") +
        ylab("Cells [per g feces] (Volume from flow rate)") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# pdf(file = "Plate_Counts_CountsFromFlowRate.pdf", width = 8, height = 9)
# Tr
# dev.off()
# ----


# -- also keep track of the controls --
DF_Plot_Control <- DF_Plot[(DF_Plot$Person == "Control"), ]

Tr <- ggplot(DF_Plot_Control, aes(x = Plate_Show, y = Cells_per_g_feces_FR))

Tr <- Tr +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(aes(col = Replicate, shape = Experimentor), width = 0.2, height = 0, size = 3, alpha = 0.9) +
        theme_bw() +
        scale_color_manual("", values = replicate_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        scale_y_continuous(limits = c(0, 2e10)) +
        xlab("") +
        ylab("Control Counts [per g feces] (Volume from flow rate)") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# pdf(file = "Control_Counts_CountsFromFlowRate.pdf", width = 6, height = 5)
# Tr
# dev.off()

Tr1 <- ggplot(DF_Plot_Control, aes(x = Experimentor, y = Cells_per_g_feces_FR))

Tr1 <- Tr1 +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(aes(col = Replicate, shape = Experimentor), width = 0.2, height = 0, size = 3, alpha = 0.9) +
        theme_bw() +
        scale_color_manual("", values = replicate_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        # scale_y_continuous(limits = c(0, 2e10)) +
        xlab("") +
        ylab("Control Counts [per g feces] (Volume from flow rate)") # +
        #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

Tr1 <- Tr1 + stat_compare_means(label = "p.signif", method = "t.test", symnum.args = symnum.args, hide.ns = TRUE, ref.group = "Liwei")


# pdf(file = "Control_Counts_ExperimentorCompare.pdf", width = 6, height = 5)
# Tr1
# dev.off()
# ----
# --


# - make an excel like table of the results -
DF_Save <- dplyr::select(DF_Plot, ID, Person, Random_ID, Experimentor, Testing_Date, Plate, Weight_mg, Moisture, Flow_Date = `$DATE`, Replicate,
                         Cells_per_g_feces, Cells_per_g_feces_FR,
                         RunTime_sec, Events_per_sec, Events_slope, DilFac_Total, flow_muL_per_sec_from_beads, 
                         flow_muL_per_sec_from_bead_slopes)
DF_Save$Testing_Date <- as.character(lubridate::parse_date_time(DF_Save$Testing_Date, orders = "ymd", tz = "CET"))
DF_Save$Flow_Date <- as.character(lubridate::parse_date_time(DF_Save$Flow_Date, orders = "dmy", tz = "CET"))
DF_Save$Plate <- as.character(DF_Save$Plate)
DF_Save$Weight_mg <- as.character(DF_Save$Weight_mg) # necessary for recast command

# For the next command see:
# <https://stackoverflow.com/questions/29775461/how-can-i-spread-repeated-measures-of-multiple-variables-into-wide-format> 

DF_Save_w <- recast(DF_Save, ID + Person + Random_ID + Experimentor + Testing_Date + Plate + Weight_mg + Moisture + Flow_Date ~ variable + Replicate)

DF_Save_w$Weight_mg <- as.numeric(DF_Save_w$Weight_mg)

DF_Save_w <- rowwise(DF_Save_w) %>% dplyr::mutate(Cells_per_g_feces_FR_mean = mean(c(Cells_per_g_feces_FR_r1, Cells_per_g_feces_FR_r2, Cells_per_g_feces_FR_r3), na.rm = T),
                           Cells_per_g_feces_FR_sd = sd(c(Cells_per_g_feces_FR_r1, Cells_per_g_feces_FR_r2, Cells_per_g_feces_FR_r3), na.rm = T),
                           Cells_per_g_feces_mean = mean(c(Cells_per_g_feces_r1, Cells_per_g_feces_r2, Cells_per_g_feces_r3), na.rm = T),
                           Cells_per_g_feces_sd = sd(c(Cells_per_g_feces_r1, Cells_per_g_feces_r2, Cells_per_g_feces_r3), na.rm = T))
DF_Save_w <- dplyr::select(DF_Save_w, ID, ID_On_Plate = Person, Random_ID:Flow_Date, Cells_per_g_feces_FR_mean:Cells_per_g_feces_sd,
                           Cells_per_g_feces_r1:flow_muL_per_sec_from_bead_slopes_r3)

# order DF_Save_w based on IDsOnPlate

DF_Save_w <- DF_Save_w[match(IDsOnPlate$ID, DF_Save_w$ID), ]
# --


# - use DF_Save_w for some dot plots -
# remove controls and emtpy samples
DF_Save_w_Plot <- dplyr::arrange(DF_Save_w, Cells_per_g_feces_FR_mean) %>% dplyr::filter(ID_On_Plate != "Control", !is.na(Weight_mg))
DF_Save_w_Plot$ID <- factor(DF_Save_w_Plot$ID, levels = DF_Save_w_Plot$ID, order = T)



Tr <- ggplot(DF_Save_w_Plot, aes(x = ID, y = Cells_per_g_feces_FR_mean, col = Experimentor, shape = Experimentor))
Tr <- Tr +
        geom_errorbar(aes(ymin = Cells_per_g_feces_FR_mean-Cells_per_g_feces_FR_sd, ymax = Cells_per_g_feces_FR_mean+Cells_per_g_feces_FR_sd), width = 1) +
        geom_point(size = 2, alpha = 0.9) +
        theme_bw() +
        scale_y_continuous(limits = c(0, max(DF_Save_w$Cells_per_g_feces_FR_mean + DF_Save_w$Cells_per_g_feces_FR_sd))) +
        scale_color_manual("", values = experimentor_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        xlab("") +
        ylab("Cells [per g feces] (volume from set flow rate)") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# pdf(file = "RangeOfCounts_DIRECT.pdf", width = 10, height = 5)
# Tr
# dev.off()



DF_Save_w_Plot$Weight_mg <- as.numeric(DF_Save_w_Plot$Weight_mg)
Tr1 <- ggplot(DF_Save_w_Plot, aes(x = Weight_mg, y = Cells_per_g_feces_FR_mean, col = Experimentor, shape = Experimentor))
Tr1 <- Tr1 +
        geom_errorbar(aes(ymin = Cells_per_g_feces_FR_mean-Cells_per_g_feces_FR_sd, ymax = Cells_per_g_feces_FR_mean+Cells_per_g_feces_FR_sd), width = 1) +
        geom_point(size = 2, alpha = 0.9) +
        theme_bw() +
        scale_y_continuous(limits = c(0, max(DF_Save_w$Cells_per_g_feces_FR_mean + DF_Save_w$Cells_per_g_feces_FR_sd))) +
        scale_color_manual("", values = experimentor_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        xlab("Weight of aliquot [mg]") +
        ylab("Cells [per g feces] (volume from set flow rate)") 


# pdf(file = "Weight_to_counts.pdf", width = 6, height = 5)
# Tr1
# dev.off()


Tr2 <- ggplot(DF_Save_w_Plot, aes(x = Moisture, y = Cells_per_g_feces_FR_mean))
Tr2 <- Tr2 +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(aes(col = Experimentor, shape = Experimentor), size = 2, alpha = 0.9, width = 0.2, height = 0) +
        theme_bw() +
        # scale_y_continuous(limits = c(0, max(DF_Save_w$Cells_per_g_feces_FR_mean + DF_Save_w$Cells_per_g_feces_FR_sd))) +
        scale_color_manual("", values = experimentor_colors) +
        scale_shape_manual("", values = experimentor_shapes) +
        xlab("Moisture scale") +
        ylab("Cells [per g feces] (volume from set flow rate)") 

Tr2 <- Tr2 + stat_compare_means(label = "p.signif", method = "t.test", symnum.args = symnum.args, hide.ns = TRUE, ref.group = "1")

# pdf(file = "Moisture_Effect.pdf", width = 6, height = 5)
# Tr2
# dev.off()
# --



# - save all relevant data -
resList <- list(DF_all = DF_all, DF_Plot = DF_Plot, DF_Save_w = DF_Save_w)
savepath <- file.path(look_up_path, "DIRECT_FinalCounts")
savename <- paste0(as.Date(now()), "_CountAnalysis_DIRECTStudy.rds")
saveRDS(object = resList, file.path(savepath, savename))

savename <- paste0(as.Date(now()), "_DIRECTStudy_Counts.csv")
write.csv2(x = DF_Save_w, file = file.path(savepath, savename))
# --




# - test your numbers in DF_Save_w with dplyr -
# DF_Plot <- group_by(DF_Plot, ID) 
# 
# DF_Plot_summary <- summarise(DF_Plot, Mean_Cells_per_g_feces = mean(Cells_per_g_feces), 
#                              SD_Cells_per_g_feces = sd(Cells_per_g_feces),
#                              Mean_Cells_per_g_feces_FR = mean(Cells_per_g_feces_FR),
#                              SD_Cells_per_g_feces_FR = sd(Cells_per_g_feces_FR))
# 
# # yes same numbers
# --
