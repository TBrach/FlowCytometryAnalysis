library(ggpubr)

functionpath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/Functions_FC"
function_file <- "20190103_FlowCytometry_Functions.R"
source(file.path(functionpath, function_file))

savepath <- "/Users/jvb740/MarieCurie_Work/CellCounting_TaskForce/FlowCytometryAnalysis/Result_rds_files/20190118_Pretest/"
files <- list.files(savepath)


LookUp <- data.frame(Tube = c(paste0("A", 1:12), paste0("B", 1:3), paste0("D", 1:12),
                              paste0("E", 1:12), paste0("F", 1:12), paste0("G", 1:12),
                              paste0("H", 1:12)), 
                     Person = c(rep("P1", 3), rep("P2", 3), rep("P3", 3),
                                rep("P4", 3), rep("P5",3), 
                                rep("P1", 12), rep("P2", 12),
                                rep("P3", 12), rep("P4", 12),
                                rep("P5", 12)))


# - liwei had one mix up, so I need a separate lookup talbe for her -
LookUpL <- LookUp
LookUpL$Person[LookUpL$Tube %in% c("E9", "E10", "E11", "E12")] <- "P3"
LookUpL$Person[LookUpL$Tube %in% c("F9", "F10", "F11", "F12")] <- "P2"

DFList <- list()

for (i in 1:length(files)){
        DF <- readRDS(file = file.path(savepath, files[i]))
        DF <- DF[["res_DF"]]
        if (i < 4){
                DF$Person <- LookUp$Person[match(DF$`TUBE NAME`, LookUp$Tube)]
        
                DF$Tester <- "Sandra"
        } else {
                DF$Tester <- "Liwei"
                DF$Person <- LookUpL$Person[match(DF$`TUBE NAME`, LookUpL$Tube)]
        }
        DFList[[i]] <- DF
        
}

DF <- do.call("rbind", DFList)

DF <- DF[!is.na(DF$Person),]
DF <- DF[!grepl("A|B", DF$`TUBE NAME`), ]


DF$DilFac_Total <- paste0(round(DF$DilFac_Total), "x")

DF$DilFac_Total <- factor(DF$DilFac_Total, levels = unique(DF$DilFac_Total), ordered = F)

person_colors <- c(cbPalette[2:4], cbPalette[7:8])
names(person_colors) <- levels(DF$Person)

DF$Tester <- factor(DF$Tester, levels = c("Sandra", "Liwei"), ordered = T)

Tr <- ggplot(DF, aes(x = DilFac_Total, y = Cells_per_g_feces_mean_e10, col = Person))

Tr <- Tr +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(width = 0.2, height = 0) +
        facet_grid(Tester ~ Person) +
        theme_bw() +
        scale_color_manual("", values = person_colors) +
        xlab("") +
        ylab("Cells [x10^10 per g feces]") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")


# Tr <- Tr + stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE)

pdf(file = "comparison_Liwei_Sandra_Persons.pdf", width = 8, height = 6)
Tr
dev.off()

tester_colors <- c(cbPalette[2], cbPalette[4])
names(tester_colors) <- c("Sandra", "Liwei")


Tr1 <- ggplot(DF, aes(x = Tester, y = Cells_per_g_feces_mean_e10, col = Tester))

Tr1 <- Tr1 +
        geom_boxplot(outlier.color = NA) +
        geom_jitter(width = 0.2, height = 0) +
        facet_grid(DilFac_Total ~ Person) +
        theme_bw() +
        scale_color_manual("", values = tester_colors) +
        xlab("") +
        ylab("Cells [x10^10 per g feces]") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")

Tr1 <- Tr1 + stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, ref.group = "Sandra")

pdf(file = "comparison_Liwei_Sandra.pdf", width = 8, height = 7)
Tr1
dev.off()
