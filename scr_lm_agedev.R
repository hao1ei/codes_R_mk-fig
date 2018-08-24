# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(ggplot2); library(mgcv); library(visreg)
Fig.dpi <- 100; Fig.wid <- 20; Fig.hei <- 15

Data.Group  <- c("Child")
WorkDir     <- "/Users/hao1ei/xData/BrainDev_ANT/Behavior"
Fig.SaveDir <- "/Users/hao1ei/xData/BrainDev_ANT/Figure1"

# Cond.NameShow <- c("Alert", "Orient", "Conflict")
# Cond.NameData <- c("A_No.Doub_med_abs", "O_Cen.Spat_med_abs", "C_Incon.Con_med_abs")

# Cond.NameShow <- c("Alert_FS_L","Alert_FS_R","Alert_SPL_L","Alert_SPL_R","Orient_PS_L","Orient_PS_R","Conflict_dACC","Conflict_Insula_L","Conflict_Insula_R", "A_FS_L_6mm_roi","A_FS_R_6mm_roi","A_SPL_L_6mm_roi","A_SPL_R_6mm_roi","O_SPL_L_6mm_roi","O_SPL_R_6mm_roi","C_Insula_L_6mm_roi","C_Insula_R_6mm_roi","C_dACC_6mm_roi")
# Cond.NameData <- Cond.NameShow

Cond.NameShow <- c("MergeC_A_FS_L_0001_roi",	"MergeC_A_FS_L_10mm_roi",	"MergeC_A_FS_L_12mm_roi",	"MergeC_A_FS_L_6mm_roi",	"MergeC_A_FS_L_8mm_roi",	"MergeC_A_FS_R_0001_roi",	"MergeC_A_FS_R_10mm_roi",	"MergeC_A_FS_R_12mm_roi",	"MergeC_A_FS_R_6mm_roi",	"MergeC_A_FS_R_8mm_roi",	"MergeC_A_SPL_L_0001_roi",	"MergeC_A_SPL_L_10mm_roi",	"MergeC_A_SPL_L_12mm_roi",	"MergeC_A_SPL_L_6mm_roi",	"MergeC_A_SPL_L_8mm_roi",	"MergeC_A_SPL_R_0001_roi",	"MergeC_A_SPL_R_10mm_roi",	"MergeC_A_SPL_R_12mm_roi",	"MergeC_A_SPL_R_6mm_roi",	"MergeC_A_SPL_R_8mm_roi",	"MergeC_O_PS_L_001_roi",	"MergeC_O_PS_R_001_roi",	"MergeC_O_SPL_L_10mm_roi",	"MergeC_O_SPL_L_12mm_roi",	"MergeC_O_SPL_L_6mm_roi",	"MergeC_O_SPL_L_8mm_roi",	"MergeC_O_SPL_R_10mm_roi",	"MergeC_O_SPL_R_12mm_roi",	"MergeC_O_SPL_R_6mm_roi",	"MergeC_O_SPL_R_8mm_roi",	"MergeC_C_ACC_10mm_roi",	"MergeC_C_ACC_12mm_roi",	"MergeC_C_ACC_6mm_roi",	"MergeC_C_ACC_8mm_roi",	"MergeC_C_Insula_L_001_roi",	"MergeC_C_Insula_L_10mm_roi",	"MergeC_C_Insula_L_12mm_roi",	"MergeC_C_Insula_L_6mm_roi",	"MergeC_C_Insula_L_8mm_roi",	"MergeC_C_Insula_R_001_roi",	"MergeC_C_Insula_R_10mm_roi",	"MergeC_C_Insula_R_12mm_roi",	"MergeC_C_Insula_R_6mm_roi",	"MergeC_C_Insula_R_8mm_roi",	"MergeC_C_dACC_001_inter_roi",	"MergeC_C_dACC_001_roi",	"MergeC_C_dACC_10mm_roi",	"MergeC_C_dACC_12mm_roi",	"MergeC_C_dACC_6mm_roi",	"MergeC_C_dACC_8mm_roi")
Cond.NameData <- Cond.NameShow

# =============================================================================================== #
setwd (WorkDir)
for (grp in  c(1:length(Data.Group))) {
  assign(Data.Group[grp], read.csv(paste("BehavImg5_", Data.Group[grp], ".csv", sep = "")))
}

# Select Smoothing Parameters with REML, Using P-splines and Draw by visreg & ggplot2
for (i in c(1:length(Cond.NameShow))) {
  for (j in Data.Group) {
    Data.Fig <- data.frame(get(j)[c("Group","Age1","Age2","Gender",Cond.NameData[i])])
    colnames(Data.Fig) <- c("Group","Age1","Age2","Gender","Index.Fig")
    
    Fit.AgeDev <- lm(Index.Fig ~ Age1 + I(Age1^2), data=Data.Fig)
    Fit.Sum    <- summary(Fit.AgeDev)
    
    p.x <- as.numeric(min(Data.Fig$Age2))
    p.y <- min(Data.Fig$Index.Fig)
    
    if (Fit.Sum$coefficients[2,4] < 0.05 & Fit.Sum$coefficients[2,4] > 0.01) {
      p.v <- paste(as.character(format(Fit.Sum$coefficients[2,4],scientific=TRUE,digit=2)),"(*)",sep="")
    } else if (Fit.Sum$coefficients[2,4] < 0.01 & Fit.Sum$coefficients[2,4] > 0.001) {
      p.v <- paste(as.character(format(Fit.Sum$coefficients[2,4],scientific=TRUE,digit=2)),"(**)",sep="")
    } else if (Fit.Sum$coefficients[2,4] < 0.001) {
      p.v <- paste(as.character(format(Fit.Sum$coefficients[2,4],scientific=TRUE,digit=2)),"(***)",sep="")
    } else if (Fit.Sum$coefficients[2,4] > 0.05) {
      p.v <- paste(as.character(format(Fit.Sum$coefficients[2,4],scientific=TRUE,digit=2)),"(ns)",sep="")
    }
    
    Fig.AgeDev <- visreg(Fit.AgeDev, "Age1", gg=TRUE, line = list(col="lightcoral"), 
                         # by="Gender", overlay = TRUE,
                         # partial=FALSE, band=FALSE,
                         points.par=list(size = 1.5,col ="dodgerblue")) + 
      annotate("text", x=p.x, y=p.y, label=paste("p < ",p.v,sep=""), size=5) + 
      labs(x="Age", y=Cond.NameShow[i], 
           title=paste("Age-related Development of ", j, sep="")) +
      scale_x_continuous(breaks=c(7,8,9,10,11,12), 
                         labels=c("7","8","9","10","11","12")) + 
      theme(
        plot.title=element_text(size=15, colour="black", face="bold", hjust=0.5),
        axis.ticks=element_line(size=0.6, colour="black"),
        axis.ticks.length=unit(0.2, "cm"),
        axis.line.x=element_line(colour="black", size=0.8),
        axis.line.y=element_line(colour="black", size=0.8),
        axis.text=element_text(size=15, colour='black'),
        axis.title=element_text(size=20, colour="black"),
        panel.background=element_rect(fill="white"),
        legend.position="none")
    Fig.AgeDev
    
    Fig.Name <- paste("AgeDevelop_lm_", Cond.NameShow[i], "_", j, ".tiff", sep = "")
    ggsave(Fig.Name, path=Fig.SaveDir, Fig.AgeDev, width=Fig.wid, height=Fig.hei, units="cm", dpi=Fig.dpi)
  }
}