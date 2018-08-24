# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(visreg); library(ggplot2); library(RColorBrewer)
suppressPackageStartupMessages(library(mgcv))
fig.dpi <- 600; fig.wid <- 20; fig.hei <- 15
fig.savedir <- "~/Downloads"

#----------------------------------------
setwd ("~/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs/ANT_3SD")
data.fig <- read.csv("basic_CBDA_all.csv")

# data.index <- read.csv("res_rsa_multi2one_CtoA_C_spmT.csv")
# data.fig <- merge(data.fig, data.index, by="Scan_ID")

#----------------------------------------
con.col <- c(brewer.pal(9, "Oranges")[4:7], brewer.pal(9, "Greens")[4:7])
# con.col <- c("lightcoral","darkolivegreen3","deepskyblue")

name.colname <- c("ACC_Con_No",	"ACC_Con_Cent",	"ACC_Con_Doub",	"ACC_Con_Spat",	"ACC_Incon_No",	"ACC_Incon_Cent",	"ACC_Incon_Doub",	"ACC_Incon_Spat")
name.figshow <- name.colname
# "A_NoDoub_mean_abs",	"O_CenSpat_mean_abs",	"C_InconCon_mean_abs"
# "A_NoDoub_med_abs",	"O_CenSpat_med_abs",	"C_InconCon_med_abs"
# "A_NoDoub_mean_rate",	"O_CenSpat_mean_rate",	"C_InconCon_mean_rate"
# "A_NoDoub_med_rate",	"O_CenSpat_med_rate",	"C_InconCon_med_rate"
# "A_NoDoub_mean", "O_CenSpat_mean", "C_InconCon_mean"
# "OA_roin_indep_a_CBDA_roi","OA_roin_indep_o_CBDA_roi","OA_roin_indep_c_CBDA_roi"
# "RT_Con_No_mean", "RT_Con_Cent_mean", "RT_Con_Doub_mean",	"RT_Con_Spat_mean",	"RT_Incon_No_mean",	"RT_Incon_Cent_mean",	"RT_Incon_Doub_mean",	"RT_Incon_Spat_mean"
# "ACC_Con_No",	"ACC_Con_Cent",	"ACC_Con_Doub",	"ACC_Con_Spat",	"ACC_Incon_No",	"ACC_Incon_Cent",	"ACC_Incon_Doub",	"ACC_Incon_Spat"
#----------------------------------------

# Select Smoothing Parameters with REML, Using P-splines and Draw by visreg & ggplot2
for (i in 1:length(name.colname)) {
  data.gam <- data.frame(data.fig[c("Group","Age1","Age2","Gender",name.colname[i])])
  colnames(data.gam) <- c("Group","Age1","Age2","Gender","index.fig")
  
  # gam(networkMeasure ~ s(age, method="REML") + covariates)
  fit.agedev <- gam(index.fig ~ s(Age1)+Gender, data=data.gam, method="REML")
  fit.sum    <- summary(fit.agedev)
  
  p.x <- as.numeric(min(data.gam$Age2)) + 1
  p.y <- min(data.gam$index.fig)
  
  if (fit.sum$s.pv < 0.05 & fit.sum$s.pv >= 0.01) {
    p.v <- paste(as.character(format(fit.sum$s.pv,scientific=TRUE,digit=2)),"(*)",sep="")
  } else if (fit.sum$s.pv < 0.01 & fit.sum$s.pv >= 0.001) {
    p.v <- paste(as.character(format(fit.sum$s.pv,scientific=TRUE,digit=2)),"(**)",sep="")
  } else if (fit.sum$s.pv < 0.001) {
    p.v <- paste(as.character(format(fit.sum$s.pv,scientific=TRUE,digit=2)),"(***)",sep="")
  } else if (fit.sum$s.pv > 0.05) {
    p.v <- paste(as.character(format(fit.sum$s.pv,scientific=TRUE,digit=2)),"(ns)",sep="")
  }
  
  fig.agedev <- visreg(fit.agedev, "Age1", gg=TRUE,
                       # by="Gender",overlay=TRUE,
                       partial=F,
                       band=F,
                       line = list(col=con.col[i],size = 1.5),
                       points.par=list(size = 0.6,col ="gray50")) + 
    #annotate("text", x=p.x, y=p.y, label=paste("p<",p.v,sep=""), size=6) + 
    labs(x="", y="", title="") +
    #scale_x_continuous(breaks=c(7,8,9,10,11,12), labels=c(7,8,9,10,11,12)) + 
    scale_x_continuous(breaks=c(19,20,21,22,23,24,25), labels=c(19,20,21,22,23,24,25)) + 
    scale_y_continuous(limits=c(0.7,1)) +
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
  fig.agedev
  
  fig.name <- paste("fig_gam_agedev_",name.figshow[i], ".tiff", sep = "")
  ggsave(fig.name, path=fig.savedir, fig.agedev, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
}
