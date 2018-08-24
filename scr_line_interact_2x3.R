# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(ggplot2)
suppressPackageStartupMessages(library(psych))
fig.dpi <- 100; fig.wid <- 15; fig.hei <- 15
fig.savedir <- "~/Downloads/line"

#----------------------------------------

data.grp   <- "CBD"
data.cond  <- factor(c(rep("Alert", 2), rep("Orient", 2), rep("Conflict", 2)),
                     order = T, levels = c("Alert","Orient","Conflict"))
fig.subgrp <- factor(rep(c("CBDC","CBDA"), 3),
                     order = T, levels = c("CBDC","CBDA"))
index.name  <- c(
  "AAL_ACC_l",	"AAL_ACC_lr",	"AAL_ACC_r",	"CBDA_single_dACC_001_right",	"CBDA_subtract_dACC_0005_right",	"CBDA_subtract_dACC_001_double",	"CBDC_single_dACC_01",	"CBDC_subtract_dACC_01",	"NeuroSynth_FanANT_dACC",	"NeuroSynth_FanANT_fef_l",	"NeuroSynth_FanANT_fef_r",	"NeuroSynth_FanANT_spl_l",	"NeuroSynth_FanANT_spl_r",	"NeuroSynth_allcomb",	"Neurosynth_dACC_l_FDR_0.01_inter_roi",	"Neurosynth_dACC_lr_FDR_0.01_inter_roi",	"Neurosynth_dACC_r_FDR_0.01_inter_roi",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_fef_l",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_fef_l_big",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_fef_l_small",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_fef_r",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_fef_r_big",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_fef_r_small",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_spl_l",	"OneANOVA_CBDA_ROIcomb_AOC_inter_roi_spl_r",	"OneANOVA_CBDA_dACC_001_33",	"cond_dacc_l_005",	"cond_dacc_r_005",	"cond_fef_l_005",	"cond_fef_r_005",	"cond_fm_l_005",	"cond_fm_r_005",	"cond_insula_l_005",	"cond_insula_r_005",	"cond_spl_l_005",	"cond_spl_r_005",	"grp_fs_r_005",	"grp_spl_l_005",	"grp_spl_r_005",	"inter_fm_r_005",	"inter_spl_l_005",	"inter_spl_r_005",	"o_acti_tpj_l1",	"o_acti_tpj_l2",	"o_age_tpj_r",	"tpj_pAgF_z_FDR_0.01",	"tpj_pFgA_z_FDR_0.01"
)

setwd ("~/Desktop/Docum/Projects/DataAnaly/BrainDev_ANT/AnalyDocs")
CBDC <- read.csv("res_behav_CBDC_img5behav.csv")
CBDA <- read.csv("res_behav_CBDA_img5behav.csv")

for (iroi in 1:length(index.name)){
  cond.all <- unique(data.cond)
  grp.all  <- unique(fig.subgrp)
  mRT.fig  <- vector(mode = "numeric", length = 0)
  seRT.fig <- vector(mode = "numeric", length = 0)
  
  for (icond in cond.all){
    data.index <- read.csv(paste("res_extrmean_nroi_", icond, ".csv",sep = ""))
    for (igrp in grp.all){
      grp <- merge(get(igrp), data.index, by="Scan_ID")
      code2exe <- paste("roidata <- grp$",index.name[iroi],sep="")
      eval(parse(text=code2exe))
      mRT.fig <- c(mRT.fig, mean(roidata))
      seRT.fig <- c(seRT.fig, describe(roidata)$se)
    }}
  
  #----------------------------------------
  
  data.interac <- data.frame(data.cond, fig.subgrp, mRT.fig, seRT.fig)
  fig.interac  <- ggplot(data = data.interac, 
                         aes(y = mRT.fig, x = fig.subgrp,
                             colour = as.factor(data.cond), 
                             group = as.factor(data.cond))) + 
    geom_errorbar(aes(ymin = mRT.fig - seRT.fig, ymax = mRT.fig + seRT.fig), width = 0.05) +
    # geom_point(size = 2) +
    geom_line(size = 1.5) +
    labs(x = "Conditions", y = index.name[iroi], title = "") + 
    scale_color_manual (values=c("lightcoral","darkolivegreen3","deepskyblue"), name="Group", labels=c("Alert", "Orient","Conflict")) +
    #scale_color_manual (values=c("salmon1","gray60"), name="Group", labels=c("Children","Adults")) +
    theme(
      plot.title = element_text(size = 15, colour = "black", face = "bold", hjust = 0.5),
      axis.ticks = element_line(size = 0.6, colour = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.x = element_line(colour = "black", size = 0.8),
      axis.line.y = element_line(colour = "black", size = 0.8),
      axis.text = element_text(size = 15, colour = 'black'),
      #axis.text.x = element_text(vjust = 1, hjust = 1, angle = 45),
      axis.title = element_text(size = 15, colour = "black"),
      panel.background = element_rect(fill = "white"),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 15),
      legend.position = "top",
      legend.justification = c(0.5,1))
  fig.interac
  
  fig.name <- paste("fig_img_interact_ANT_", data.grp, "_", index.name[iroi],".tiff", sep = "")
  ggsave(fig.name,path=fig.savedir,fig.interac,width=fig.wid,height=fig.hei,units="cm",dpi=fig.dpi)
}