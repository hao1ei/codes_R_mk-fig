# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
# library(plyr)
# library(ggplot2)
library(ggsignif)
suppressPackageStartupMessages(library(ggpubr))
# suppressPackageStartupMessages(library(psych))
fig.dpi <- 600; fig.wid <- 25; fig.hei <- 15; p.y = 2

setwd ("~/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs")
fig.savedir <- "~/Downloads"

#----------------------------------------
condname.figshow <- c("ACC_Con_No",	"ACC_Con_Cent",	"ACC_Con_Doub",	"ACC_Con_Spat",	"ACC_Incon_No",	"ACC_Incon_Cent",	"ACC_Incon_Doub",	"ACC_Incon_Spat")
condname.colname <- condname.figshow

# "OA_CBDCA_spl_l_fwe05_roi", "OA_CBDCA_spl_r_fwe05_roi", "NS_roi1_aoc_inter_Fan_spl_l", "NS_roi1_aoc_inter_Fan_spl_r"
# "FF_cond_fm_l_fdr05_roi", "FF_cond_fm_r_fdr05_roi", "FF_cond_tpj_l_fdr05_roi", "FF_cond_tpj_r_fdr05_roi"
# "AAL_ACC_l", "AAL_ACC_lr", "AAL_ACC_r", "FF_cond_dacc_l_fdr05_roi", "FF_cond_dacc_r_fdr05_roi", "Neurosynth_dACC_l_FDR_0.01_inter_roi", "Neurosynth_dACC_lr_FDR_0.01_inter_roi", "Neurosynth_dACC_r_FDR_0.01_inter_roi","FF_cond_ai_l_fdr05_roi", "FF_cond_ai_r_fdr05_roi"

#----------------------------------------
data.grp    <- "CBD_Alert"
data.index  <- read.csv("~/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs/res_extrmean_alert_con.csv")
data.subgrp <- c("grp1", "grp2")
grp1 <- merge(read.csv("basic_CBDC_img5behav.csv"), data.index, by="Scan_ID")
grp2 <- merge(read.csv("basic_CBDA_img5behav.csv"), data.index, by="Scan_ID")

#----------------------------------------

# grp1 <- ddply(grp1, .(Group), subset,
#               A_NoDoub_mean > mean(A_NoDoub_mean)-sd(A_NoDoub_mean)*2 & A_NoDoub_mean < mean(A_NoDoub_mean)+sd(A_NoDoub_mean)*2)
# grp2 <- ddply(grp2, .(Group), subset,
#               A_NoDoub_mean > mean(A_NoDoub_mean)-sd(A_NoDoub_mean)*2 & A_NoDoub_mean < mean(A_NoDoub_mean)+sd(A_NoDoub_mean)*2)

#----------------------------------------

data.fig <- data.frame(matrix(NA,0,3))
for (igrp in  c(1:length(data.subgrp))) {
  for (icond in c(1:length(condname.figshow))) {
    data.con <- data.frame(rep(condname.figshow[icond], nrow(get(data.subgrp[igrp]))))
    data.ned <- data.frame(get(data.subgrp[igrp])[c("Group",condname.colname[icond])])
    
    data.tem <- cbind(data.con, data.ned)
    colnames(data.tem) <- c("Index","Group","Index.Data")
    
    data.fig <- rbind(data.fig, data.tem)
  }
}

# p.sign <- c(1,2,3)
# p.line <- c(1,2,3)
# for (icond in c(1:length(condname.figshow))) {
#   subset.fig <- ddply(data.fig, .(Index), subset, Index == condname.figshow[icond] )
#   grp1  <- ddply(subset.fig, .(Index), subset, Group == data.subgrp[1])
#   grp2  <- ddply(subset.fig, .(Index), subset, Group == data.subgrp[2])
#   if (mean(grp1$Index.Data) > mean(grp2$Index.Data)) {
#     p.sign[icond] <- mean(grp1$Index.Data) + describe(grp1$Index.Data)$se * 3.5
#     p.line[icond] <- mean(grp1$Index.Data) + describe(grp1$Index.Data)$se * 2
#   } else {
#     p.sign[icond] <- mean(grp2$Index.Data) + describe(grp2$Index.Data)$se * 3.5
#     p.line[icond] <- mean(grp1$Index.Data) + describe(grp1$Index.Data)$se * 2
#   }
# }

data.barfig <- ggbarplot (data.fig, x="Index", y="Index.Data",
                          #ylim = c(2,10),
                          title = F, xlab = F, ylab = F,
                          #add = c("mean_se", "jitter"),  add.params = list(size = 1),
                          add = "mean_se",  add.params = list(size = 1.6),
                          color = "Group", fill = "Group", position = position_dodge(0.8)) + 
  stat_compare_means (aes(group = Group), method = "t.test",
                      label = "p.signif", # p.signif, p.format
                      label.y = p.y,
                      size = 5) +
  # geom_jitter(aes(x = Index, y = Index.Data, colour = factor(Group))) +
  scale_fill_manual (values=c("darkorange","gray58"),name = "Group", labels = c("Children", "Adults")) +
  scale_color_manual (values=c("darkorange","gray58"),name = "Group", labels = c("Children", "Adults"),
                      guide = FALSE) +
  # geom_signif(stat="identity", 
  #             data=data.frame(x=c(0.8, 1.8, 2.8), xend=c(1.2, 2.2, 3.2),
  #                             y=c(p.line[1], p.line[2], p.line[3]), 
  #                             annotation=c("", "", "")),
  #             aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  theme(
    plot.title = element_text(size = 15, colour = "black", face = "bold", hjust = 0.5),
    axis.ticks = element_line(size = 0.6, colour = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x = element_line(colour = "black", size = 0.8),
    axis.line.y = element_line(colour = "black", size = 0.8),
    axis.text = element_text(size = 15, colour = 'black'),
    axis.text.x = element_text(size=15,vjust = 1, hjust = 1, angle = 30),
    axis.title = element_text(size = 20, colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.position = "none",
    legend.justification = c(0.5,1))
data.barfig

fig.name <- paste("fig_img_compareCA_ANT_", data.grp, ".tiff", sep = "")
ggsave(fig.name, path = fig.savedir, data.barfig, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
