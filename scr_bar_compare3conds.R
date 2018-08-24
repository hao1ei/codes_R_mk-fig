# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
# library(plyr)
# library(ggplot2)
library(ggsignif)
suppressPackageStartupMessages(library(ggpubr))
# suppressPackageStartupMessages(library(psych))
fig.dpi <- 600; fig.wid <- 32; fig.hei <- 8; p.y = 9.5

setwd ("/Users/haol/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs")
fig.savedir <- "~/Downloads"

#----------------------------------------
condname.colname <- c("FF_cond_spl7_l_fwe05_roi","FF_cond_spl7_r_fwe05_roi",
                      "FF_cond_fef_l_fdr05_roi","FF_cond_fef_r_fdr05_roi",
                      "FF_cond_fm_l_fdr05_roi","FF_cond_fm_r_fdr05_roi",
                      "FF_cond_tpj_l_fdr05_roi","FF_cond_tpj_r_fdr05_roi",
                      "FF_cond_dacc_l_fdr05_roi","FF_cond_dacc_r_fdr05_roi",
                      "FF_cond_ai_l_fdr05_roi","FF_cond_ai_r_fdr05_roi",
                      "FF_cond_lo_l_fwe05_roi","FF_cond_lo_r_fwe05_roi",
                      "FF_cond_cuneus_lr_fwe05_roi")
condname.show <- condname.colname

#----------------------------------------
data.index <- read.csv("/Users/haol/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs/basic_CBDA_img5behav.csv")
data.subcond <- c("cond1", "cond2", "cond3")
cond1 <- merge(read.csv("res_extrmean_alert_con.csv"), data.index, by="Scan_ID")
cond2 <- merge(read.csv("res_extrmean_orient_con.csv"), data.index, by="Scan_ID")
cond3 <- merge(read.csv("res_extrmean_conflict_con.csv"), data.index, by="Scan_ID")

#----------------------------------------

data.fig <- data.frame(matrix(NA,0,3))
for (icond in  c(1:length(data.subcond))) {
  for (icol in c(1:length(condname.colname))) {
    data.con <- data.frame(rep(condname.colname[icol], nrow(get(data.subcond[icond]))))
    data.ned <- data.frame(get(data.subcond[icond])[c("Conds",condname.colname[icol])])
    
    data.tem <- cbind(data.con, data.ned)
    colnames(data.tem) <- c("Index","Conds","Index.Data")
    
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
                          title = "", xlab = "", ylab = "",
                          #add = c("mean_se", "jitter"),  add.params = list(size = 1),
                          add = "mean_se",  add.params = list(size = 0.6),
                          size = 1,
                          color = "Conds", fill = "Conds", position = position_dodge(0.8)) +
  # stat_compare_means (aes(group = Conds), method = "t.test", 
  #                    label = "p.signif", 
  #                    label.y = p.y, 
  #                    size = 5) +
  # geom_jitter(aes(x = Index, y = Index.Data, colour = factor(Group))) +
  
  scale_fill_manual (values=c("lightcoral","darkolivegreen3","deepskyblue"),name = "Conds", labels = c("a","o","c")) +
  scale_color_manual (values=c("lightcoral","darkolivegreen3","deepskyblue"),name = "Conds", labels = c("a","o","c"),
                      guide = FALSE) +
  # data.barfig <- ggplot(data.fig, aes(x=Index, y=Index.Data, fill=Conds)) + 
  #   geom_bar(position = "dodge", stat="identity") +
  #geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.2, position=position_dodge(.9)) +
  # scale_fill_manual (values=c("yellow3","mediumvioletred","darkcyan"),name = "Conds", labels = c("ao","ac","oc")) +
  # scale_color_manual (values=c("yellow3","mediumvioletred","darkcyan"),name = "Conds", labels = c("a","o","c"),
  #                     guide = FALSE) +
  scale_x_discrete(breaks=condname.colname, labels=c("","","","","","","","","","","","","","","")) + 
  coord_cartesian(ylim=c(-7,10))+
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
    axis.text.x = element_text(vjust = 1, hjust = 1, angle = 30),
    axis.title = element_text(size = 20, colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none"
    # legend.title = element_text(size = 18),
    # legend.text = element_text(size = 15),
    # legend.justification = c(0.5,1)
  )
data.barfig

fig.name <- paste("fig_bar_compare3cond_dissim_a.tiff", sep = "")
ggsave(fig.name, path = fig.savedir, data.barfig, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
