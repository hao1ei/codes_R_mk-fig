# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(plyr);library(ggplot2);library(ggsignif)
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(psych))

fig.dpi <- 600; fig.wid <- 20; fig.hei <- 15; p.pos <- 66
fig.savedir <- "~/Downloads"
setwd("/Users/haol/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs/ANT_3SD")

condname.figshow <- c("Alerting", "Orienting", "Conflict")
condname.colname <- c("A_NoDoub_mean_abs",	"O_CenSpat_mean_abs",	"C_InconCon_mean_abs")

#----------------------------------------
data.grp    <- "SWU"
data.subgrp <- c("SWUC", "SWUA")
SWUC <- read.csv("basic_SWUC_all.csv")
SWUA <- read.csv("basic_SWUA_all.csv")

#----------------------------------------
data.grp    <- "CBD"
data.subgrp <- c("CBDC", "CBDA")
CBDC <- read.csv("basic_CBDC_all.csv")
CBDA <- read.csv("basic_CBDA_all.csv")

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

p.sign <- c(1,2,3)
p.line <- c(1,2,3)
for (icond in c(1:length(condname.figshow))) {
  subset.fig <- ddply(data.fig, .(Index), subset, Index == condname.figshow[icond] )
  grp1  <- ddply(subset.fig, .(Index), subset, Group == data.subgrp[1])
  grp2  <- ddply(subset.fig, .(Index), subset, Group == data.subgrp[2])
  if (mean(grp1$Index.Data) > mean(grp2$Index.Data)) {
    p.sign[icond] <- mean(grp1$Index.Data) + describe(grp1$Index.Data)$se * 3.5
    p.line[icond] <- mean(grp1$Index.Data) + describe(grp1$Index.Data)$se * 2
  } else {
    p.sign[icond] <- mean(grp2$Index.Data) + describe(grp2$Index.Data)$se * 3.5
    p.line[icond] <- mean(grp1$Index.Data) + describe(grp1$Index.Data)$se * 2
  }
}

data.barfig <- ggbarplot (data.fig, x="Index", y="Index.Data",
                          #ylim = c(20,65),
                          title = " ", xlab = " ", ylab = " ",
                          #add = c("mean_se", "jitter"),  add.params = list(size = 1),
                          add = "mean_se",  add.params = list(size = 1.6),
                          size = 1.3,
                          color = "Group", fill = "Group", position = position_dodge(0.8)) + 
  stat_compare_means (aes(group = Group), method = "t.test",
                      label.y = c(p.sign[1], p.sign[2], p.sign[3]),
                      #label.y = p.pos,
                      label = "p.format", size = 5) + # signif format
  #geom_jitter(aes(x = Index, y = Index.Data,colour = factor(Group)), position = position_jitterdodge()) +
  scale_color_manual (values=c("darkorange","gray58"),name = "Group", labels = c("Children", "Adults")
                      #guide = FALSE
  ) +
  scale_fill_manual (values=c("darkorange","gray58"),name = "Group", labels = c("Children", "Adults")) +
  scale_x_discrete(breaks=condname.figshow, labels=c()) + 
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
    axis.text = element_text(size = 25, colour = 'black'),
    axis.title = element_text(size = 20, colour = "black"),
    panel.background = element_rect(fill = "white"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.position = "none")
data.barfig

fig.name <- paste("fig_indexcomp_ANT_", data.grp, ".tiff", sep = "")
ggsave(fig.name, path = fig.savedir, data.barfig, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
