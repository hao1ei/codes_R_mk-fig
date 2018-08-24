# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(plyr);library(ggplot2);library(RColorBrewer);
suppressPackageStartupMessages(library(psych))
fig.dpi <- 100; fig.wid <- 20; fig.hei <- 15
fig.savedir <- "~/Downloads"

#----------------------------------------
setwd ("~/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs/ANT_3SD")
data.fig <- read.csv("basic_CBDC_all.csv")

# setwd ("~/xDocum/Projects/BrainDev_ANT/Image")
# cond.name <- c("Alert")
# data.index <- read.csv("res_rsa_multi2one_ChiC_AduC.csv")
# data.fig <- merge(data.fig, data.index, by="Scan_ID")

name.colname <- c("A_NoDoub_mean_abs",	"O_CenSpat_mean_abs",	"C_InconCon_mean_abs")
name.figshow <- name.colname
#----------------------------------------

for (i in 1:length(name.colname)) {
  data.line <- data.frame(data.fig[c("Scan_ID","Group","Age1", "Age2", rep(name.colname[i], 3))])
  
  colnames(data.line)=c("Scan_ID","Group","Age1", "Age2", "Index.Fig", "Index.m", "Index.se")
  data.line <- ddply(data.line, .(Age2), transform, Index.m = mean(Index.m))
  data.line <- ddply(data.line, .(Age2), transform, Index.se = describe(Index.Fig)$se)
  
  fig.agedev <- ggplot(data = data.line) + 
    # facet_grid(. ~ Group, scales = "free_x") +
    geom_jitter(aes(x = Age2, y = Index.Fig, colour = factor(Age2))) + 
    #scale_color_manual(values=brewer.pal(9, "OrRd")[2:8]) +
    geom_errorbar(aes(x = Age2, ymin = Index.m-Index.se, ymax = Index.m+Index.se), 
                  width = 0.1, size = 0.5, colour = "salmon2") + 
    geom_line(aes(x = Age2, y = Index.m), size = 1.1, colour = "salmon2") + 
    geom_point(aes(x = Age2, y = Index.m), size = 2, colour = "salmon2") + 
    #geom_smooth(aes(x = Age2, y = Index.Fig), method = "lm", colour = "red") +
    labs(x = "Age", y =" ", title = " ") +
    scale_x_continuous(breaks = c(7,8,9,10,11,12), labels = c(7,8,9,10,11,12)) + 
    #scale_x_continuous(breaks=c(19,20,21,22,23,24,25), labels=c(19,20,21,22,23,24,25)) + 
    theme(
      plot.title = element_text(size = 15, colour = "black", face = "bold", hjust = 0.5),
      axis.ticks = element_line(size = 0.6, colour = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line.x = element_line(colour = "black", size = 0.8),
      axis.line.y = element_line(colour = "black", size = 0.8),
      axis.text = element_text(size = 15, colour = 'black'),
      axis.title = element_text(size = 20, colour = "black"),
      panel.background = element_rect(fill = "white"),
      legend.position="none")
  fig.agedev
  
  fig.name <- paste("fig_agedev_line_", name.figshow[i], ".tiff", sep = "")
  ggsave(fig.name, path=fig.savedir, fig.agedev, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
}