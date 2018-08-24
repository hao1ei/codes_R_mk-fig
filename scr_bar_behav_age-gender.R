# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(ggplot2); library(plyr)
fig.dpi <- 600; fig.wid <- 15; fig.hei <- 15

data.grp    <- c("CBDC","CBDA","SWUC","SWUA")
work.dir    <- "/Users/haol/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs/ANT_3SD"
fig.savedir <- "~/Downloads"

# =====================================================================================
setwd (work.dir)
for (grp in  c(1:length(data.grp))) {
  assign(data.grp[grp], read.csv(paste("basic_", data.grp[grp], "_all.csv", sep = "")))
}

# Age Distributed Chart
for (sub.grp in data.grp) {
  data.fig <- get(sub.grp)
  age.all   <- unique(data.fig$Age2); gender.all <- unique(data.fig$Gender)
  cnt <- 0; age.fig <- 0; gender.fig <- 0; subjnum.fig <- 0
  for (i in age.all) {
    for (j in gender.all) {
      cnt <- cnt + 1
      age.fig[cnt]     <- i
      gender.fig[cnt]  <- j
      
      if (length(subset(data.fig, Age2 == i & Gender == j)$Age2) == 0) {
        subjnum.fig[cnt] <- 0
      } else {
        subjnum.fig[cnt] <- count(subset(data.fig, Age2 == i & Gender == j)$Age2)$freq
      }
    }
  }
  agegender.fig <- data.frame(age.fig, gender.fig, subjnum.fig)
  
  agegender.bar <- ggplot(data = agegender.fig) +
    
    geom_bar(stat="identity", aes(x=age.fig, y=subjnum.fig, fill=factor(gender.fig))) +
    scale_fill_manual(values=c("cornflowerblue","salmon"),name="Gender", labels=c("Male", "Female")) +
    # colour=factor(gender.fig)
    #scale_color_manual(values=c("cornflowerblue","salmon"),name="Gender", labels=c("Male", "Female")) +
    
    
    #geom_text(aes(x=age.fig, y=subjnum.fig, label=factor(subjnum.fig)), position=position_stack(vjust=0.9), size=9) +
    labs(x=" ", y=" ", title=" ") + 
    scale_x_continuous(breaks=age.all, labels=age.all) +
    theme(
      plot.title=element_text(size=15, colour='black', face='bold', hjust=0.5),
      axis.ticks=element_line(size=0.6, colour='black'),
      axis.ticks.length=unit(0.2, "cm"),
      axis.line.x=element_line(colour='black', size=0.8),
      axis.line.y=element_line(colour='black', size=0.8),
      axis.text=element_text(size=15, colour='black'),
      axis.title=element_text(size=20, colour='black'),
      panel.background=element_rect(fill='white'),
      legend.title=element_text(size=18),
      legend.text=element_text(size=15),
      legend.position="none",
      legend.justification=c(0.5,1)
    )
  agegender.bar
  
  fig.name <- paste("ANT_AgeDist_", unique(data.fig$Group), ".tiff", sep = "")
  ggsave(fig.name, path=fig.savedir, agegender.bar, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
}