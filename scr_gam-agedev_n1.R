# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(visreg);library(ggplot2);library(gtools);library(RColorBrewer);library(gtools)
suppressPackageStartupMessages(library(mgcv))
fig.dpi <- 600; fig.wid <- 20; fig.hei <- 15
fig.savedir <- "~/Downloads"

setwd ("~/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs")
data.res <- read.csv("basic_CBDC_img5behav.csv")

#----------------------------------------
con.col <- c(brewer.pal(9, "Oranges")[4:7], brewer.pal(9, "Greens")[4:7])
# con.col <- c("lightcoral","darkolivegreen3","deepskyblue")

# con.name <- c("A_NoDoub_mean", "O_CenSpat_mean", "C_InconCon_mean")
# con.fig <- data.frame(c(rep("con1", 171), rep("con2", 171), rep("con3", 171)))
# age.fig <- rbind(data.res["Age1"],data.res["Age1"],data.res["Age1"])
# sex.fig <- rbind(data.res["Gender"],data.res["Gender"],data.res["Gender"])
# rt.rep <- smartbind(data.res[con.name[1]],data.res[con.name[2]],data.res[con.name[3]],fill=0)

con.name <- c("ACC_Con_No",	"ACC_Con_Cent",	"ACC_Con_Doub",	"ACC_Con_Spat",
              "ACC_Incon_No",	"ACC_Incon_Cent",	"ACC_Incon_Doub",	"ACC_Incon_Spat")
con.fig <- data.frame(c(rep("con1", 150), rep("con2", 150), rep("con3", 150), rep("con4", 150),
                        rep("con5", 150), rep("con6", 150), rep("con7", 150), rep("con8", 150)))
age.fig <- rbind(data.res["Age1"],data.res["Age1"],data.res["Age1"],data.res["Age1"],
                 data.res["Age1"],data.res["Age1"],data.res["Age1"],data.res["Age1"])
sex.fig <- rbind(data.res["Gender"],data.res["Gender"],data.res["Gender"],data.res["Gender"],
                 data.res["Gender"],data.res["Gender"],data.res["Gender"],data.res["Gender"])
rt.rep <- smartbind(data.res[con.name[1]],data.res[con.name[2]],data.res[con.name[3]],data.res[con.name[4]],
                    data.res[con.name[5]],data.res[con.name[6]],data.res[con.name[7]],data.res[con.name[8]],fill=0)
rt.fig <- data.frame(rowSums(rt.rep))

data.gam <- data.frame(age.fig,sex.fig,con.fig,rt.fig)
colnames(data.gam) <- c("age","sex","con","rt.con")

# Select Smoothing Parameters with REML, Using P-splines and Draw by visreg & ggplot2
# gam(networkMeasure ~ s(age, method="REML") + covariates)
fit.rt   <- gam(rt.con ~ s(age) + sex + con, data=data.gam, method="REML")

fig.agedev <- visreg(fit.rt, "age", gg=TRUE,
                     by="con",overlay=TRUE,
                     partial=FALSE,
                     band=FALSE) +
  #annotate("text", x=p.x, y=p.y, label=paste("p<",p.v,sep=""), size=5) + 
  labs(x=" ",y="",title=" ") +
  scale_x_continuous(breaks=c(7,8,9,10,11,12), labels=c(7,8,9,10,11,12)) + 
  #scale_y_continuous(limits=c(0.7,1)) +
  scale_color_manual(values=con.col, name="condition") +
  #scale_x_continuous(breaks=c(19,20,21,22,23,24,25), labels=c(19,20,21,22,23,24,25)) + 
  theme(
    plot.title=element_text(size=15, colour="black", face="bold", hjust=0.5),
    axis.ticks=element_line(size=0.6, colour="black"),
    axis.ticks.length=unit(0.2, "cm"),
    axis.line.x=element_line(colour="black", size=0.8),
    axis.line.y=element_line(colour="black", size=0.8),
    axis.text=element_text(size=15, colour='black'),
    axis.title=element_text(size=20, colour="black"),
    panel.background=element_rect(fill="white"),
    legend.position="right")
fig.agedev

fig.name <- paste("fig_gam_agedev.tiff")
ggsave(fig.name, path=fig.savedir, fig.agedev, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
