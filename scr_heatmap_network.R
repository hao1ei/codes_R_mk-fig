# written by hao (ver_18.06.08)
# rock3.hao@gmail.com
# qinlab.BNU

rm(list = ls())
library(plyr);library(ggplot2);library(reshape2)
fig.dpi <- 100; fig.wid <- 15; fig.hei <- 15

data.dir   <- "/Users/haol/Dropbox/Docums/Projects/BrainDev_ANT/AnalyDocs"
fig.savedir <- "~/Downloads"

con.name <- "Alert"
net.name <- "FF"
node.num <- 15
mat.part <- "down" # up / down / mean
data1 <- read.csv(paste(data.dir,"/basic_CBDC_img5behav.csv",sep=""))
data2 <- read.csv(paste(data.dir,"/res_corrvect_ANT_InconFlk_FF.csv",sep=""))

data1    <- data1[c('Scan_ID','Group','Gender','Age1','Age2')]
data.fig <- merge(data1, data2, by="Scan_ID")

age.fig <- unique(data.fig$Age2)
for (age.grp in age.fig) {
  grp.fig  <- ddply(data.fig, .(Group), subset, Age2 == age.grp)
  grp.fig  <- grp.fig[,(ncol(data1)+1):(ncol(data1)+node.num^2)]
  mean.fig <- colMeans(grp.fig)
  
  mat.fig <- matrix(mean.fig, nrow=node.num, byrow=TRUE)
  #mat.fig <- scale(mat.fig, center=TRUE, scale=TRUE)
  
  mat.figX <- array(0, dim=c(node.num, node.num))
  for (m_i in 1:node.num) { 
    for (m_j in 1:node.num) {
      if (mat.part == 'up'){
        mat.figX[upper.tri(mat.figX)] <- mat.fig[upper.tri(mat.fig)]
        mat.figX <- mat.figX + t(mat.figX)
      }
      if (mat.part == 'down'){
        mat.figX[lower.tri(mat.figX)] <- mat.fig[lower.tri(mat.fig)]
        mat.figX <- mat.figX + t(mat.figX)
      }
      if (mat.part == 'mean'){
        mat.figX[m_i,m_j] <- (mat.fig[m_i,m_j] + mat.fig[m_j,m_i])/2
      }
    }}
  for (m_i in 1:node.num) { mat.figX[m_i,m_i] <- 0 }
  
  colnames(mat.figX) = paste("ROI.", 1:node.num, sep = "")
  rownames(mat.figX) = paste("ROI.", 1:node.num, sep = "")
  mat.figX <- melt(mat.figX)
  colnames(mat.figX)=c("ROI1","ROI2", "T.value")
  
  net.fig <- ggplot(mat.figX, aes(ROI1, ROI2)) + geom_tile(aes(fill = T.value)) + coord_fixed(ratio=1) +
    scale_fill_gradient2(low="dodgerblue3", mid="white", high="firebrick3", midpoint = 0) +
    labs(x = "", y = "", title = "") +
    scale_x_discrete(position = "top") + 
    scale_y_discrete(limits = rev(levels(mat.figX$ROI2))) +
    theme(
      plot.title = element_text(size=15, colour="black", face="bold", hjust=0.5),
      axis.ticks.length = unit(0, "cm"),
      axis.text = element_text(size=8, colour="black", face="bold"),
      legend.position='right')
  net.fig
  
  fig.name <- paste("Network_", con.name, "_", net.name,"_Age", age.grp, ".tiff", sep = "")
  ggsave(fig.name, path=fig.savedir, net.fig, width=fig.wid, height=fig.hei, units="cm", dpi=fig.dpi)
}
