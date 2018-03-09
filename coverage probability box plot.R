library(RColorBrewer)

# load the data and convert to a matrix
p <- read.csv("coverage probabilities.csv")
m <- as.matrix(p[,ncol(p):1])

# create some random data for jitter
r <-  (matrix(runif(nrow(m)*ncol(m)), nrow=nrow(m), ncol=ncol(m)) / 2) - 0.25

# create colours and colour matrix (for points)
cols  <- colorRampPalette(brewer.pal(12, "Set3"), alpha=TRUE)(ncol(m))
colsm <-matrix(rep(cols, each=nrow(m)), ncol=ncol(m))

# get the greys (refer from https://github.com/zonination/perceptions/blob/master/percept.R)
palette <- brewer.pal("Greys", n=9)
color.background = palette[2]
color.grid.major = palette[5]

# set graphical area
par(bty="n", bg=palette[2], mar=c(5,8,3,1))

# plot initial boxplot
boxplot(m~col(m), horizontal=TRUE, outline=FALSE, lty=1, staplewex=0, boxwex=0.8, boxlwd=1, medlwd=1, col=cols, xaxt="n", yaxt="n")

# plot gridlines
for (i in seq(80,100,by=1)) {
  lines(c(i,i), c(0,20), col=palette[4])
}

for (i in seq(1,9,by=1)) {
  lines(c(-5,105), c(i,i), col=palette[4])
}

# plot points
points(m, col(m)+r, col=colsm, pch=16)

# overlay boxplot
boxplot(m~col(m), horizontal=TRUE, outline=FALSE, lty=1, staplewex=0, boxwex=0.8, boxlwd=1, medlwd=1, col=cols, add=TRUE, xaxt="n", yaxt="n")

# add axes and title
axis(side=1, at=seq(80,100,by=1), col.axis=palette[7], cex.axis=0.8, lty=0, tick=NA, line=-1)
axis(side=1, at=89, labels="Coverage Probability %", lty=0, tick=NA, col.axis=palette[7])
axis(side=2, at=1:9, col.axis=palette[7], cex.axis=0.8, lty=0, tick=NA, labels=colnames(m), las=2)
#axis(side=2, at=9/2, labels="Group Setting", col.axis=palette[7], lty=0, tick=NA, las=3, line=6)
title("Simulation results when sample size = 1000")

