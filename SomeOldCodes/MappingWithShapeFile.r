

mapping.shape <- function(){
#setwd("c:/Susumu/R-Programme/Daten/")
setwd("d:/toshiba/R-Programme/Daten/")

  daten05 <- read.table(file="btw2005.csv", header = TRUE ,sep="\t", row.names = NULL)

  daten05 <- daten05[order(daten05$wkr),]

  wkr.id <- daten05[,3] # berlin = 11
  wkr.id <- ifelse(wkr.id<11,0,1)

  daten05 <- daten05[,c(10,12,14,16,18,20)]
  daten05[is.na(daten05)] <- 0
  daten05[,2] <- daten05[,2]+daten05[,3] # cdu = cdu+csu
  daten05 <- daten05[,c(1,2,4:6)]        # delete csu

  ranking <- t(apply(daten05,1,rank,na.last=F))
  ranking[ranking<(dim(daten05)[2]-1)] <- 0
  ranking[ranking>(dim(daten05)[2]-2)] <- 1
  dominated.05 <- ranking[,1]+ranking[,2]

  WKR_NR <- c(1:299)
  dominated.05 <- cbind(WKR_NR,dominated.05)


# btw2002

  daten4902 <- read.table(file="union-spd-dominanz.csv", header = TRUE ,sep="\t", row.names = NULL)

  daten02 <- daten4902[daten4902$jahr==2002,]

  daten02$dominanz <- ifelse(daten02$dominanz==1,2,1)

  names(daten02)[1] <- c("WKR_NR")

  dominated.02 <- daten02



library(maptools)
library(foreign)
library(sp)

#setwd("C:/Susumu/R-Programme/geodata")
setwd("d:/toshiba/r-Programm/geodata")

btw2005.05 <- read.shape("Geometrie_Wahlkreise_16DBT.shp")
btw2005.02 <- read.shape("Geometrie_Wahlkreise_16DBT.shp")
btw2005.05$att.data <- merge(dominated.05,btw2005.05$att.data,sort=F,by="WKR_NR")
btw2005.02$att.data <- merge(dominated.02,btw2005.02$att.data,sort=F,by="WKR_NR")

#temp <- btw2005$att.data

cols <- c("gray","white","blue","navy")

classes.05 <- btw2005.05$att.data$dominated.05
classes.02 <- btw2005.02$att.data$dominanz

mappoly.05 <- Map2poly(btw2005.05)
mappoly.02 <- Map2poly(btw2005.02)

pdf("C:/localtexmf/docs/MPSA2006DominatedBRD.pdf",width=12,height=8,pointsize=12)
par(mfrow=c(1,2))
plot.polylist(mappoly.02,col=cols[ordered(classes.02)],xlab="",ylab="",axes=F,main="2002")
plot.polylist(mappoly.05,col=cols[ordered(classes.05)],xlab="",ylab="",axes=F,main="2005")
par(mfrow=c(1,1))
dev.off()


# only DDR

btwData.05 <- btw2005.05$att.data
btwPoly.05 <- Map2poly(btw2005.05)

ddrData.05 <- subset(btwData.05,btwData.05$LAND_NR>=11)
ddrPoly.05 <- subset(btwPoly.05,btwData.05$LAND_NR>=11)

classes.05 <- ddrData.05$dominated.05

#btwxy <- get.Pcent(btw2005)
#btwx <- btwxy[,1]
#btwy <- btwxy[,2]
#x <- subset(btwx,btwData$LAND_NR>=11)
#y <- subset(btwy,btwData$LAND_NR>=11)

btwData.02 <- btw2005.02$att.data
btwPoly.02 <- Map2poly(btw2005.02)

ddrData.02 <- subset(btwData.02,btwData.02$LAND_NR>=11)
ddrPoly.02 <- subset(btwPoly.02,btwData.02$LAND_NR>=11)

classes.02 <- ddrData.02$dominanz


pdf("C:/localtexmf/docs/MPSA2006DominatedDDR.pdf",width=12,height=8,pointsize=12)
par(mfrow=c(1,2))
plot.polylist(ddrPoly.02,col=cols[ordered(classes.02)],xlab="",ylab="",axes=F,main="2002")
plot.polylist(ddrPoly.05,col=cols[ordered(classes.05)],xlab="",ylab="",axes=F,main="2005")
par(mfrow=c(1,1))
dev.off()



jpn <- read.shape("japan_jdg.shp")
jpndata <- jpn$att.data
jpnpoly <- Map2poly(jpn,region.id=attr(jpn,"region.id"),raw=F)
jpnxy <- get.Pcent(jpn)
jpnx <- jpnxy[,1]
jpny <- jpnxy[,2]
gunmadata <- subset(jpndata,jpndata$PREF=="???")
gunmapoly <- subset(jpnpoly,jpndata$PREF=="???")
x <- subset(jpnx,jpndata$PREF=="???")
y <- subset(jpny,jpndata$PREF=="???")
aged <- read.delim("agedprop.txt")
gunmadata <- merge(aged,gunmadata,sort=F,by="JCODE")
DD <- gunmadata$AP2003
}
