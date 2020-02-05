# Alex Bettinardi
# 5-22-19
# A script to verify how well UGB population controls are being achieved.

# First read persons output
per <- read.csv("output/GP_synthetic_persons.csv",as.is=T)

# read the ugb totals that need to be achieved
ugb <- read.csv("data/GP_ugbData.csv",as.is=T,row.names=1)
# Add total population column
ugb$Total <- ugb$MALE + ugb$FEMALE

# read in crosswalk table
cw <- read.csv("data/geo_cross_walk.csv",as.is=T,row.names=1)

# add UGB field to the persons record
per$UGB <- cw[as.character(per$MAZ),"UGB_NAME"]
perUGB <-table(per$UGB)

# plot result
png("plots/GP_PRC_UGB_Compare.png")

par(mar=c(7,4,4,2)+0.1)
plot(ugb$Total, main="Comparison of PRC UGB Population Controls vs Results", ylab="Total Population", xlab="",axes=F, ylim=range(ugb$Total)+(c(-.1,.1)*max(ugb$Total)))
box()
axis(1,1:nrow(ugb),rownames(ugb), las=2)
axis(2)
arrows(1:nrow(ugb),ugb$Total,1:nrow(ugb),perUGB[rownames(ugb)], col="red",angle=90, length=nrow(ugb)/100)
text(1:nrow(ugb),((perUGB[rownames(ugb)]+ugb$Total)/2)+ifelse(abs(perUGB[rownames(ugb)]-ugb$Total)<5000,5000,0),round(perUGB[rownames(ugb)]-ugb$Total))
text(1:nrow(ugb),((perUGB[rownames(ugb)]+ugb$Total)/2)-5000,paste0(round(100*(perUGB[rownames(ugb)]-ugb$Total)/ugb$Total),"%"))
dev.off()

hhUGB <-tapply(per$household_id,per$UGB, function(x) length(unique(x)))
perUGB/hhUGB

# read in taz data
taz <- read.csv("data/GP_tazData.csv",as.is=T,row.names=1)
tazUGBcw <- tapply(cw$UGB_NAME, cw$TAZ, unique)
Check <- unlist(lapply(tazUGBcw,length))
if(length(Check[Check>1])) print(paste("The following TAZs have more than one UGB", paste(Check[Check>1], collapse=", ")))
taz$UGB <- tazUGBcw[rownames(taz)]

HHs <- tapply(rowSums(taz[,paste0("HHSIZE",1:4)]),taz$UGB,sum)

ugb$HHs <- HHs[rownames(ugb)]
ugb$avgHHsize <-  ugb$Total/ugb$HHs 

HHs <- tapply(rowSums(sweep(taz[,paste0("HHSIZE",1:4)],2,c(1:3,4.5),"*")),taz$UGB,sum)/HHs

ugb$TAZinputAvgHHsize <- HHs[rownames(ugb)]

write.csv(ugb,"output/Ugb_HHsize_Analysis.csv")