# script to tabulate GQ output and subract it from total population controls for the GP run
# Alex original and Jin streamlined on 9/25/2019

# get current work directory
curDir <- getwd()

# back to the main directory
setwd("..")

# set up GQ and GP direcotries
gqDir <- "1_PopSim_GQ_SOABM"
gpDir <- "2_PopSim_GP_SOABM"

# read GQ output
gqPer <- read.csv(paste0(gqDir,"/output/GQ_synthetic_persons.csv"),as.is=T)

# read CW (to UGB)
cw <- read.csv(paste0(gqDir,"/data/geo_cross_walk.csv"),row.names=1,as.is=T)

# add UGB to gq
gqPer$UGB <- cw[as.character(gqPer$MAZ),"UGB_NAME"]

# tabulate ugb totals
ugbGQ <- table(gqPer$UGB, c("MALE","FEMALE")[gqPer$SEX])

# tabulate puma totals
pumaGQ <- table(gqPer$PUMA,cut(gqPer$AGEP,breaks=c(-1,5,12,15,17,24,34,44,54,64,74,84,99999),labels=paste0("AGE",1:12)))

# read in the Full Controls
ugb <- read.csv(paste0(gpDir,"/data/ugbGPop.csv"),as.is=T)
puma <- read.csv(paste0(gpDir,"/data/metaGPop.csv"),as.is=T)

# write out updated controls for just the general population (minus the GQ)
ugbOut <- ugb
ugbOut[ugb$UGB_NAME %in% rownames(ugbGQ),c("MALE","FEMALE")] <- ugb[ugb$UGB_NAME %in% rownames(ugbGQ),c("MALE","FEMALE")] - ugbGQ[ugb[ugb$UGB_NAME %in% rownames(ugbGQ),"UGB_NAME"],c("MALE","FEMALE")]
write.csv(ugbOut, paste0(gpDir, "/data/GP_ugbData.csv"),row.names=F) 

pumaOut <- puma
rownames(pumaOut) <- puma$PUMA
pumaOut[rownames(pumaGQ), colnames(pumaGQ)] <- pumaOut[rownames(pumaGQ), colnames(pumaGQ)] - pumaGQ
pumaOut[rownames(pumaGQ), "POP"] <- rowSums(pumaOut[rownames(pumaGQ), paste0("AGE",1:12)])
write.csv(pumaOut, paste0(gpDir, "/data/GP_metaData.csv"),row.names=F) 

