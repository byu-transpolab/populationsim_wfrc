# inputCheck.R
# Alex Bettinardi
# 5-30-17
# 3-1-18 edited AB
# 10-03-18 updated by JXR for 2017 PopSim HH Data
# 09-26-19 JXR streamlined as 2c1_CP_InputCheck.r

# Script to check for consistencies across syn pop input tables

# read syn pop input tables
meta <- read.csv("data/GP_metaData.csv",as.is=T)
taz <- read.csv("data/GP_tazData.csv", as.is=T)
rownames(taz) <- taz$TAZ
maz <- read.csv("data/GP_mazData.csv",as.is=T)
  #newCW <- cbind(MAZ=maz$MAZ,TAZ=floor(maz$MAZ/100),PUMA=900,REGION=1)
cw <- read.csv("data/geo_cross_walk.csv",as.is=T)
rownames(cw) <- cw$MAZ

# first verify all zone numbers align
# maz
if(length(maz$MAZ[!maz$MAZ %in% cw$MAZ])>0) stop("MAZ numbers don't match crosswalk")
if(length(cw$MAZ[!cw$MAZ %in% maz$MAZ])>0) stop("MAZ numbers don't match crosswalk")
# taz
if(length(taz$TAZ[!taz$TAZ %in% unique(cw$TAZ)])>0) stop("TAZ numbers don't match crosswalk")
if(length(unique(cw$TAZ)[!unique(cw$TAZ) %in% taz$TAZ])>0) stop("TAZ numbers don't match crosswalk")
# meta
if(length(meta$REGION[!meta$REGION %in% unique(cw$REGION)])>0) stop("Meta numbers don't match crosswalk")
if(length(unique(cw$REGION)[!unique(cw$REGION) %in% meta$REGION])>0) stop("MAZ numbers don't match crosswalk")

# do totals align
# check hh: taz -> taz
if((any(as.integer(rowSums(taz[,grep("INC",names(taz))]) - rowSums(taz[,grep("SIZE",names(taz))]))))!=0) stop("Inc and Size Households don't match at a TAZ level")
if((any(as.integer(rowSums(taz[,grep("INC",names(taz))]) - rowSums(taz[,grep("WORK",names(taz))]))))!=0) stop("Inc and Worker Households don't match at a TAZ level")
if((any(as.integer(rowSums(taz[,grep("INC",names(taz))]) - rowSums(taz[,grep("CHILD",names(taz))]))))!=0) stop("Inc and Children Households don't match at a TAZ level")

# check hh: maz -> maz
if(any(summary(rowSums(maz[,grep("_HH",names(maz))]) - maz$HH)!=0)) stop("Households by type don't match total households at a MAZ level")

# check hh maz -> taz
mazHH.Tz <- tapply(maz$HH, cw[as.character(maz$MAZ),"TAZ"],sum)
if(any(round(summary(mazHH.Tz-rowSums(taz[names(mazHH.Tz),grep("INC",names(taz))])))!=0)) stop("TAZ HH totals versus MAZ HH totals don't align")

# check population, but won't be perfect
taz$Pop <- rowSums(sweep(taz[,grep("SIZE",names(taz))],2, c(1:3, 4.50), "*"))
print("TAZ Pop (by size) compared to Meta Pop - should never be greater than 1")
print(tapply(taz$Pop, cw[match(taz$TAZ, cw$TAZ),"REGION"],sum)/meta$POP)

# check population at meta level
if(any(summary(rowSums(meta[,grep("AGE",names(meta))]) - meta$POP)!=0)) stop("Total Pop and Pop by Age don't match at a Meta level")

#Worker/occupation totals by REGION (PUMA)
cat("TAZ workers (by number of workers) compared to Meta workers by occupation\nshould  be as close to 1 as possible\n")
meta$WORKER <- rowSums(meta[,grep("OCC",names(meta))])
taz$WORKER <- rowSums(sweep(taz[,grep("WORK",names(taz))],2,c(0:2,3.25),"*"))
print (tapply(taz$WORKER, cw[match(taz$TAZ, cw$TAZ),"REGION"],sum)/meta$WORKER)

#meta[,grep("OCC",names(meta))]<- round(meta[,grep("OCC",names(meta))]/(sum(meta[,grep("OCC",names(meta))])/sum(sweep(taz[,grep("WORK",names(taz))],2,c(0:2,3.3),"*"))))
#write.csv(meta,"metaData.csv",row.names=F)
