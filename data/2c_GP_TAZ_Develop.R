# TAZ_Develop.R
# Alex Bettinardi
# 1-29-19
# 9-26-19 jxr to streamline GP data creation

# Script to check re-build the TAZ control based on new MAZ totals

# read syn pop input tables
meta <- read.csv("data/GP_metaData.csv",as.is=T)
taz <- read.csv("data/GP_tazData_orig.csv", as.is=T)
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
if(any(summary(rowSums(taz[,grep("INC",names(taz))]) - rowSums(taz[,grep("SIZE",names(taz))]))!=0)) stop("Inc and Size Households don't match at a TAZ level")
if(any(summary(rowSums(taz[,grep("INC",names(taz))]) - rowSums(taz[,grep("WORK",names(taz))]))!=0)) stop("Inc and Worker Households don't match at a TAZ level")
if(any(summary(rowSums(taz[,grep("INC",names(taz))]) - rowSums(taz[,grep("CHILD",names(taz))]))!=0)) stop("Inc and Children Households don't match at a TAZ level")

# check hh: maz -> maz
if(any(summary(rowSums(maz[,grep("_HH",names(maz))]) - maz$HH)!=0)) stop("Households by type don't match total households at a MAZ level")

# create an MAZ household total by TAZ
mazHH.Tz <- tapply(maz$HH, cw[as.character(maz$MAZ),"TAZ"],sum)

# create a function to re-tabulate taz households by category given new total households 
ReAllocate <- function(ctrl, Dist){
                  NewDist <- round(Dist*ctrl/sum(Dist))
                  if(sum(Dist)==0) NewDist=Dist  
                    if((ctrl-sum(NewDist))!=0) {
                       ind <- grep(max(NewDist),NewDist)[1]
                       NewDist[ind] <- NewDist[ind]+ ctrl-sum(NewDist)
                    }
                    NewDist
                    }
taz[,grep("INC",names(taz))] <- t(apply(cbind(mazHH.Tz[rownames(taz)],taz[,grep("INC",names(taz))]),1,function(x) ReAllocate(x[1],x[2:length(x)])))
taz[,grep("SIZE",names(taz))] <- t(apply(cbind(mazHH.Tz[rownames(taz)],taz[,grep("SIZE",names(taz))]),1,function(x) ReAllocate(x[1],x[2:length(x)])))
taz[,grep("WORK",names(taz))] <- t(apply(cbind(mazHH.Tz[rownames(taz)],taz[,grep("WORK",names(taz))]),1,function(x) ReAllocate(x[1],x[2:length(x)])))
taz[,grep("CHILD",names(taz))] <- t(apply(cbind(mazHH.Tz[rownames(taz)],taz[,grep("CHILD",names(taz))]),1,function(x) ReAllocate(x[1],x[2:length(x)])))

# write out revised TAZ data
write.csv(taz,"data/GP_tazData.csv",row.names=F)

# read back in taz data
taz <- read.csv("data/GP_tazData.csv", as.is=T)
rownames(taz) <- taz$TAZ

# check hh maz -> taz
if(any(summary(mazHH.Tz-rowSums(taz[names(mazHH.Tz),grep("INC",names(taz))]))!=0)) stop("TAZ HH totals versus MAZ HH totals don't align")
if(any(summary(mazHH.Tz-rowSums(taz[names(mazHH.Tz),grep("SIZE",names(taz))]))!=0)) stop("TAZ HH totals versus MAZ HH totals don't align")
if(any(summary(mazHH.Tz-rowSums(taz[names(mazHH.Tz),grep("WORK",names(taz))]))!=0)) stop("TAZ HH totals versus MAZ HH totals don't align")
if(any(summary(mazHH.Tz-rowSums(taz[names(mazHH.Tz),grep("CHILD",names(taz))]))!=0)) stop("TAZ HH totals versus MAZ HH totals don't align")

# check population, but won't be perfect
taz$Pop <- rowSums(sweep(taz[,grep("SIZE",names(taz))],2,1:4,"*"))
print("TAZ Pop (by size) compared to Meta Pop - should never be greater than 1")
print(tapply(taz$Pop, cw[match(taz$TAZ, cw$TAZ),"REGION"],sum)/rowSums(meta[,grep("AGE",names(meta))]))


#Worker/occupation totals
cat("TAZ workers (by number of workers) compared to Meta workers by occupation\nshould  be as close to 1 as possible\n")
print (sum(meta[,grep("OCC",names(meta))])/sum(sweep(taz[,grep("WORK",names(taz))],2,c(0:2,3.3),"*")))


