# Libraries

library(simpleaffy)
library(affy)


# Read the data
set.name <- "GSE2378"
raw.data <- read.affy(path=set.name)
num.arrays <- length(raw.data)

# First check, images
sapply(1:num.arrays, 
       FUN=function(i) {
         png(file=paste(set.name, "_image_array", i,".png",sep=""))
         image(raw.data[,i])
         dev.off()
       })
       
# Boxplot and distribution pre-normalization
boxplot(raw.data, col=raw.data@phenoData@data$Type)

hist(raw.data)

# RNA degradation

data.deg <- AffyRNAdeg(raw.data)
plotAffyRNAdeg(data.deg)



# Get the array with the median expression

medians <- sapply(1:num.arrays, 
       FUN=function(i) {
         return(median(exprs(raw.data[,i])))
       })
       
id.ref <- order(medians)[num.arrays/2]

# MA Plot

plotMA <- function (data, index) { 
	m <- exprs(data[,index]) - exprs(data[,id.ref])
	a <- (exprs(data[,index]) + exprs(data[,id.ref]))/2
	ma.plot(a,m,cex=0.75,lwd=3)
}

sapply(1:num.arrays, 
       FUN = function(i) {
         png(file=paste("ma_pre",i,".png",sep=""))
         plotMA(raw.data, i)
         dev.off()
       })


# QC

mas5.data <- call.exprs(raw.data,"mas5")
qcs <- qc(raw.data,mas5.data)
plot(qcs)


# Normalization
rma.data <- call.exprs(raw.data,"rma")

  
# Boxplot and distribution after normalization
boxplot(exprs(rma.data), col=raw.data@phenoData@data$Type)

plot(density(exprs(rma.data[,1]),col=raw.data@phenoData@data$Type[1]))
sapply(2:num.arrays, 
       FUN=function(i) {
         lines(density(exprs(rma.data[,i])), col=raw.data@phenoData@data$Type[i])
       })

# MA Plot

sapply(1:num.arrays, 
       FUN = function(i) {
         png(file=paste("ma_post",i,".png",sep=""))
         plotMA(rma.data, i)
         dev.off()
       })

