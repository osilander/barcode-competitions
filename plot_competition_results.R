# should check for more edge cases
# e.g. if there are differences in the
# number of timepoints, replicates, etc.

library(Hmisc)
library(RColorBrewer)
library(here)
rm(list = ls())


### check that the figures driectory exists
if(!dir.exists(here("figures"))) {
	dir.create(here("figures"), FALSE)
}

## get the data on the barcodes
barcodes <- read.table(file=here("config","barcodes.txt"), header=T)
# have to set fwd and rev separately
barcodes.rev <- barcodes[,c(1,3)]
colnames(barcodes.rev) <- c("barcode","seq")
barcodes.fwd <- barcodes[,c(1,2)]
colnames(barcodes.fwd) <- c("barcode","seq")

comp.paths <- dir(here("data"), pattern="counts", full.names=T)
sample.layout <- read.table(file=here("config","sample_layout.txt"), header=T)
barcode.layout <- read.table(file=here("config","barcode_layout.txt"), header=T)

time.points <- unique(sample.layout$hour)
replicates <- unique(sample.layout$rep)
samples <- unique(sample.layout$sample)

# open pdf, set height and width based on the number of samples and replicates we have
pdf(file=here("figures","competition_plots.pdf"), height=4*length(samples), width=4*length(replicates))
par(mfrow=c(length(samples),length(replicates)))
par(las=1)
plot.cols <- brewer.pal(7,"Set2")

#loop over samples
for(s in 1:length(samples)) {
	# loop over replicates
	for(r in 1:length(replicates)) {
		# reset a bunch of stuff once we start to plot a new sample and replicate
		strain.dyn <- matrix()
		exp.layout <- barcode.layout[which(barcode.layout$sample==samples[s]),]
		exp.layout.rev <- merge(exp.layout,  barcodes.rev, by="barcode")
		exp.layout.fwd <- merge(exp.layout,  barcodes.fwd, by="barcode")
		## assumes timepoints are in order, but they should be
		for (t in 1:length(time.points)){

			# finds matching data file according to the sample layout file
			comp.file <- sample.layout[which((sample.layout$hour==time.points[t]) & 
				(sample.layout$rep==replicates[r]) &
				(sample.layout$sample==samples[s])),4]
			comp <- read.table(file=here("data", comp.file))
			# resets the column names for consistency
			colnames(comp) <- c("seq","count")
			# only at t=1 do we reset what is in strain.dyn
			if(t==1) {
				strain.dyn <- comp
			}
			else {
				strain.dyn <- merge(strain.dyn, comp, by="seq")
			}
		}
		
		# this is kludgy
		strain.counts.fwd <- merge(strain.dyn, exp.layout.fwd, by="seq")[,c("barcode","strain","count.x","count.y","count")]
		strain.counts.rev <- merge(strain.dyn, exp.layout.rev, by="seq")[,c("barcode","strain","count.x","count.y","count")]
		strain.counts <- merge(strain.counts.fwd, strain.counts.rev, by="strain")
		# this is extremely kludgy, not sure how to improve right now
		strain.counts[,3] <- strain.counts[,3]+strain.counts[,7]
		strain.counts[,4] <- strain.counts[,4]+strain.counts[,8]
		strain.counts[,5] <- strain.counts[,5]+strain.counts[,9]
		strain.counts <- strain.counts[,1:5]

		# just for neatness
		colnames(strain.counts) <- c("strain","barcode","t0","t1","t2")

		# normalise by col sums
		strain.fract <- sweep(strain.counts[,3:5], 2, colSums(strain.counts[,3:5]), FUN="/")
		strain.fract <- cbind(strain.counts[,1:2],strain.fract)
		# set up plot
		plot(-10,-10,xlim=c(0,50),ylim=c(0,1),xlab="Time (hours)", ylab="Fraction of reads",
			main=paste(samples[s], "repl.", replicates[r], sep=" "))

		# and plot the data
		for(i in 1:dim(strain.fract)[1]) {
			points(time.points, strain.fract[i,3:5], cex=1.3,ty="o",col=plot.cols[i], pch=19)
		}
		# stick in a legend
		legend(35,0.6,pch=19,col=plot.cols,legend=strain.fract$strain,bty="n")
		# add some useful text
		total.reads <- sum(strain.counts[,3:5])
		text(7,1,labels=paste("total reads: ",total.reads, sep=""))
		# kludgy needs to be fixed
		text(7,0.95,labels=paste("t0: ",sum(strain.counts[1:2,3]),sep=""))
		text(7,0.90,labels=paste("t1: ",sum(strain.counts[1:2,4]),sep=""))
		text(7,0.85,labels=paste("t2: ",sum(strain.counts[1:2,5]),sep=""))
	}
}
dev.off()


