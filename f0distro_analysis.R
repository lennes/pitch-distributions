# R
#
# The data analysis described in the article:
# Mietta Lennes, Melisa Stevanovic, Daniel Aalto & Pertti Palo (2015).
# Comparing pitch distributions in Praat and R. The Phonetician.
# 
# You are free to use and modify this R code. However, we (the authors) do not guarantee that this code 
# will work in your computer, and we do not take any responsibility of any harm that the script might cause.
#
# You need to generate the data files first, e.g., with the Praat script called 
# collectPitchSamplesFromCorpus.praat.
# The analysis procedure is described in the above mentioned article.
#
#
# 25.10.2015
#------------
#

# First change your working directory to where this R script is located 
# (uncomment the following line and edit the directory path):
#setwd("/Users/lennes/projektit/f0distro/ThePhonetician2015_paper")

# If required, you can delete the old data objects by giving the following (uncommented) R commands:
#rm(pitch)
#rm(pitch.all)
#rm(pitch.lim)

#----  READING IN THE DATA FILES
#
# Data file produced by applying the speaker-specific pitch parameters, including annotated utterances only:
pitch <- read.table("data/pitch_utterances_indi.txt", header=TRUE, sep="\t", na.strings="NA", comment.char="")
nrow(pitch)
pitch[1:4,]
write.table(data.frame(table(pitch$Speaker)),file="data/n_table.txt",sep="\t",dec=",",quote=FALSE,row.names=FALSE)
speakers <- as.character(sort(unique(pitch$Speaker)))
#
# Data file produced by applying the default parameters, including annotated utterances only:
pitch.utt.all <- read.table("data/pitch_utterances_all.txt", header=TRUE, sep="\t", na.strings="NA", comment.char="")
nrow(pitch.utt.all)
pitch.utt.all[1:4,]
summary(pitch.utt.all)
#
# Data file produced by applying the speaker-specific pitch parameters to unannotated data:
pitch.lim <- read.table("data/pitch_indi.txt", header=TRUE, sep="\t", na.strings="NA", comment.char="")
nrow(pitch.lim)	
pitch.lim[1:4,]
summary(pitch.lim)
#
# Data file produced from raw, unannotated data with no speaker-specific parameters:
pitch.all <- read.table("data/pitch_all.txt", header=TRUE, sep="\t", na.strings="NA", comment.char="")
nrow(pitch.all)
summary(pitch.all)
pitch.all[1:4,]
#
#
# We may read in the original parameter file as well, in case the information turns out to be useful:
parms <- read.table("corpus/parms.txt", header=TRUE, sep="\t",na.strings="NA",comment.char="",row.names=1)


#----  ADDING USEFUL COLUMNS FOR FURTHER ANALYSIS
#
# The first character in the Conversation column indicates corpus A or B:
pitch$Set <- substr(pitch$Conversation,1,1)
pitch.lim$Set <- substr(pitch.lim$Conversation,1,1)
pitch.all$Set <- substr(pitch.all$Conversation,1,1)


#----  PITCH SCALES
# Define a couple of functions for converting pitch values between Hertz and semitone scale
# (using the same mathematical functions as in Praat)
#
# Semitones with reference to 100 Hz, as in Praat:
#
hertzToSemitones <- function(hertz) {
	result <- 12 * log(hertz/100) / log(2)
	result
	}
#	
semitonesToHertz <- function(semitones) {
	result <- 100 * exp (semitones*log(2)/12)
	result
	}

# Retrieve and store the list of all different speakers (in this case, only the file pitch_utterances.txt 
# contains samples from all speakers):
speakers <- as.character(sort(unique(pitch$Speaker)))
speakers.lim <- as.character(sort(unique(pitch.lim$Speaker)))
speakers.all <- as.character(sort(unique(pitch.all$Speaker)))
speakers

#----  ESTIMATING SPEAKER-SPECIFIC PITCH MODES
#
# Calculate the individual pitch modes by locating the highest peak in the probability density functions:
stats = data.frame(cbind(speakers,0,0))
rownames(stats) = stats[,1]
colnames(stats) = c("Speaker","ModeST","ModeHz")
stats$ModeST = NA
stats$ModeHz = NA
pitch.all$PitchMode <- NA
pitch.lim$PitchMode <- NA
pitch$PitchMode <- NA
for (spk in 1:length(speakers)){
	speaker = speakers[spk]
	data = subset(pitch,pitch$Speaker==speaker)
	# Calculate mode according to semitone scale:
	denspitch <- density(data$PitchST,na.rm=TRUE)
	modeST <- denspitch$x[which(denspitch$y==max(denspitch$y))]
	stats$ModeST[(stats$Speaker==speaker)] <- modeST
	# Calculate mode according to Hertz scale:
	denspitch <- density(data$PitchHz,na.rm=TRUE)
	modeHz <- denspitch$x[which(denspitch$y==max(denspitch$y))]
	stats$ModeHz[(stats$Speaker==speaker)] <- modeHz
	# Copy modes to the original data tables:
	pitch$PitchMode[pitch$Speaker==speaker] <- modeST
	pitch.lim$PitchMode[pitch.lim$Speaker==speaker] <- modeST
	pitch.all$PitchMode[pitch.all$Speaker==speaker] <- modeST
	# Copy total mean to the data table for reference:
	meanpitch = mean(data$PitchST,na.rm=TRUE)
	pitch$PitchMean[pitch$Speaker==speaker] <- meanpitch
}
# Calculate the relative pitch with respect to the speaker-specific pitch mode:
pitch.all$PitchSTreMode <- pitch.all$PitchST - pitch.all$PitchMode
pitch.lim$PitchSTreMode <- pitch.lim$PitchST - pitch.lim$PitchMode
pitch$PitchSTreMode <- pitch$PitchST - pitch$PitchMode
# And relative to speaker-specific mean:
pitch$PitchSTreMean <- pitch$PitchST - pitch$PitchMean

#------------
# Summary tables with basic statistics:
stats <- cbind(stats,tapply(pitch$PitchHz,pitch$Speaker,mean,na.rm=TRUE))
stats <- cbind(stats,tapply(pitch$PitchHz,pitch$Speaker,median,na.rm=TRUE))
stats <- cbind(stats,tapply(pitch$PitchHz,pitch$Speaker,sd,na.rm=TRUE))
stats <- cbind(stats,tapply(pitch$PitchST,pitch$Speaker,mean,na.rm=TRUE))
stats <- cbind(stats,tapply(pitch$PitchST,pitch$Speaker,median,na.rm=TRUE))
stats <- cbind(stats,tapply(pitch$PitchST,pitch$Speaker,sd,na.rm=TRUE))
colnames(stats) = c("Speaker","ModeST","ModeHz","MeanHz","MedianHz","StdevHz","MeanST","MedianST","StdevST")
# Reorder the table columns:
stats <- stats[,c(1:3,8,5,7,4,9,6)]
write.table(round(stats[,c(2:ncol(stats))],2),file="data/stats_table.txt",sep="\t",dec=",",quote=FALSE,row.names=TRUE)


# Create density plots for individual speakers:
# (Figures 1 and 2 in Lennes et al.)
dens = density(pitch$PitchST,na.rm=TRUE)
f.Hz <- pretty(range(semitonesToHertz(pitch$PitchST),na.rm=TRUE))
for (spk in 1:length(speakers)){
	speaker = speakers[spk]
	data1 = subset(pitch.all,pitch.all$Speaker==speaker)
	data2 = subset(pitch.lim,pitch.lim$Speaker==speaker)
	data3 = subset(pitch,pitch$Speaker==speaker)
	#filename = paste("fig/density_utt_vs_all_",speaker,".png",sep="")
	filename = paste("fig/density_utt_vs_all_",speaker,".eps",sep="")
	#png(filename, width=600, height=400, units="px", bg="white", pointsize=14)
	#cairo_ps(filename, width=6, height=4, bg="white", pointsize=12)
	postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", width=6, height=4, pointsize=10,colormodel="cmyk")
	par(mar=c(4,5,3,1))
	par(mfrow=c(1,1))
	plot(dens,ylim=c(0,0.3),xlim=c(-12,30),main="",xlab="",lwd=2, lty=3,type="n")
	if (nrow(data1[data1$PitchST!=NA,])>5) {dens1 = density(data1$PitchST,na.rm=TRUE)
		lines(dens1,lwd=2, lty=3)}
	if (nrow(data2[data2$PitchST!=NA,])>5) {dens2 = density(data2$PitchST,na.rm=TRUE)
		lines(dens2,lwd=2,lty=2)}
	if (nrow(data3[data3$PitchST!=NA,])>5) {dens3 = density(data3$PitchST,na.rm=TRUE)
		lines(dens3,lwd=2)}
	meanpoint = mean(data3$PitchST,na.rm=TRUE)
	medianpoint = median(data3$PitchST,na.rm=TRUE)
	modepoint <- dens3$x[which(dens3$y==max(dens3$y))]
	abline(v=meanpoint,col="red")
	abline(v=medianpoint,col="blue")
	abline(v=modepoint,col="green")
	axis(side=3,at=hertzToSemitones(f.Hz),lab=f.Hz,srt=90)
	abline(v=hertzToSemitones(c(50,100,150,200,250,300,350,400,450,500)),lty=3, col="grey")
	abline(h=c(0,0.05,0.1,0.15,0.2,0.25),lty=3, col="grey")
	mtext("Hz",side=3,line=2,adj=1,cex=0.9)
	mtext("semitones re 100 Hz",side=1,line=2,adj=1,cex=0.9)
	graphics.off()
	meanpoint
	semitonesToHertz(meanpoint)
	medianpoint 
	semitonesToHertz(medianpoint)
	modepoint
	semitonesToHertz(modepoint)
}


# Compare the individual pitch distributions as boxplots:
par(mfrow=c(2,1))
boxplot(PitchSTreMode~Speaker,data=pitch[,],pch="*",cex=0.7,xlab="Speaker",ylab="Pitch (ST re mode)")
boxplot(PitchSTreMode~Speaker,data=pitch.all,pch="*",cex=0.7,xlab="Speaker",ylab="Pitch (ST re mode)")
grid()


# Look more closely at the distribution of one speaker (default: speaker "F3")
speaker="F3"
# Plot absolute pitch distribution in Hertz scale:
filename = paste("fig/density_utt_Hz_",speaker,".png",sep="")
png(filename, width=600, height=400, units="px", bg="white", pointsize=14)
#filename = paste("fig/density_utt_Hz_",speaker,".eps",sep="")
#postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", width=6, height=4, pointsize=10,colormodel="cmyk")
par(mar=c(3,4,3,1))
par(mfrow=c(1,1))
f.Hz <- pretty(range(pitch$PitchHz),na.rm=TRUE)
	data = subset(pitch,pitch$Speaker==speaker)
	dens = density(data$PitchHz,na.rm=TRUE)
	colour = "black"
	plot(dens,main="",xlab="",col=colour,lwd=2)
axis(side=3,at=hertzToSemitones(f.Hz),lab=f.Hz,srt=90)
mtext("semitones re 100 Hz",side=3,line=2,adj=1,cex=0.9)
mtext("Hz",side=1,line=2,adj=1,cex=0.9)
grid()
graphics.off()
#
# Check the skewness of the distribution of a particular speaker:
# The moments package needs to be installed and loaded in order to use the skewness command.
library(moments)
speaker = "F3"
data = subset(pitch,pitch$Speaker==speaker)
skewness(data$PitchSTreMode,na.rm=TRUE)

# Plot pitch distributions for all speakers (separating male and female speakers with line colour):
# (Figure 3 in Lennes et al.)
#png("fig/density_utt_all.png", width=600, height=400, units="px", bg="white", pointsize=14)
postscript("fig/density_utt_all.eps", horizontal=FALSE, onefile=FALSE, paper="special", width=6, height=4, pointsize=10, colormodel="cmyk")
par(mar=c(3,4,3,1))
par(mfrow=c(1,1))
f.Hz <- pretty(range(semitonesToHertz(pitch$PitchST),na.rm=TRUE))
for (spk in 1:length(speakers)){
	speaker = speakers[spk]
	print(speaker)
	data = subset(pitch,pitch$Speaker==speaker)
	dens = density(data$PitchST,na.rm=TRUE)
	if (substr(speaker,1,1)=="F") colour = "red" else colour = "blue"
	if (spk == 1) plot(dens,ylim=c(0,0.3),xlim=c(-15,30),main="",xlab="",col=colour,lwd=2) else lines(dens,col=colour,lwd=2)
}
axis(side=3,at=hertzToSemitones(f.Hz),lab=f.Hz,srt=90)
mtext("Hz",side=3,line=2,adj=1,cex=0.9)
mtext("semitones re 100 Hz",side=1,line=2,adj=1,cex=0.9)
abline(v=hertzToSemitones(c(50,100,150,200,250,300,350,400,450,500)),col="grey",lty=3)
graphics.off()

# Plot pitch distributions for all speakers (separating male and female speakers with line colour),
# using the mode-referred pitch values from the utterance dataset:
# (Figure 4 in Lennes et al.)
#png("fig/density_utt_all_re_mode.png", width=600, height=400, units="px", bg="white", pointsize=14)
postscript("fig/density_utt_all_re_mode.eps", horizontal=FALSE, onefile=FALSE, paper="special", width=6, height=4, pointsize=10, colormodel="cmyk")
par(mar=c(3,4,3,1))
par(mfrow=c(1,1))
for (spk in 1:length(speakers)){
	speaker = speakers[spk]
	data = subset(pitch,pitch$Speaker==speaker)
	dens = density(data$PitchSTreMode,na.rm=TRUE)
	#colour = "black"
	if (substr(speaker,1,1)=="F") colour = "red" else colour = "blue"
	if (spk == 1) plot(dens,ylim=c(0,0.3),xlim=c(-8,18),main="",xlab="",col=colour,lwd=2) else lines(dens,col=colour,lwd=2)
}
mtext("semitones re mode",side=1,line=2,adj=1,cex=0.9)
abline(v=c(-5,0,5,10,15),col="grey",lty=3)
graphics.off()


#---------- T-TEST
# Are the mode-referred pitch distributions significantly different from a normal distribution 
# with mu = 0? (this should not be very surprising…)
for (spk in 1:length(speakers)){
  speaker = speakers[spk]
  data = subset(pitch,pitch$Speaker==speaker)
  print(speaker)
  print(t.test(data$PitchSTreMode,mu=0))
}

# Draw a histogram of all mode-referred pitch values from all speakers:
# (Figure 5 in Lennes et al.)
dens = density(pitch$PitchSTreMode,na.rm=TRUE)
par(mfrow=c(1,1))
h = hist(pitch$PitchSTreMode,breaks=c(-12:25)-0.5)
# Show the values inside the histogram object:
h(plot=FALSE)
# Make a table of the probabilities in the histogram:
pitch.prob.tab <- data.frame(cbind(h$mids,h$density,h$counts))
colnames(pitch.prob.tab) <- c("mid","density","count")
# Run the following line in order to see the frequencies for each bin in the histogram:
pitch.prob.tab
# You can get the probability of a given bin in the histogram by changing the following line:
h$density[h$mids==0]
# (The line above provides the probability of the mode bin (relative pitch = 0).
# Plot the overall pitch distribution as histogram + density plot:
# Plot the histogram
#filename = paste("fig/histogram_all.png",sep="")
filename = paste("fig/histogram_all.eps",sep="")
postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", width=6, height=4, pointsize=10,colormodel="cmyk")
#png(filename, width=600, height=400, units="px", bg="white", pointsize=14)
par(mar=c(4,5,1,1))
plot(h, freq=FALSE, main="", xlim=c(-10,20), ylim=c(0,0.18), xlab="")
# Write the total number of samples to the top right corner of the figure:
#mtext(paste("N =",nrow(subset(pitch,is.na(pitch$PitchSTreMode)==FALSE))),side=3,line=-2,adj=1,cex=0.9)
# Add the density curve:
lines(dens,lwd=2,lty=1)
mtext("semitones re speaker-specific mode",side=1,line=2,adj=1,cex=0.9)
graphics.off()

# # Colour the mode bin: 
# filename = paste("fig/histogram_all_modemark.png",sep="")
# png(filename, width=600, height=500, units="px", bg="white", pointsize=14)
# plot(h, freq=FALSE, main="Total pitch distribution, 40 speakers", xlim=c(-10,20),ylim=c(0,0.18), ylab="Probability", xlab="Relative pitch (semitones from speaker-specific mode)",col=c(rep("white",12),rep("blue",1),rep("white",20)))
# # Write the total number of samples to the top right corner of the figure:
# mtext(paste("N =",nrow(subset(pitch,is.na(pitch$PitchSTreMode)==FALSE))),side=3,line=-2,adj=1,cex=0.9)
# lines(dens,lwd=2,lty=1)
# text(0,0.18,labels=paste(round(100 * (h$density[h$mids==0]))," %",sep=""),cex=2)
# graphics.off()
# # Colour the 7 bins of the highest probabilities: 
# filename = paste("fig/histogram_all_7binmark.png",sep="")
# png(filename, width=600, height=500, units="px", bg="white", pointsize=14)
# plot(h, freq=FALSE, main="Total pitch distribution, 40 speakers", xlim=c(-10,20),, ylim=c(0,0.18), ylab="Probability", xlab="Relative pitch (semitones from speaker-specific mode)",col=c(rep("white",10),rep("blue",7),rep("white",20)))
# # Write the total number of samples to the top right corner of the figure:
# mtext(paste("N =",nrow(subset(pitch,is.na(pitch$PitchSTreMode)==FALSE))),side=3,line=-2,adj=1,cex=0.9)
# lines(dens,lwd=2,lty=1)
# text(8,0.12,labels=paste("~",round(100 * (sum(h$density[h$mid==c(0)],h$density[h$mid==c(1)],h$density[h$mid==c(-1)],h$density[h$mid==c(2)],h$density[h$mid==c(-2)],h$density[h$mid==c(3)],h$density[h$mid==c(4)])))," %",sep=""),cex=2)
# sum(h$density[h$mid==c(0)],h$density[h$mid==c(1)],h$density[h$mid==c(-1)],h$density[h$mid==c(2)],h$density[h$mid==c(-2)],h$density[h$mid==c(3)],h$density[h$mid==c(4)])
# graphics.off()
#
# # Colour the 13 bins of the highest probabilities: 
# filename = paste("fig/histogram_all_13binmark.png",sep="")
# png(filename, width=600, height=500, units="px", bg="white", pointsize=14)
# plot(h, freq=FALSE, main="Total pitch distribution, 40 speakers", xlim=c(-10,20),, ylim=c(0,0.18), ylab="Probability", xlab="Relative pitch (semitones from speaker-specific mode)",col=c(rep("white",8),rep("blue",13),rep("white",20)))
# # Write the total number of samples to the top right corner of the figure:
# mtext(paste("N =",nrow(subset(pitch,is.na(pitch$PitchSTreMode)==FALSE))),side=3,line=-2,adj=1,cex=0.9)
# lines(dens,lwd=2,lty=1)
# text(8,0.12,labels=paste("~",round(100 * (sum(h$density[h$mid==c(0)],h$density[h$mid==c(1)],h$density[h$mid==c(-1)],h$density[h$mid==c(2)],h$density[h$mid==c(-2)],h$density[h$mid==c(3)],h$density[h$mid==c(4)],h$density[h$mid==c(-3)],h$density[h$mid==c(5)],h$density[h$mid==c(6)],h$density[h$mid==c(-4)],h$density[h$mid==c(7)],h$density[h$mid==c(8)])))," %",sep=""),cex=2)
# text(10,0.09,labels=paste("-4 ST < mode < +8 ST"),cex=1.2)
# graphics.off()


#----- BOOTSTRAPPING THE SUMMARY STATISTICS
#
# NB: Each of the following bootstrapping processes may take a long time! 
# Please prepare to wait for half an hour or so, depending on your machine.
#
# Parameters for bootstrapping the mode and the mean:
# Maximum number of consecutive values in a draw:
ntest = 10000
# The increment in sample size between rounds:
step = 50
# Calculate how many rounds of draws will be required:
steps = ntest / step
# In each round, how many draws should be performed for each speaker?
max.draws = 5
# In order to repeat a draw for a speaker, there should be at least 
# 1.5 times nstep values left in the subset of data.
min.datasize = 1.5

#
#-----  Bootstrap the mean and mode for all speakers and save the sampled values to a table
# (required in order to create figures 6 and 7 in Lennes et al.)
# Create empty data frames for the sampled means and modes:
bootstrap.mean <- data.frame(cbind(rep(NA,steps),rep(NA,steps)))
bootstrap.mode <- data.frame(cbind(rep(NA,steps),rep(NA,steps)))
# Bootstrap for one speaker at a time:
for (spk in 1:length(speakers)){
# Alternatively, you might first run this only for a few speakers in order to test:
#for (spk in 1:3){
	speaker = speakers[spk]
	# This is the first column for this speaker in the bootstrap table (each speaker is allocated 'max.draws' columns):
	spk.col = 1 + (spk-1) * max.draws
	bootstrap.mode[,c(spk.col:(spk.col+4))] = NA
	bootstrap.mean[,c(spk.col:(spk.col+4))] = NA
	data = subset(pitch,pitch$Speaker==speaker)
	totalmean= mean(data$PitchST,na.rm=TRUE)
	# Draw from 5 to ntest consecutive samples randomly, 5 draws per speaker.
	# Calculate and plot the summary statistics in each draw:
	for (test in 1:steps) {
		n = test * step
		lowerlimit = n * min.datasize
		data = subset(pitch,pitch$Speaker==speaker)
		col = spk.col
		if (nrow(data) > lowerlimit) { 
			for (draw in 1:5) {
				if (nrow(data) > lowerlimit) {
				  test.start = sample(c(1:(nrow(data) - n)),1)
				  test.end = test.start + n
				  data.test = data[c(test.start:test.end),]
				  # Make sure that the samples included in this draw are excluded from the next draw:
				  data = rbind(data[c(1:(test.start-1)),], data[c((test.end+1):nrow(data)),])
				  mode = NA
				  mean = NA
				  # Here, at least five defined pitch values are required in order to calculate the mode:
				  if (nrow(data.test[is.na(data.test$PitchSTreMode)==FALSE,]) > 5) {
				  	dens = density(data.test$PitchSTreMode,na.rm=TRUE)
				  	mode <- dens$x[which(dens$y==max(dens$y))]
				  	mean = mean(data.test$PitchST,na.rm=TRUE) - totalmean
				  }
				  # In case there are more than 5 values in this sample, store the mode and mean to dataframes:
				  if (n > 5) {
				    bootstrap.mode[test,col] = mode
				    bootstrap.mean[test,col] = mean
				    }
				  }
				  col = col + 1
				} 
			}
	}
}
# Calculate mean and standard deviation at each sample size (adding columns):
bootstrap.mean <- transform(bootstrap.mean, MEAN=apply(bootstrap.mean,1, mean, na.rm = TRUE))
bootstrap.mean <- transform(bootstrap.mean, SD=apply(bootstrap.mean,1, sd, na.rm = TRUE))
bootstrap.mode <- transform(bootstrap.mode, MEAN=apply(bootstrap.mode,1, mean, na.rm = TRUE))
bootstrap.mode <- transform(bootstrap.mode, SD=apply(bootstrap.mode,1, sd, na.rm = TRUE))
rownames(bootstrap.mean) <- c(1:steps*step)
rownames(bootstrap.mode) <- c(1:steps*step)
# Get example means at 1000, 3000 and 6000 samples (reported in Lennes et al.)
bootstrap.mean[c("1000","3000","6000"),c("MEAN","SD")]
bootstrap.mode[c("1000","3000","6000"),c("MEAN","SD")]
# Save the bootstrapped tables (otherwise it would be impossible to collect the exact same values again, 
# since they were drawn at random!)
write.table(bootstrap.mean, file = "data/bootstrap.mean.csv")
write.table(bootstrap.mode, file = "data/bootstrap.mode.csv")
#
# Plot the bootstrapped results for mean:
# (figure 6 in Lennes et al.)
#filename = paste("fig/bootstrap_mean_all.png",sep="")
#png(filename, width=600, height=400, units="px", bg="white", pointsize=14)
filename = paste("fig/bootstrap_mean_all.eps",sep="")
postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", width=6, height=4, pointsize=10, colormodel="cmyk")
par(mar=c(4,4,3,1))
par(mfrow=c(1,1))
plot(c(0:ntest),ylim=c(-5,8),ylab="ST re total mean", xlab="Number of consecutive pitch samples in one draw", type="n")
abline(h=0)
grid()
axis(side=3,at=c(1500,3000,4500,6000,7500,9000),lab=c("30 sec","1 min","1,5 min","2 min","2,5 min","3 min"),srt=90)
for (spk in 1:length(speakers)){
  print(paste ("Now plotting speaker:",speakers[spk]))
  spk.col = 1 + (spk-1) * max.draws
  for (draw in 1:max.draws){
    points(c(1:nrow(bootstrap.mean)*50), bootstrap.mean[,spk.col],pch="o",cex=0.5,col="grey")
    spk.col = spk.col + 1
  }
}
lines(c(1:nrow(bootstrap.mean)*50),bootstrap.mean$MEAN,lwd = 2)
lines(c(1:nrow(bootstrap.mean)*50),bootstrap.mean$MEAN+bootstrap.mean$SD,lwd = 1)
lines(c(1:nrow(bootstrap.mean)*50),bootstrap.mean$MEAN-bootstrap.mean$SD,lwd = 1)
graphics.off()
#------
# Plot the bootstrapped results for mode:
# (figure 7 in Lennes et al.)
#filename = paste("fig/bootstrap_mode_all.png",sep="")
#png(filename, width=600, height=400, units="px", bg="white", pointsize=14)
filename = paste("fig/bootstrap_mode_all.eps",sep="")
postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", width=6, height=4, pointsize=10, colormodel="cmyk")
par(mar=c(4,4,3,1))
par(mfrow=c(1,1))
plot(c(0:ntest),ylim=c(-5,8),ylab="ST re total mode", xlab="Number of consecutive pitch samples in one draw", type="n")
abline(h=0)
grid()
axis(side=3,at=c(1500,3000,4500,6000,7500,9000),lab=c("30 sec","1 min","1,5 min","2 min","2,5 min","3 min"),srt=90)
for (spk in 1:length(speakers)){
  print(paste ("Now plotting speaker:",speakers[spk]))
  spk.col = 1 + (spk-1) * max.draws
  for (draw in 1:max.draws){
    points(c(1:nrow(bootstrap.mode)*50), bootstrap.mode[,spk.col],pch="o",cex=0.5,col="grey")
    spk.col = spk.col + 1
    }
  }
lines(c(1:nrow(bootstrap.mode)*50),bootstrap.mode$MEAN,lwd = 2)
lines(c(1:nrow(bootstrap.mode)*50),bootstrap.mode$MEAN+bootstrap.mode$SD,lwd = 1)
lines(c(1:nrow(bootstrap.mode)*50),bootstrap.mode$MEAN-bootstrap.mode$SD,lwd = 1)
graphics.off()
#

#-----  Plot exemplary density curves of MEAN-referred pitch at specific 
#       random sample sizes, only one draw per speaker at each :
# (figure 8 in Lennes et al.)
#step = 50, as in the previous figures!
#filename = paste("fig/bootstrap_dens_mean_n1000-6000_all.png",sep="")
#png(filename, width=800, height=350, units="px", bg="white", pointsize=16)
filename = paste("fig/bootstrap_dens_mean_n1000-6000_all.eps",sep="")
postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", width=8, height=3.5, pointsize=10, colormodel="cmyk")
par(mfrow=c(1,3))
par(mar=c(4,4,1,1))
# For the article, we plot figures at 
# test=20 -> 1000 samples
# test=60 -> 3000 samples
# test=120 -> 6000 samples
for (test in c(20,60,120)){
  n = test * step
  dens = density(pitch$PitchSTreMean,na.rm=TRUE)
  if (test==20) plot(dens,xlim=c(-6,6),ylim=c(0,0.33), xlab="ST re total mean", main="", type="n",cex.axis=1.4,cex.lab=1.4) else plot(dens,xlim=c(-6,6),ylim=c(0,0.33), xlab="", ylab="", main="", type="n",cex.axis=1.4,cex.lab=1.4) 
  abline(v=0)
  mtext(paste(n,"samples  "),side=3,adj=1,cex=0.8, line=-1.5)
  grid()
  for (spk in 1:length(speakers)){
    speaker = speakers[spk]
    data = subset(pitch,pitch$Speaker==speaker)
    # Draw 'n' consecutive values from the dataset, once for each speaker, and plot the density:
    if (nrow(data) > n) {
      test.draw = sample(c(1:(nrow(data) - n)),1)
      test.start = test.draw
      test.end = test.start + n
      data.test = data[c(test.start:test.end),]
      # Actually, there is only one draw in this example, so it s not necessary to exclude this 
      # draw from the subset; but we will keep the following line just in case!
      data = rbind(data[c(1:(test.start-1)),], data[c((test.end+1):nrow(data)),])
      if (substr(speaker,1,1)=="F") colour = "red" else colour = "blue"
      if (nrow(data.test[is.na(data.test$PitchSTreMean)==FALSE,]) > 5) {
        dens = density(data.test$PitchSTreMean,na.rm=TRUE)
        lines(dens,col=colour)
      }
    }
  }
}
graphics.off()


#-----  Plot exemplary density curves of MODE-referred pitch at specific 
#       random sample sizes, only one draw per speaker at each :
# (figure 9 in Lennes et al.)
#step = 50, as in the previous figures!
#filename = paste("fig/bootstrap_dens_mode_n1000-6000_all.png",sep="")
#png(filename, width=800, height=350, units="px", bg="white", pointsize=16)
filename = paste("fig/bootstrap_dens_mode_n1000-6000_all.eps",sep="")
postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", width=8, height=3.5, pointsize=10, colormodel="cmyk")
par(mfrow=c(1,3))
par(mar=c(4,4,1,1))
# For the article, we plot figures at 
# test=20 -> 1000 samples
# test=60 -> 3000 samples
# test=120 -> 6000 samples
for (test in c(20,60,120)){
n = test * step
dens = density(pitch$PitchSTreMode,na.rm=TRUE)
if (test==20) plot(dens,xlim=c(-6,6),ylim=c(0,0.33), xlab="ST re total mode", main="", type="n",cex.axis=1.4,cex.lab=1.4) else plot(dens,xlim=c(-6,6),ylim=c(0,0.33), xlab="", ylab="", main="", type="n",cex.axis=1.4,cex.lab=1.4) 
abline(v=0)
mtext(paste(n,"samples  "),side=3,adj=1,cex=0.8, line=-1.5)
grid()
for (spk in 1:length(speakers)){
	speaker = speakers[spk]
	data = subset(pitch,pitch$Speaker==speaker)
	# Draw 'n' consecutive values from the dataset, once for each speaker, and plot the density:
	if (nrow(data) > n) {
		test.draw = sample(c(1:(nrow(data) - n)),1)
		test.start = test.draw
		test.end = test.start + n
		data.test = data[c(test.start:test.end),]
		# Actually, there is only one draw in this example, so it s not necessary to exclude this 
		# draw from the subset; but we will keep the following line just in case!
		data = rbind(data[c(1:(test.start-1)),], data[c((test.end+1):nrow(data)),])
		if (substr(speaker,1,1)=="F") colour = "red" else colour = "blue"
		if (nrow(data.test[is.na(data.test$PitchSTreMode)==FALSE,]) > 5) {
			dens = density(data.test$PitchSTreMode,na.rm=TRUE)
			lines(dens,col=colour)
			}

		}
	}
}
graphics.off()


# --------------------------------
# We can also try to bootstrap the mode, median, mean and standard deviation separately for each speaker while plotting.
# This may be used for visually inspecting whether the data of a particular speaker is more unreliable than that of the other speakers.
# (This is not directly used in the article by Lennes et al., but an individual analysis is quite necessary when
# exploring this kind of data.)
#
# Maximum number of pitch samples
ntest = 10000
# Increase the number of samples by the following step between consecutive draws:
step = 50
# Calculate the total number of steps required:
steps = ntest / step - 1
for (spk in 1:length(speakers)){
  # Alternatively, you might first run this only for a few speakers in order to test:
  #for (spk in 1:5){
  speaker = speakers[spk]
  filename = paste("fig/bootstrap_",speaker,".png",sep="")
  png(filename, width=600, height=400, units="px", bg="white", pointsize=14)
  #filename = paste("fig/bootstrap_",speaker,".eps",sep="")
  #postscript(filename, horizontal=FALSE, onefile=FALSE, paper="special", height=5, width=5, 	pointsize=12)
  par(mar=c(4,4,3,1))
  par(mfrow=c(1,1))
  plot(c(0:ntest),ylim=c(-5,5),ylab="ST re total mode", xlab="Number of consecutive pitch samples in one draw", type="n")
  abline(h=0)
  data = subset(pitch,pitch$Speaker==speaker)
  abline(h=mean(data$PitchSTreMode,na.rm=TRUE),col="red")
  abline(h=median(data$PitchSTreMode,na.rm=TRUE),col="blue")
  abline(h=sd(data$PitchSTreMode,na.rm=TRUE),col="grey")
  abline(h=(mean(data$PitchSTreMode,na.rm=TRUE)-sd(data$PitchSTreMode,na.rm=TRUE)),col="grey")
  axis(side=3,at=c(1500,3000,4500,6000,7500,9000),lab=c("30 sec","1 min","1,5 min","2 min","2,5 min","3 min"),srt=90)
  # Draw from 5 to ntest consecutive samples randomly, max 5 draws per speaker.
  # Calculate and plot the summary statistics in each draw:
  for (test in 1:steps) {
    n = test * step
    data = subset(pitch,pitch$Speaker==speaker)
    for (draw in 1:5) {
      if (nrow(data) > n) {
        test.start = sample(c(1:(nrow(data) - n)),1)
        test.end = test.start + n
        data.test = data[c(test.start:test.end),]
        # Make sure that the samples in this draw are excluded from the next draw:
        data = rbind(data[c(1:(test.start-1)),], data[c((test.end+1):nrow(data)),])
        mean = mean(data.test$PitchSTreMode,na.rm=TRUE)
        median = median(data.test$PitchSTreMode,na.rm=TRUE)
        stdev = sd(data.test$PitchSTreMode,na.rm=TRUE)
        neg.stdev = mean - stdev
        mode = NA
        # Here, at least five defined pitch values are required in order to calculate the mode:
        if (nrow(data.test[is.na(data.test$PitchSTreMode)==FALSE,]) > 5) {
          dens = density(data.test$PitchSTreMode,na.rm=TRUE)
          mode <- dens$x[which(dens$y==max(dens$y))]
        }
        if (n > 5) {
          points(n,mean,col="red",pch="o",cex=0.7)
          points(n,median,col="blue",pch="o",cex=0.7)
          points(n,stdev,col="grey",pch="o",cex=0.7)
          points(n,neg.stdev,col="grey",pch="o",cex=0.7)
          points(n,mode,col="green",pch="o",cex=0.7)
        }
      }
    }
  }
  graphics.off()
}

