#Benchmark GenActive vs FitbitHR
#===============================

genactive.data <- "/home/afolarinbrc/Dropbox/purple_robot/test_genwatch-v-fitbitHR/max_data/GeneActiv-FitbitBasis_24-03_to_26-03/GeneActiv_60epoch.csv"
fitbit.data <- "/home/afolarinbrc/Dropbox/purple_robot/test_genwatch-v-fitbitHR/max_data/GeneActiv-FitbitBasis_24-03_to_26-03/purple_robot_export_2015-03-24_2015-03-26.txt"
fitbit2.data <- "/home/afolarinbrc/Dropbox/purple_robot/test_genwatch-v-fitbitHR/max_data/GeneActiv-FitbitBasis_24-03_to_26-03/purple_robot_export_2015-03-24_2015-03-26_export-fb-only.txt"
basis.data <- "/home/afolarinbrc/Dropbox/purple_robot/test_genwatch-v-fitbitHR/max_data/GeneActiv-FitbitBasis_24-03_to_26-03/basis_bodymetrics_2015-03-19T00_00_00_2015-03-26T10_34_00.csv"


#load GenActive Data
#===================

ga<- read.csv(genactive.data, 
        col.names=c("datetime","u1","u2","u3","u4","u5","accel.x","accel.y","accel.z","light","button.pressed","temp"),
        skip=100, stringsAsFactors=F)

##NOTE: seems like the temp and accel.x are the wrong way round ?? hard to interpret without the column names... I'm guessing here.

#Data @ 10Hz, get the 1/6 average, i.e. downsample to 1/min
#require(zoo)
#ga.ts <- zoo(ga)

#get the date$time in format from prev script....lazy as I don't want to change var name later
ga.datetime <- ga$datetime

ga.accel.x <- ga$accel.x
ga.accel.y <- ga$accel.y
ga.accel.z <- ga$accel.z
ga.light <- ga$light
ga.temp <- ga$temp

#ga.accel.x <- rollapply(ga$accel.x, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
#ga.accel.y <- rollapply(ga$accel.y, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
#ga.accel.z <- rollapply(ga$accel.z, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
#ga.light <- rollapply(ga$light, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
#ga.temp <- rollapply(ga$temp, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)

#check plot & save
par.o <- par()
par(mfrow=c(5,1))
plot(ga.accel.x, xaxt="n")
plot(ga.accel.y, xaxt="n")
plot(ga.accel.z, xaxt="n")
plot(ga.light, xaxt="n")
plot(ga.temp, xaxt="n")



#Read Basis Monitor data
#=======================
bm <- read.csv(file=basis.data, header=T, stringsAsFactors=F)




#load the FitbitHR data
#======================

#fb.all <- read.table(fitbit.data, sep="\t", header=T, stringsAsFactors=F, quote='')
#max imported all probes together -- separate out the fitbit probe
#fb.match <- grep("FitbitProbe", fb.all$Probe)
#fb <- fb.all[fb.match, ]

#attempt 2 using fb probe export on its own
fb <- read.table(fitbit2.data, sep="\t", header=T, stringsAsFactors=F, quote='')


require(jsonlite)

#parse the Json column
fb.xt <- t(sapply(fb$Payload, fromJSON, simplifyVector=T))
fb.xt <- cbind(fb[, -4], fb.xt) 
rownames(fb.xt) <- 1:nrow(fb.xt) #remove large json str. rowname as obstructive to viewing
#add a column which is the converted timestamp values into datetime
fb.xt$"timestamp.datetime" <- as.character(as.POSIXct(unlist(fb.xt[, "TIMESTAMP"]), origin="1970-01-01"))
#add integrated active minutes modes
fb.xt$"SUMMED_ACTIVE_MINUTES" <- as.list(unlist(fb.xt[, "LIGHTLY_ACTIVE_MINUTES"]) + unlist(fb.xt[, "FAIRLY_ACTIVE_MINUTES"]) + unlist(fb.xt[, "VERY_ACTIVE_MINUTES"]))

#plot
plot(unlist(fb.xt[, "LIGHTLY_ACTIVE_MINUTES"]))
plot(unlist(fb.xt[, "FAIRLY_ACTIVE_MINUTES"]))
plot(unlist(fb.xt[, "VERY_ACTIVE_MINUTES"]))
plot(unlist(fb.xt[, "SLEEP_MEASUREMENTS"]))
plot(unlist(fb.xt[, "ACTIVE_SCORE"]))



# select best common start for both datasets
ga$datetime[1]
#[1] "2015-03-24 14:27:55:000"
ga$datetime[length(ga$datetime)]
#[1] "2015-03-26 11:01:55:000"


fb.xt$timestamp.datetime[24]
#[1] "2015-03-24 14:26:31"
fb.xt$timestamp.datetime[length(fb.xt$timestamp.datetime)]
#[1] "2015-03-26 23:56:47"
fb.xt$timestamp.datetime[568]
#[1] "2015-03-26 11:03:21"

bm$date[1]
#[1] "2015-03-23 00:00Z"
bm$date[length(bm$date)]
#[1] "2015-03-25 17:45Z"

ga.from <- 1
ga.to <- length(ga.datetime)
fb.from <- 24
fb.to <- 568
bm.from <- 2307
bm.to <- 3946


x11(width=19, height=10)
par(mfrow=c(4,1))
plot(ga.accel.z[ga.from:ga.to], xaxt="n", main="Geneactive (AF)")
plot(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Lightly Active Mins (AF)")
plot(unlist(fb.xt[from:to, "FAIRLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Fairly Active Mins (AF)")
plot(unlist(fb.xt[from:to, "VERY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Very Active Mins (AF)", xlab="Time/mins")
savePlot(filename="dev-tests-ga+fb-MK.jpg")

x11(width=19, height=10)
par(mfrow=c(4,1))
plot(ga.accel.z[ga.from:ga.to], xaxt="n", main="Geneactive (AF)")
plot(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Lightly Active Mins (AF)")
plot(unlist(fb.xt[from:to, "SEDENTARY_RATIO"]), xaxt="n", main="Fitbit Sedentary Ratio (AF)")
plot(unlist(fb.xt[from:to, "SLEEP_MEASUREMENTS"]), xaxt="n", main="Fitbit Sleep Auto Marking (AF)", xlab="Time/mins")
savePlot(filename="dev-tests-ga+fb+sleep-calls-MK.jpg")

##try plotting as timeseries
#plot(as.POSIXlt(ga$datetime[ga.from:ga.to]), ga.accel.z[ga.from:ga.to], xaxt="n", main="Geneactive (AF)", )
#plot(as.POSIXlt(unlist(fb.xt[fb.from:fb.to, "timestamp.datetime"])), unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Lightly Active Mins (AF)")
require("xts")
ga.ts <- xts(ga$accel.z[ga.from:ga.to], order.by=as.POSIXct(ga$datetime[ga.from:ga.to]))
fb.ts <- xts(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), order.by=as.POSIXct(unlist(fb.xt[fb.from:fb.to, "timestamp.datetime"])))
fb.ts.diff <- xts(diff(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"])), order.by=as.POSIXct(unlist(fb.xt[fb.from:(fb.to-1), "timestamp.datetime"])))  #strictly speaking you should take the average time between each diff rather than the value of the first of the two timepoints... 
bm.ts.hr <- xts(bm$heart.rate[bm.from:bm.to], order.by=as.POSIXct(bm$date[bm.from:bm.to]))
bm.ts.steps <- xts(bm$steps[bm.from:bm.to], order.by=as.POSIXct(bm$date[bm.from:bm.to]))
bm.ts.skin.temp <- xts(bm$skin.temp[bm.from:bm.to], order.by=as.POSIXct(bm$date[bm.from:bm.to]))
bm.ts.gsr <- xts(bm$gsr[bm.from:bm.to], order.by=as.POSIXct(bm$date[bm.from:bm.to]))

x11(width=19, height=10)
par(mfrow=c(3,1))
plot.xts(ga.ts)
plot.xts(fb.ts)
plot.xts(bm.ts.steps)
#plot.xts(fb.ts.diff)
savePlot(filename="dev-tests-ga+fb-as-timeseries-MK.jpg")

x11(width=19, height=10)
par(mfrow=c(4,1))
plot.xts(bm.ts.steps)
plot.xts(bm.ts.hr)
plot.xts(bm.ts.skin.temp)
plot.xts(bm.ts.gsr)


#plot the diff 
#par(mfrow=c(5,1))
#plot(ga.accel.z[from:length(ga.accel.z)], xaxt="n")
#plot(diff(unlist(fb.xt[, "LIGHTLY_ACTIVE_MINUTES"])), log="y") #the diff
#plot(unlist(fb.xt[from:to, "LIGHTLY_ACTIVE_RATIO"]), xaxt="n")
#plot(unlist(fb.xt[from:to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n")
#plot(unlist(fb.xt[from:to, "SUMMED_ACTIVE_MINUTES"]), xaxt="n")


#todo 


