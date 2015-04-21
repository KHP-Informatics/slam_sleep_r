#Benchmark GenActive vs FitbitHR
#===============================


#load GenActive Data
#===================

ga<- read.csv("~/Dropbox/purple_robot/test_genwatch-v-fitbitHR/genwatch/2015-03-20_16-09.csv", 
        col.names=c("datetime","u1","u2","u3","u4","u5","accel.x","accel.y","accel.z","light","button.pressed","temp"),
        skip=100, stringsAsFactors=F)

##NOTE: seems like the temp and accel.x are the wrong way round ?? hard to interpret without the column names... I'm guessing here.

#Data @ 10Hz, get the 1/6 average, i.e. downsample to 1/min
require(zoo)
#ga.ts <- zoo(ga)

#downsample resolution with a rolling window mean
ga.datetime <- rollapply(as.vector(ga$datetime), width=6, by=3, FUN="[", align="center", fill=NA, na.rm=T)[, 1]

ga.accel.x <- rollapply(ga$accel.x, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
ga.accel.y <- rollapply(ga$accel.y, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
ga.accel.z <- rollapply(ga$accel.z, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
ga.light <- rollapply(ga$light, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)
ga.temp <- rollapply(ga$temp, width=6, by=3, FUN=mean, align="center", fill=NA, na.rm=T)

#check plot & save
par.o <- par()
par(mfrow=c(5,1))
plot(ga.accel.x, xaxt="n")
plot(ga.accel.y, xaxt="n")
plot(ga.accel.z, xaxt="n")
plot(ga.light, xaxt="n")
plot(ga.temp, xaxt="n")




#load the FitbitHR data
#======================

fb <- read.table("~/Dropbox/purple_robot/test_genwatch-v-fitbitHR/export_2015-03-17_2015-03-20_uxu7zx8gg3iq4oob.txt", sep="\t", header=T, stringsAsFactors=F, quote='')

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
ga.datetime[3]
ga.datetime[length(ga.datetime)-4]

fb.xt[1, "timestamp.datetime"]
fb.xt[nrow(fb.xt), "timestamp.datetime"]

#> # start and endtimes for both datasets
#> ga.datetime[3]
#[1] "2015-03-17 13:57:01:500"
#> ga.datetime[length(ga.datetime)-4]
#[1] "2015-03-20 15:56:31:500"
#> 
#> fb.xt[1, "timestamp.datetime"]
#[1] "2015-03-18 17:44:49"
#> fb.xt[nrow(fb.xt), "timestamp.datetime"]
#[1] "2015-03-20 15:53:07"
#> 
#> View(ga.datetime)
#> ga.datetime[10008]
#[1] "2015-03-18 17:44:31:500"

ga.from <- 10008
ga.to <- length(ga.datetime)-4
fb.from <- 1
fb.to <- nrow(fb.xt)

x11(width=19, height=10)
par(mfrow=c(4,1))
plot(ga.accel.z[ga.from:ga.to], xaxt="n", main="Geneactive (AF)")
plot(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Lightly Active Mins (AF)")
plot(unlist(fb.xt[from:to, "FAIRLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Fairly Active Mins (AF)")
plot(unlist(fb.xt[from:to, "VERY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Very Active Mins (AF)", xlab="Time/mins")
savePlot(filename="dev-tests-ga+fb-AF.jpg")

x11(width=19, height=10)
par(mfrow=c(4,1))
plot(ga.accel.z[ga.from:ga.to], xaxt="n", main="Geneactive (AF)")
plot(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Lightly Active Mins (AF)")
plot(unlist(fb.xt[from:to, "SEDENTARY_RATIO"]), xaxt="n", main="Fitbit Sedentary Ratio (AF)")
plot(unlist(fb.xt[from:to, "SLEEP_MEASUREMENTS"]), xaxt="n", main="Fitbit Sleep Auto Marking (AF)", xlab="Time/mins")
savePlot(filename="dev-tests-ga+fb+sleep-calls-AF.jpg")


##try plotting as timeseries
#plot(as.POSIXlt(ga$datetime[ga.from:ga.to]), ga.accel.z[ga.from:ga.to], xaxt="n", main="Geneactive (AF)", )
#plot(as.POSIXlt(unlist(fb.xt[fb.from:fb.to, "timestamp.datetime"])), unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n", main="Fitbit Lightly Active Mins (AF)")
require("xts")
ga.ts <- xts(ga$accel.z[ga.from:ga.to], order.by=as.POSIXct(ga$datetime[ga.from:ga.to]))
fb.ts <- xts(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"]), order.by=as.POSIXct(unlist(fb.xt[fb.from:fb.to, "timestamp.datetime"])))
fb.ts.diff <- xts(diff(unlist(fb.xt[fb.from:fb.to, "LIGHTLY_ACTIVE_MINUTES"])), order.by=as.POSIXct(unlist(fb.xt[fb.from:(fb.to-1), "timestamp.datetime"])))  #strictly speaking you should take the average time between each diff rather than the value of the first of the two timepoints... 
x11(width=19, height=10)
par(mfrow=c(3,1))
plot.xts(ga.ts)
plot.xts(fb.ts)
plot.xts(fb.ts.diff)
savePlot(filename="dev-tests-ga+fb-as-timeseries-AF.jpg")




#plot the diff 
par(mfrow=c(5,1))
plot(ga.accel.z[from:length(ga.accel.z)], xaxt="n")
plot(diff(unlist(fb.xt[, "LIGHTLY_ACTIVE_MINUTES"])), log="y") #the diff
plot(unlist(fb.xt[from:to, "LIGHTLY_ACTIVE_RATIO"]), xaxt="n")
plot(unlist(fb.xt[from:to, "LIGHTLY_ACTIVE_MINUTES"]), xaxt="n")
plot(unlist(fb.xt[from:to, "SUMMED_ACTIVE_MINUTES"]), xaxt="n")


#todo 
2. set both onto equal sampling rate, 360:1 of the Genactive == 1/min (which i think is what FB is giving us)
3. plot from same starting & ending points


