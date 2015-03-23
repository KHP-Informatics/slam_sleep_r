#Benchmark GenActive vs FitbitHR
#===============================


#load GenActive Data
#-------------------
ga<- read.csv("~/Dropbox/purple_robot/test_genwatch-v-fitbitHR/genwatch/2015-03-20_16-09.csv", 
        col.names=c("datetime","u1","u2","u3","u4","u5","accel.x","accel.y","accel.z","light","button.pressed","temp"),
        skip=100)

##NOTE: seems like the temp and accel.x are the wrong way round ?? hard to interpret without the column names... I'm guessing here.

#Data @ 10Hz, get the 1/6 average, i.e. downsample to 1/min
require(zoo)
#ga.ts <- zoo(ga)

#select equivalent datetime points
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
#---------------------
fb <- read.table("~/Dropbox/purple_robot/test_genwatch-v-fitbitHR/export_2015-03-17_2015-03-20_uxu7zx8gg3iq4oob.txt", sep="\t", header=T, stringsAsFactors=F, quote='')

require(jsonlite)

#parse the Json column
fb.xt <- t(sapply(fb$Payload, fromJSON, simplifyVector=T))
fb.xt <- cbind(fb[, -4], fb.xt) 
rownames(fb.xt) <- 1:nrow(fb.xt) #remove large json str. rowname as obstructive to viewing

#plot
plot(unlist((fb.xt[, "LIGHTLY_ACTIVE_MINUTES"])))
plot(unlist((fb.xt[, "FAIRLY_ACTIVE_MINUTES"])))
plot(unlist((fb.xt[, "VERY_ACTIVE_MINUTES"])))
plot(unlist((fb.xt[, "SLEEP_MEASUREMENTS"])))
plot(unlist((fb.xt[, "ACTIVE_SCORE"])))

#todo 
1. convert timestamp in FB to datetime
2. set both onto equal sampling rate, 360:1 of the Genactive == 1/min (which i think is what FB is giving us)
3. plot from same starting & ending points


