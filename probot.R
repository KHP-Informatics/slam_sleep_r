#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
# A quick look at the initial month or so of purplerobot + fitbit data
# 1) get a handle on data
# 2) build a classifier with hand picked vars loc and fitbit activity measure.
# 3) trial some machine learning using sleep times labelled by fitbit (i.e.
#    where you double tapped the fitbit device) -- this can be your gold 
#    standard for sleep or training set.
#
# fitbit API info on sleep params:
# https://wiki.fitbit.com/display/API/API-Get-Sleep
#------------------------------------------------------------------------

# purple robot data dir (downloaded postgres dbs extracted into csv format)
data.dir <- "/home/afolarinbrc/workspace/Datasets/purple_robot_data/purple_dbs/csv/";

accel <- read.csv(paste(data.dir, "af_AccelerometerProbe.csv", sep=""), stringsAsFactors=F, header=T);
fitbit <- read.csv(paste(data.dir, "af_FitBitApiFeature.csv", sep=""), stringsAsFactors=F, header=T);
location <- read.csv(paste(data.dir, "af_LocationProbe.csv", sep=""), stringsAsFactors=F, header=T);
screen <- read.csv(paste(data.dir, "af_ScreenProbe.csv", sep=""), stringsAsFactors=F, header=T);
temp <- read.csv(paste(data.dir, "af_TemperatureProbe.csv, sep="")", stringsAsFactors=F, header=T);
deviceuse <- read.csv(paste(data.dir, "af_DeviceInUseFeature.csv", sep=""), stringsAsFactors=F, header=T);
light <- read.csv(paste(data.dir, "af_LightProbe.csv", sep=""), stringsAsFactors=F, header=T);
prhealth <- read.csv(paste(data.dir, "af_RobotHealthProbe.csv", sep=""), stringsAsFactors=F, header=T);
sun.rs <- read.csv(paste(data.dir, "af_SunriseSunsetFeature.csv", sep=""), stringsAsFactors=F, header=T);
weather.ug <- read.csv(paste(data.dir, "af_WeatherUndergroundFeature.csv", sep=""), stringsAsFactors=F, header=T);


#--------------------  fitbit data -------------------------------------------

#Added cols
#split eventDateTime
fitbit$"event_Date" <- sapply(strsplit(fitbit$eventDateTime, "[ :]"), "[", 1);
fitbit$"event_Hour" <- sapply(strsplit(fitbit$eventDateTime, "[ :]"), "[", 2);
fitbit$"event_Min" <- sapply(strsplit(fitbit$eventDateTime, "[ :]"), "[", 3);
fitbit$"event_Sec" <- sapply(strsplit(fitbit$eventDateTime, "[ :]"), "[", 4);
fitbit$"midnight_Hour"[fitbit$event_Hour == "00"] <- fitbit$timestamp[fitbit$event_Hour == "00"];


fit.m <- as.matrix(fitbit[, c("VERY_ACTIVE_MINUTES", "FAIRLY_ACTIVE_MINUTES", "LIGHTLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "ACTIVITY_CALORIES", "SLEEP_MEASUREMENTS_DT_AWAKENINGS_COUNT", "SLEEP_MEASUREMENTS_DT_AWAKE_COUNT", "SLEEP_MEASUREMENTS_DT_DURATION", "SLEEP_MEASUREMENTS_DT_MINUTES_ASLEEP", "SLEEP_MEASUREMENTS_DT_MINUTES_AWAKE", "SLEEP_MEASUREMENTS_DT_MINUTES_IN_BED_AFTER", "SLEEP_MEASUREMENTS_DT_MINUTES_IN_BED_BEFORE", "SLEEP_MEASUREMENTS_DT_RESTLESS_COUNT", "SLEEP_MEASUREMENTS_DT_TIME_IN_BED", "STEPS")]);
cor(fit.m, use="pairwise.complete.obs");

#----------------some quickie exploratory plots


#on log scale (though dropping points...?)
x11(width=18,height=9);
fit.nc<- ncol(fit.m);
matplot(fitbit$timestamp, fit.m, log="y", pch=1:fit.nc, col=1:fit.nc);
legend(locator(1), colnames(fit.m), pch =1:fit.nc, col =1:fit.nc, cex=0.7, main="Fitbit Data", xlab="event timestamp", ylab="various fitbit metrics");
abline(v=fitbit$midnight_Hour);
savePlot("fitbit-lg-all.jpg");

# leave out an the off scale data col#8 SLEEP_MEASUREMENTS_DT_DURATION
x11(width=18,height=9);
fit.m2 <- fit.m[, -8];
fit.nc<- ncol(fit.m2);
matplot(fitbit$timestamp, fit.m2, pch=1:fit.nc, col=1:fit.nc,  main="Fitbit Data", sub="without SLEEP_MEASUREMENTS_DT_DURATION and linear x-axis", xlab="event timestamp", ylab="various fitbit metrics");
legend(locator(1), colnames(fit.m2), pch =1:fit.nc, col =1:fit.nc, cex=0.7);
abline(v=fitbit$midnight_Hour);
savePlot("fitbit-linearsc.jpg");

# look closer at just 24hrs data (#11 2014-04-10 00:04:15 -> #291 2014-04-10 23:55:41)
x11(width=18,height=9);
fit.m.d <- fit.m[11:291, ];
fit.nc<- ncol(fit.m.d);
matplot(fitbit$timestamp[11:291], fit.m.d, log="y", pch=1:fit.nc, col=1:fit.nc, main="Fitbit Data", sub="24hr Slice (2014-04-10)", xlab="event timestamp", ylab="various fitbit metrics");
#matplot(fitbit$timestamp[11:291], fit.m.d, pch=1:fit.nc, col=1:fit.nc);
legend(locator(1), colnames(fit.m.d), pch =1:fit.nc, col =1:fit.nc, cex=0.7);
abline(v=fitbit$midnight_Hour);
savePlot("fitbit-24hr-slice.jpg");


# fitbit time slice
fitbit.ts <- split(fitbit, fitbit$"event_Date");

plot_acti_slice <- function(x)
{
    # a 24hrs slice
    x11(width=18,height=9);
    fit.m.d <- x["LIGHTLY_ACTIVE_MINUTES"];
    fit.nc<- ncol(fit.m.d);
    matplot(x$timestamp, fit.m.d, pch=1:fit.nc, col=1:fit.nc, main="Fitbit Data", sub="24hr Slice (2014-04-10)", xlab="event timestamp", ylab="various fitbit metrics");
    legend("topleft", legend=colnames(fit.m.d), inset=.05, pch =1:fit.nc, col =1:fit.nc, cex=0.7);
    abline(v=x$midnight_Hour);
    savePlot(paste("fitbit-24hr-slice_", x$event_Date[1], "_.jpg", sep=""));

}

#plot_acti_slice(fitbit.ts[[1]]);
lapply(fitbit.ts, plot_acti_slice);










#----------------------- locationProbe data -----------------------------------
#Added cols
#split eventDateTime
location$"event_Date" <- sapply(strsplit(location$eventDateTime, "[ :]"), "[", 1);
location$"event_Hour" <- sapply(strsplit(location$eventDateTime, "[ :]"), "[", 2);
location$"event_Min" <- sapply(strsplit(location$eventDateTime, "[ :]"), "[", 3);
location$"event_Sec" <- sapply(strsplit(location$eventDateTime, "[ :]"), "[", 4);
location$"midnight_Hour"[location$event_Hour == "00"] <- location$timestamp[location$event_Hour == "00"];


#quick cor 
loc.m <- as.matrix(location[, c("ALTITUDE", "BEARING", "SPEED", "LONGITUDE", "LATITUDE", "ACCURACY")]);
cor(loc.m, use="pairwise.complete.obs");

#----------------some quickie exploratory plots
 

#on log scale (though dropping points...?)
x11(width=18,height=9);
loc.nc<- ncol(loc.m);
matplot(location$timestamp, loc.m, log="y", pch=1:loc.nc, col=1:loc.nc);
legend(locator(1), colnames(loc.m), pch =1:loc.nc, col =1:loc.nc, cex=0.7);
abline(v=location$midnight_Hour);
savePlot("location-lg-all.jpg");

# longitude and latitude only
loc.ll <- loc.m[, c("LONGITUDE", "LATITUDE")];
x11(width=18,height=9);
loc.nc<- ncol(loc.ll);
matplot(location$timestamp, loc.ll, log="y", pch=1:loc.nc, col=1:loc.nc,  main="Purple Robot Location Probe", xlab="event timestamp", ylab="various location probe metrics");
legend(locator(1), colnames(loc.ll), pch =1:loc.nc, col =1:loc.nc, cex=0.7);
abline(v=location$midnight_Hour);
savePlot("location-lg-geo.jpg");

# latitude only
loc.ll <- loc.m[, c("LATITUDE")];
#loc.ll <- scale(loc.ll);
loc.ll[loc.ll > 51.5] <- NA;
x11(width=18,height=9);
loc.nc<- 1;
#matplot(location$timestamp, loc.ll, log="y", pch=1:loc.nc, col=1:loc.nc);
matplot(location$timestamp, loc.ll, pch=1:loc.nc, col=1:loc.nc, lty=2, main="Purple Robot Latitude", xlab="event timestamp", ylab="latitude");
legend(locator(1), "LATITUDE", pch =1:loc.nc, col =1:loc.nc, cex=0.7);
abline(v=location$midnight_Hour);
savePlot("location-lin-lat.jpg");

# latitude only 24hr slice (#9005 2014-04-10 00:01:25 -> #10027 2014-04-10 23:57:05)
loc.ll <- loc.m[, c("LATITUDE")];
#loc.ll <- scale(loc.ll);
loc.ll[loc.ll > 51.5] <- NA; ### blank points above my home latitude.. ** need better way to scale!!!!
loc.ll<- loc.ll[9005:10027]  # equivalent 24hr slice to fitbit above
x11(width=18,height=9);
loc.nc<- 1;
#matplot(location$timestamp, loc.ll, log="y", pch=1:loc.nc, col=1:loc.nc);
matplot(location$timestamp[9005:10027], loc.ll, pch=1:loc.nc, col=1:loc.nc, lty=2, main="Purple Robot Latitude", sub="24hr Slice (2014-04-10)", xlab="event timestamp", ylab="latitude");
legend(locator(1), "LATITUDE", pch =1:loc.nc, col =1:loc.nc, cex=0.7);
abline(v=location$midnight_Hour);
savePlot("location-lin-lat_24hr-slice.jpg");


### let's look at each 24hr timeslice
loc.ts <- split(location, location$"event_Date");

#plot_loc_slice <- function(x)
#{
#    # latitude only 24hr slice 
#    loc.ll <- as.matrix(x[, c("LATITUDE")]);
#    #loc.ll <- scale(loc.ll);
#    loc.ll[loc.ll > 51.5] <- NA;  ### blank points above my latitude... ** need better way to scale!!!! TODO
#    x11(width=18, height=9);
#    loc.nc<- 1;
#    #matplot(location$timestamp, loc.ll, log="y", pch=1:loc.nc, col=1:loc.nc);
#    matplot(x$timestamp, loc.ll, pch=1:loc.nc, col=1:loc.nc, lty=2, main="Purple Robot Latitude", sub="24hr Slice", xlab="event timestamp", ylab="latitude");
#    #legend(locator(1), "LATITUDE", pch =1:loc.nc, col =1:loc.nc, cex=0.7);
#    abline(v=x$midnight_Hour);
#}

plot_loc_slice <- function(x)
{
    # latitude only 24hr slice 
    loc.ll <- x["LATITUDE"]; #grab single col dataframe
    #loc.ll <- scale(loc.ll);
    loc.ll[loc.ll > 51.5] <- NA;  ### blank points above my home latitude... ** need better way to scale!!!! TODO
    loc.ll[loc.ll < 51.45] <- NA;  ### blank points below my home latitude... ** need better way to scale!!!! TODO
    x11(width=18, height=9);
    loc.nc<- ncol(loc.ll);
    #matplot(location$timestamp, loc.ll, log="y", pch=1:loc.nc, col=1:loc.nc);
    matplot(x$timestamp, loc.ll, pch=1:loc.nc, col=1:loc.nc, lty=2, main="Purple Robot Latitude", sub="24hr Slice", xlab="event timestamp", ylab="latitude");
    legend("topleft", legend=colnames(loc.ll), inset=.05, pch =1:loc.nc, col =1:loc.nc, cex=0.7);
    abline(v=x$midnight_Hour);
    savePlot(paste("location-24hr-slice_", x$event_Date[1], "_.jpg", sep=""));
}

#plot_loc_slice(loc.ts[[2]]);
lapply(loc.ts, plot_loc_slice);





## get parallel time dataset for fitbit and location (here 09-24/04/2014)
start.ts <- rownames(location[c("id","event_Date")][location$event_Date %in% "2014-04-09", ][1,]);
end <- location[c("id","event_Date")][location$event_Date %in% "2014-04-24", ];
end.ts <- rownames(end[nrow(end), ])
loc.j <- location[start.ts:end.ts, ];  #grab overlapping days with fitbit data
fit.j <- fitbit;

#joined set (for further work on integrating the probe data, join on timestamp)
loc.fit.merge <- merge(loc.j, fit.j, by.x="timestamp", by.y="timestamp", all=T)


plot_joint_slice <- function(x)
{
    # split the joined lists back into their constituents
    l <- x$loc.ts;
    f <- x$fit.ts;

    x11(width=18, height=9);
    
    # latitude only 24hr slice 
    loc.ll <- l["LATITUDE"]; #grab single col dataframe
    #loc.ll <- scale(loc.ll);
    loc.ll[loc.ll > 51.5] <- NA;  ### blank points above my home latitude... ** need better way to scale!!!! TODO
    loc.ll[loc.ll < 51.45] <- NA;  ### blank points below my home latitude... ** need better way to scale!!!! TODO
    loc.nc<- ncol(loc.ll);
    #matplot(location$timestamp, loc.ll, log="y", pch=1:loc.nc, col=1:loc.nc);
    matplot(l$timestamp, loc.ll, pch=1:loc.nc, col=1:loc.nc, lty=2, main="Latitude and Lightly Active Minutes", sub="24hr Slice", xlab="event timestamp", ylab="latitude and lightly active mins");
    legend("topleft", legend=colnames(loc.ll), inset=.05, pch =1:loc.nc, col =1:loc.nc, cex=0.7);
    abline(v=l$midnight_Hour);

    # a 24hrs slice
    fit.m.d <- f["LIGHTLY_ACTIVE_MINUTES"];
    fit.nc<- ncol(fit.m.d);
    matpoints(f$timestamp, fit.m.d, pch=1:fit.nc, col=1:fit.nc);
    legend("topright", legend=colnames(fit.m.d), inset=.05, pch =1:fit.nc, col =1:fit.nc, cex=0.7);
    abline(v=f$midnight_Hour);

    savePlot(paste("joint-24hr-slice_", l$event_Date[1], "_.jpg", sep=""));

}

#split by day, 
loc.ts <- split(loc.j, loc.j$"event_Date");
fit.ts <- split(fit.j, fit.j$"event_Date");
#cbind the 2 lists
slices <- cbind(loc.ts, fit.ts);

lapply(slices, plot_joint_slice);




