#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#########################################################################
# Collate the data for our groups PR and Fitbit tables and do some 
# basic sleep classification
#########################################################################

#source some stuff. 
source("/home/afolarinbrc/workspace/git_projects/slam_sleep_r/getPRTables.R")
source("/home/afolarinbrc/workspace/git_projects/slam_sleep_r/preprocessPRTables.R")
source("/home/afolarinbrc/workspace/git_projects/slam_sleep_r/prPlotting.R")
source("/home/afolarinbrc/workspace/git_projects/slam_sleep_r/sleepClass.R")

#------------------------------------------------------------------------
# Devices have diff starting timestamps when data from tables are merged
# select first non-NA value for a column
# ARG1 d: dataframe 
# ARG2 column.name: the column of interest
# RETURN vector: vector[1] = start , vector[2] = end
#------------------------------------------------------------------------
getStartEndRows <- function(d, column.name)
{
    nnul <- which(!is.na(d[column.name]))
    start <- nnul[1]
    end <- nnul[length(nnul)]
    return(c(start,end))
}


#------------------------------------------------------------------------
# Load the data tables from getPRTables.R and preprocessPRTables.R
#------------------------------------------------------------------------
setwd("/scratch/Datasets/purple_robot_data/pr_test_data")

for(i in dir(pattern="*.RData"))
{ load(i)}

ls()




#------------------------------------------------------------------------
# For each dataset collate the intersecting data for purple robot and 
# fitbit tables
# *TODO* get all the tables...
#------------------------------------------------------------------------

#Amos
nn.range_af <- getStartEndRows(merged_af, "LIGHTLY_ACTIVE_MINUTES")
isect_af <- merged_af[nn.range_af[1]:nn.range_af[2], ]

#Steve
nn.range_sn <- getStartEndRows(merged_sn, "LIGHTLY_ACTIVE_MINUTES")
isect_sn <- merged_sn[nn.range_sn[1]:nn.range_sn[2], ]

#Rich

#Zina

#Cas


#------------------------------------------------------------------------
# Extra prep for classification 
#------------------------------------------------------------------------
# 1) Add merged Event_Hour.x Event_Hour.y 
t1 <- isect_af$event_Hour.x
t2 <- isect_af$event_Hour.y
t1[is.na(isect_af$event_Hour.x)] <- t2[is.na(isect_af$event_Hour.x)]
isect_af$"event_Hour" <- t1

t1 <- isect_sn$event_Hour.x
t2 <- isect_sn$event_Hour.y
t1[is.na(isect_sn$event_Hour.x)] <- t2[is.na(isect_sn$event_Hour.x)]
isect_sn$"event_Hour" <- t1


# 2)  Add Sleep-Wake binary var
isect_af$"SleepWake" <- binSleepDuration(isect_af)
isect_sn$"SleepWake" <- binSleepDuration(isect_sn)


# see plots, for sleep-wake: sn_fb-loc_sleep-dura.jpg, af_fb-loc_sleep-dura.jpg
# quite a few days where fitbit was not registered in sleep/wake mode
# so need to cleanup
#x11(); plot(isect_af$timestamp, isect_af$SLEEP_MEASUREMENTS_DT_DURATION)
#x11(); plot(isect_af$timestamp, isect_af$SleepWake)
#x11(); plot(isect_af$event_Hour, isect_af$SLEEP_MEASUREMENTS_DT_DURATION)
#x11(); plot(isect_af$event_Hour, isect_af$SleepWake)
#dump timepoints which look bad?
bad.sw.1 <- isect_af$SleepWake == 1 & (isect_af$event_Hour> 0 & isect_af$event_Hour < 7)
#x11(); plot(isect_af$timestamp[!bad.sw.1], isect_af$SleepWake[!bad.sw.1], main="bad.sw.1")
bad.sw.0 <- isect_af$SleepWake == 0 & (isect_af$event_Hour> 8  & isect_af$event_Hour < 20)
#x11(); plot(isect_af$timestamp[!bad.sw.0], isect_af$SleepWake[!bad.sw.0], main="bad.sw.0")
bad.sw.0.1 <- bad.sw.0 | bad.sw.1
#x11(); plot(isect_af$timestamp[!bad.sw.0.1], isect_af$SleepWake[!bad.sw.0.1], main="bad.sw.0.1")
#cleaned
data.af <- isect_af[!bad.sw.0.1, ]

#x11(); plot(isect_sn$timestamp, isect_sn$SLEEP_MEASUREMENTS_DT_DURATION)
#x11(); plot(isect_sn$timestamp, isect_sn$SleepWake)
#x11(); plot(isect_sn$event_Hour, isect_sn$SLEEP_MEASUREMENTS_DT_DURATION)
#x11(); plot(isect_sn$event_Hour, isect_sn$SleepWake)
#dump timepoints which look bad?
bad.sw.1 <- isect_sn$SleepWake == 1 & (isect_sn$event_Hour> 0 & isect_sn$event_Hour < 7)
#x11(); plot(isect_sn$timestamp[!bad.sw.1], isect_sn$SleepWake[!bad.sw.1], main="bad.sw.1")
bad.sw.0 <- isect_sn$SleepWake == 0 & (isect_sn$event_Hour> 8  & isect_sn$event_Hour < 20)
#x11(); plot(isect_sn$timestamp[!bad.sw.0], isect_sn$SleepWake[!bad.sw.0], main="bad.sw.0")
bad.sw.0.1 <- bad.sw.0 | bad.sw.1
#x11(); plot(isect_sn$timestamp[!bad.sw.0.1], isect_sn$SleepWake[!bad.sw.0.1], main="bad.sw.0.1")
#cleaned
data.sn <- isect_sn[!bad.sw.0.1, ]


#------------------------------------------------------------------------
# Naieve Bayes Classifier  
#------------------------------------------------------------------------
require("klaR")
require("caret")


#---try with the caret package  ## Takes a looooong time!!!
    #x <- di.k2[, c("LATITUDE", "LIGHTLY_ACTIVE_MINUTES")]
    x <- data.af[, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    y <- factor(data.af$"SLEEP_MEASUREMENTS_DT_DURATION") #must be a factor not dataframe

    classifier2 <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))  # Takes a looooong time!
    classifier2
    pred2 <- predict(classifier2$finalModel, x)$class
    table(y, pred2, dnn=list('actual', 'predicted'))

#---try with the caret package  ## Takes a looooong time!!!
    #x <- di.k2[, c("LATITUDE", "LIGHTLY_ACTIVE_MINUTES")]
    x <- data.af[, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES",  "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    y <- factor(data.af$"SleepWake") #must be a factor not dataframe
    classifier2 <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))  # Takes a looooong time!
    pred2 <- predict(classifier2$finalModel, x)$class
    table(y, pred2, dnn=list('actual', 'predicted'))
    #look at the training and predicted vals
    tmp <- cbind(unclass(y), unclass(pred2))
    rownames(tmp) <- rownames(di.k2)
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")



#---try with the caret package  ## n=5000 try a quick version with less data
    sub.r <- sample(1:nrow(data.af), 5000)
    x <- data.af[sub.r, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES",  "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
#    x <- x[sub.r, c("LATITUDE", "LIGHTLY_ACTIVE_MINUTES")]   #get a subset of x, 5000 rows.
    y <- factor(data.af$SleepWake)
    y <- y[sub.r]
    classifier2 <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))  
    pred2 <- predict(classifier2$finalModel, x)$class
    table(y, pred2, dnn=list('actual', 'predicted'))
   #look at the training and predicted vals
    tmp <- cbind(unclass(y), unclass(pred2))
    rownames(tmp) <- rownames(data.af[sub.r,])
    tmp <- tmp[sort(rownames(tmp)), ]
    x11(); plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")

    # now try classifying all data.af, using the classifier produced from the 5000 rnd sample subset of the data
    x.all <- data.af[, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES",  "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    y.all <- factor(data.af$SleepWake)
    pred2 <- predict(classifier2$finalModel, x.all)$class
    table(y.all, pred2, dnn=list('actual', 'predicted'))
    #look at the training and predicted vals
    tmp <- cbind(unclass(y.all), unclass(pred2))
    rownames(tmp) <- rownames(data.af)
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")

    # now try classifying all data.sn, using the classifier produced from the 5000 rnd sample subset of the data
    x.all <- data.sn[, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES",  "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    y.all <- factor(data.sn$SleepWake)
    pred2 <- predict(classifier2$finalModel, x.all)$class
    table(y.all, pred2, dnn=list('actual', 'predicted'))
    #look at the training and predicted vals
    tmp <- cbind(unclass(y.all), unclass(pred2))
    rownames(tmp) <- rownames(data.sn)
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")


#---try with the caret package  ## n=10,000 try a quick version with less data
    sub.r <- sample(1:nrow(data.af), 10000)
    x <- data.af[sub.r, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
#    x <- x[sub.r, c("LATITUDE", "LIGHTLY_ACTIVE_MINUTES")]   #get a subset of x, 5000 rows.
    y <- factor(data.af$SleepWake)
    y <- y[sub.r]
    classifier2 <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))  
    pred2 <- predict(classifier2$finalModel, x)$class
    table(y, pred2, dnn=list('actual', 'predicted'))
   #look at the training and predicted vals
    tmp <- cbind(unclass(y), unclass(pred2))
    rownames(tmp) <- rownames(data.af[sub.r,])
    tmp <- tmp[sort(rownames(tmp)), ]
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")

    # now try classifying all data.af, using the classifier produced from the 10000 rnd sample subset of the data
    x.all <- data.af[, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES",  "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    y.all <- factor(data.af$SleepWake)
    pred2 <- predict(classifier2$finalModel, x.all)$class
    table(y.all, pred2, dnn=list('actual', 'predicted'))
    #look at the training and predicted vals
    tmp <- cbind(unclass(y.all), unclass(pred2))
    rownames(tmp) <- rownames(data.af)
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")

    # now try classifying all data.sn, using the classifier produced from the 10000 rnd sample subset of the data
    x.all <- data.sn[, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES",  "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    y.all <- factor(data.sn$SleepWake)
    pred2 <- predict(classifier2$finalModel, x.all)$class
    table(y.all, pred2, dnn=list('actual', 'predicted'))
    #look at the training and predicted vals
    tmp <- cbind(unclass(y.all), unclass(pred2))
    rownames(tmp) <- rownames(data.sn)
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")


#no build a classifier with steves data, does this improve things or is the dataset bad??
#  now try classifying all data.sn, using the classifier produced from the 10000 rnd sample subset of the data
#---try with the caret package  ## n=10,000 try a quick version with less data
    sub.r <- sample(1:nrow(data.sn), 2000)
    x <- data.sn[sub.r, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
#    x <- x[sub.r, c("LATITUDE", "LIGHTLY_ACTIVE_MINUTES")]   #get a subset of x, 5000 rows.
    y <- factor(data.sn$SleepWake)
    y <- y[sub.r]
    classifier.sn <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))
    pred2 <- predict(classifier.sn$finalModel, x)$class
    table(y, pred2, dnn=list('actual', 'predicted'))


    x.all <- data.sn[, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES",  "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    y.all <- factor(data.sn$SleepWake)
    pred2 <- predict(classifier.sn$finalModel, x.all)$class
    table(y.all, pred2, dnn=list('actual', 'predicted'))
    #look at the training and predicted vals
    tmp <- cbind(unclass(y.all), unclass(pred2))
    rownames(tmp) <- rownames(data.sn)
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green")
    


##--- actually looks like latitude should be a delta-latitude, esp for generalizaing the classifier
    sub.r <- sample(1:nrow(data.af), 10000)
    x <- data.af[sub.r, c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "event_Hour")]
    d <- cbind(0, x$LATITUDE[-length(x$LATITUDE)])
    x$LATITUDE <- x - d
    y <- factor(data.af$SleepWake)
    y <- y[sub.r]
    classifier2 <- train(x,y,'nb', trControl=trainControl(method='cv', number=10))
    pred2 <- predict(classifier2$finalModel, x)$class
    table(y, pred2, dnn=list('actual', 'predicted'))
    #look at the training and predicted vals
    tmp <- cbind(unclass(y), unclass(pred2))
    rownames(tmp) <- rownames(data.af[sub.r,])
    tmp <- tmp[sort(rownames(tmp)), ]
    x11();plot(rownames(tmp), tmp[,1])
    lines(rownames(tmp), tmp[,2], pch=5, col="green"



#------------------------------------------------------------------------
# Figs: showing some of the data vars in quick plot 
#------------------------------------------------------------------------
opar <- par(no.readonly=T)
x11(width=19, height=10.5, pointsize=8)
par(mfrow=c(7,1), mar=c(2,2,2,1))
tmp <- data.af[20000:40000, ]
tmp$LATITUDE <- (log(tmp$LATITUDE))
#tmp <- data.af[12000:19000, ]
plot(tmp$timestamp, tmp$"SleepWake", xaxt='n', ann=FALSE)
plot(tmp$timestamp, tmp$"SEDENTARY_MINUTES", xaxt='n', ann=FALSE)
plot(tmp$timestamp, tmp$"LIGHTLY_ACTIVE_MINUTES", xaxt='n', ann=FALSE)
plot(tmp$timestamp, tmp$"FAIRLY_ACTIVE_MINUTES", xaxt='n', ann=FALSE)
plot(tmp$timestamp, tmp$"VERY_ACTIVE_MINUTES", xaxt='n', ann=FALSE)
plot(tmp$timestamp, tmp$"LATITUDE", xaxt='n', ann=FALSE)
plot(tmp$timestamp, tmp$"SPEED")
par(opar)


# plot a heatmap of the data
require("gplots")
sub.r <- sample(1:nrow(data.af), 10000)
d <- as.matrix(data.af[!colnames(data.af) %in% c("event_Date", "timestamp")])
x11(width=19, height=10.5); heatmap.2(d[sub.r, ], scale=c("column"))
x11(width=19, height=10.5); heatmap.2(d[sub.r, ])


#------------------------------------------------------------------------
#------------------------------------------------------------------------


#------------------------------------------------------------------------
