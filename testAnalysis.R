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
x11(); plot(isect_af$timestamp, isect_af$SLEEP_MEASUREMENTS_DT_DURATION)
x11(); plot(isect_af$timestamp, isect_af$SleepWake)
x11(); plot(isect_af$event_Hour, isect_af$SLEEP_MEASUREMENTS_DT_DURATION)
x11(); plot(isect_af$event_Hour, isect_af$SleepWake)
#dump timepoints which look bad?
bad.sw.1 <- isect_af$SleepWake == 1 & (isect_af$event_Hour> 0 & isect_af$event_Hour < 7)
x11(); plot(isect_af$timestamp[!bad.sw.1], isect_af$SleepWake[!bad.sw.1], main="bad.sw.1")
bad.sw.0 <- isect_af$SleepWake == 0 & (isect_af$event_Hour> 8  & isect_af$event_Hour < 20)
x11(); plot(isect_af$timestamp[!bad.sw.0], isect_af$SleepWake[!bad.sw.0], main="bad.sw.0")
bad.sw.0.1 <- bad.sw.0 | bad.sw.1
x11(); plot(isect_af$timestamp[!bad.sw.0.1], isect_af$SleepWake[!bad.sw.0.1], main="bad.sw.0.1")

#------------------------------------------------------------------------
#  
#------------------------------------------------------------------------


#------------------------------------------------------------------------
# 
#------------------------------------------------------------------------


#------------------------------------------------------------------------
#------------------------------------------------------------------------


#------------------------------------------------------------------------
