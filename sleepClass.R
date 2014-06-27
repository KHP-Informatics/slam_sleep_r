#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
# Simple bining for fitbit SLEEP_MEASUREMENTS_DT_DURATION, 
# need a binary var not numeric type given
# simple kmeans 2 cluster classifier solves this easily
# ARG1: dataframe
# RETURN numeric vector: 1=wake, 0=sleep
#------------------------------------------------------------------------
binSleepDuration <- function(data)
{
    bin.k <- kmeans(data$SLEEP_MEASUREMENTS_DT_DURATION, centers=2, iter.max=100) 

    #sub back in the values -- guarantee which way round the labels will go as kmeans not deterministic
    centroid <- aggregate(data$SLEEP_MEASUREMENTS_DT_DURATION,by=list(bin.k$cluster),FUN=mean)
    centroid <- as.data.frame(centroid)
    if(centroid[1,2] < centroid[2,2])
    {
        bin.k$cluster[bin.k$cluster == 1] <- 0 #sleep = 0
            bin.k$cluster[bin.k$cluster == 2] <- 1 #wake = 1
    }else if(centroid[1,2] > centroid[2,2])
    {
        bin.k$cluster[bin.k$cluster == 1] <- 1 #sleep = 0
            bin.k$cluster[bin.k$cluster == 2] <- 0 #wake = 1
    }else
    {
        warning("problem with SLEEP_MEASUREMENTS_DT_DURATION, equal centroid vals")
    }

    return(bin.k$cluster)
}



#------------------------------------------------------------------------
# 
#------------------------------------------------------------------------


#------------------------------------------------------------------------
# Naive Bayes Classification func 
#------------------------------------------------------------------------



#------------------------------------------------------------------------
# Random Forrest Classification func
#------------------------------------------------------------------------




#------------------------------------------------------------------------
# 
#------------------------------------------------------------------------




#------------------------------------------------------------------------
# 
#------------------------------------------------------------------------


    
