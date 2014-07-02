#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
# Interface with the PR database for R
# Extract the required set of data for a chunk of sleep status to 
# forward on to MyHealthLocker/HealthVault
#------------------------------------------------------------------------

#require("RODBC")
require("zoo")


#================== LIBRARY OF PRE-PROCESSING FUNCTIONS ==================

#------------------------------------------------------------------------
# Order by timestamp
# ARG table: - PR table to order by timestamp
#------------------------------------------------------------------------
order.by.timestamp <- function(table)
{
    table <- table[order(table$"timestamp"), ]
    return(table)
}


#------------------------------------------------------------------------
# Append columns for date and time values 
# ARG table: - PR table to add extra date and time cols
#------------------------------------------------------------------------

## original data eventDateTime SEEMS TO BE CHANGED! eventDateTime no longer "2014-03-27 15:20:06" but a variety of other formats 
# FIX by converting timestamps into datetime.

#convert the actual timestamps into a datetime column
add.timestamp.date <- function(table)
{
    table$"timestamp_datetime" <- as.character(as.POSIXct(table$timestamp, origin="1970-01-01"))
    return(table)
}



# Add separate date/time information and mark the midnight hour timestamp values
add.split.date <- function(table)
{
    table$"event_Date" <- sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 1)
    table$"event_Hour" <- as.numeric(sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 2) )
    table$"event_Min" <- as.numeric(sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 3) )
    table$"event_Sec" <- as.numeric(sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 4) )
    table$"midnight_Hour"[table$event_Hour == 00] <- table$timestamp[table$event_Hour == 00]

    return(table)
}


#------------------------------------------------------------------------
# Merge the parsed data 
# ARG1 table1: - 1st PR table to merge
# ARG2 table2: - 2nd PR table to merge
#combined tables:
#merged.data <- merge.tables.on.time(LocationProbe, FitBitApiFeature);
#------------------------------------------------------------------------
merge.tables.on.time <- function(table1, table2)
{
    return(merge(table1, table2, by.x=c("timestamp", "event_Date"), by.y=c("timestamp", "event_Date"), all=TRUE))
}




#------------------------------------------------------------------------
# Interpolate the parsed data to estimate missing values
# NB! interpolation will warn if there are not at least 2 non-NA values to interpolate in the column, 
# ARG1 table: dataframe PR table
# ARG2 interp.on.cols: set of columns to interpolate (must include "timestamp" column),
#                       defaults to columns which have at least 2 non-NA values. 
#                       NOTE: you may want to leave the Date_Hour.x|y out for example as these 
#                       would normally get interpolated, and as they are complimentary there 
#                       are better ways to fill the NAs.
# RETURN dataframe: interpolated on the numeric colums of the interp.on.cols (in the column order of the columns of table)
#------------------------------------------------------------------------
interp.data <- function(table, interp.on.cols=NULL)
{

    #define the interpolation cols
    if(is.null(interp.on.cols))
    { 
        # only merge columns where there is at least 2 real values, and generate warning
        interp.on.cols <- apply(table, 2, function(x){ sum(is.na(x)) >= length(x)-2 } )
        #table <- table[, interpable.cols]
        message("WARNING: Column with <2 real values detected! Ignoring them for interpolation")
    }
    
    interp.b <- colnames(table) %in% interp.on.cols  #which cols of table are marked for interpolation
    numeric.b <- sapply(table, class) %in% "numeric"  #which cols of table are numeric
    interp.num.cols <- interp.b & numeric.b  #intersect of interpolate and numeric cols
    dz <- zoo(table[, interp.num.cols])
    index(dz) <- dz[, "timestamp"]
    dz <- na.approx(dz)
    #reasemble interpolated cols and non-interpolated cols into a dataframe and reorder cols back to interp.on.cols order
    return( cbind(as.data.frame(dz, stringsAsFactors=FALSE), table[,!interp.num.cols])[colnames(table)] )
    
}




#------------------------------------------------------------------------
# Smooth the Interpolated data, window=5
#------------------------------------------------------------------------
noise.filter <- function(table)
{
    return(runmed(na.exclude(table), 5))
}


#==================== END OF PREPROCESSING FUNCs ========================



#------------------------------------------------------------------------
# Sample preprocessing pipeline merging two tables at a time.
# e.g. here two tables, sort, add time cols, join, join, interpolate 
# ARG1 table1: 1st PR table to merge
# ARG2 table2: 2nd PR table to merge
# ARG3 interp.on.columns: vector of column names to interpolate on (optional, default=all cols)  see interp.data func.
# ARG4 columns.of.interest (optoinal): a set of columns to subset to
#------------------------------------------------------------------------

preprocess.tables <- function(table1, table2, interp.on.columns, columns.of.interest=NULL)
{

    table1 <- order.by.timestamp(table1)
    table1 <- add.timestamp.date(table1)
    table1 <- add.split.date(table1)
    table2 <- order.by.timestamp(table2)
    table2 <- add.timestamp.date(table2)
    table2 <- add.split.date(table2)
    table.m <- merge.tables.on.time(table1, table2)
    table.mi <- NULL
   
    if(!is.null(columns.of.interest))
    {
        #first subset with the columns.of.interest if given
        table.mi <- interp.data(table.m[columns.of.interest], interp.on.columns)
    }else
    {
        table.mi <- interp.data(table.m, interp.on.columns)
    }

    #    table.mif <- noise.filter(table.mi)  #TODO
    return(table.mi)
}



#------------------------------------------------------------------------
# Stub code: Run a selection of preprocessing functions
# running 2 tables at a time, so if you want more than 2 tables recycle 
# the output of this with additional ones recursively
#------------------------------------------------------------------------

#1) some tables from the list of tables (myTables) created in getTable.R

#2) merge and interpolate FitBitApiFeature, LocationProbe, tables:
#e.g. ** currently not all the talbes will pass the interpolation step, prob. want to subset out the cols that very sparsely populated.
# interest.cols <- c("timestamp", "LATITUDE", "ACCURACY", "SPEED", "LIGHTLY_ACTIVE_MINUTES", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "SLEEP_MEASUREMENTS_DT_DURATION", "event_Date", "event_Hour.x",  "event_Hour.y")
# interp.cols <- c("timestamp", "LATITUDE", "ACCURACY", "SPEED", "LIGHTLY_ACTIVE_MINUTES", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "SLEEP_MEASUREMENTS_DT_DURATION")
# data <- preprocess.tables(myTables$FitBitApiFeature, myTables$LocationProbe, interp.cols, interest.cols)








