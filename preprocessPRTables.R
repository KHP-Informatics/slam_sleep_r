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
#------------------------------------------------------------------------
merge.tables.on.time <- function(table1, table2)
{
    return(merge(table1, table2, by.x=c("timestamp", "event_Date"), by.y=c("timestamp", "event_Date"), all=TRUE))
}

#combined tables:
#merged.data <- merge.tables.on.time(LocationProbe, FitBitApiFeature);


#------------------------------------------------------------------------
# Interpolate the parsed data to estimate missing values
# NB! interpolation will error out if there are not at least 2 non-NA values to interpolate in the column, 
# TODO can automate dropping of columns matching this criteria, then could interp everything, but for now just select columns of interest.
# ARG1 table: dataframe PR table
# ARG2 cols.of.interest: - a set of columns to return
# ARG3 interp.on.cols: -  set of columns of interest (subset of columns.of.interest), defaults to columns which with values that are not all 
#------------------------------------------------------------------------
interp.data <- function(table, cols.of.interest=NULL, interp.on.cols=NULL)
{

    #get the cols of interest, if a set is given (else use all)
    if(is.null(cols.of.interest))
    {
       cols.of.interest <- colnames(table) 
    } else
    {
        table <- table[, cols.of.interest]
    }
    
    # subset test
    if(sum(is.element(interp.on.cols, cols.of.interest)) != length(interp.on.cols))
    {
        stop("ERROR: interp.on.cols not a subset of cols.of.interest")
    }

    #define the interpolation cols
    if(!is.null(interp.on.cols))
    {
        #option a) provide a set of columns from the table on which to interpolate
        #table <- table[, interp.on.cols]  #NOTE: removed, added explicit subsetting cols.of.interest var

    }else
    { 
        #option b) should really validate before interpolate, i.e. only merge columns where there is at least 2 real values, and generate warning
        interp.on.cols <- apply(table, 2, function(x){ sum(is.na(x)) >= length(x)-2 } )
        #table <- table[, interpable.cols]
        message("WARNING: Column with <2 real values detected! Ignoring them for interpolation")
    }

    
    interp.b <- colnames(table) %in% interp.on.cols  
    numeric.b <- sapply(table, class) %in% "numeric" 
    interp.num.cols <- interp.b & numeric.b  #further restrict to interpolate numeric data cols
    dz <- zoo(table[, interp.num.cols])
    index(dz) <- dz[, "timestamp"]
    dz <- na.approx(dz)
    #reasemble
    return(cbind(as.data.frame(dz, stringsAsFactors=FALSE), table[,!interp.num.cols] ))
    
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
# Preprocess two tables: sort, add time cols, join, join, interpolate 
# ARG1 table1: - 1st PR table to merge
# ARG2 table2: - 2nd PR table to merge
# ARG3 columns.of.interest: - a set of columns to return
# ARG4 interp.on.columns: - vector of column names to interpolate on (optional, default=all cols) 
#------------------------------------------------------------------------

preprocess.tables <- function(table1, table2, columns.of.interest, interp.on.columns)
{

    table1 <- order.by.timestamp(table1)
    table1 <- add.timestamp.date(table1)
    table1 <- add.split.date(table1)
    table2 <- order.by.timestamp(table2)
    table2 <- add.timestamp.date(table2)
    table2 <- add.split.date(table2)
    tables.m <- merge.tables.on.time(table1, table2)
    tables.mi <- NULL
    if(! is.null(interp.on.columns))
    {
        tables.mi <- interp.data(tables.m, columns.of.interest, interp.on.columns)
    }else
    {
        tables.mi <- interp.data(tables.m)
    }
    #    tables.mif <- noise.filter(tables.mi)  #TODO
    return(tables.mi)
}



#------------------------------------------------------------------------
# Stub code: Run a selection of preprocessing functions
# running 2 tables at a time, so if you want more than 2 tables recycle 
# the output of this with additional ones recursively
#------------------------------------------------------------------------

#1) some tables from the list of tables (myTables) created in getTable.R

#2) merge and interpolate FitBitApiFeature, LocationProbe, tables:
#e.g. ** currently not all the talbes will pass the interpolation step, prob. want to subset out the cols that very sparsely populated.
# interest.cols <- c("timestamp", "LATITUDE", "ACCURACY", "SPEED", "LIGHTLY_ACTIVE_MINUTES", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "SLEEP_MEASUREMENTS_DT_DURATION", "event_Date",  "event_Hour.y")
# interp.cols <- c("LATITUDE", "ACCURACY", "SPEED", "LIGHTLY_ACTIVE_MINUTES", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "SLEEP_MEASUREMENTS_DT_DURATION")
# data <- preprocess.tables(myTables$FitBitApiFeature, myTables$LocationProbe, interest.cols, interp.cols)








