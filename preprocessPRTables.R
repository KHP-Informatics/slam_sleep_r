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

#require("RODBC");
require("zoo");


#================== LIBRARY OF PRE-PROCESSING FUNCTIONS ==================

#------------------------------------------------------------------------
# Order by timestamp
#------------------------------------------------------------------------
order.by.timestamp <- function(table)
{
    table <- table[order(table$"timestamp"), ];
    return(table);
}


#------------------------------------------------------------------------
# Append columns for date and time values 
#------------------------------------------------------------------------

## original data eventDateTime SEEMS TO BE CHANGED! eventDateTime no longer "2014-03-27 15:20:06" but a variety of other formats 
# FIX by converting timestamps into datetime.

#convert the actual timestamps into a datetime column
add.timestamp.date <- function(table)
{
    table$"timestamp_datetime" <- as.character(as.POSIXct(table$timestamp, origin="1970-01-01"));
    return(table);
}



# Add separate date/time information and mark the midnight hour timestamp values
add.split.date <- function(table)
{
    table$"event_Date" <- sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 1);
    table$"event_Hour" <- sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 2); 
    table$"event_Min" <- sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 3); 
    table$"event_Sec" <- sapply(strsplit(table$timestamp_datetime, "[ :]"), "[", 4); 
    table$"midnight_Hour"[table$event_Hour == "00"] <- table$timestamp[table$event_Hour == "00"];

    return(table);
}


#------------------------------------------------------------------------
# Merge the parsed data 
#------------------------------------------------------------------------
merge.tables.on.time <- function(table1, table2)
{
    return(merge(table1, table2, by.x=c("timestamp", "event_Date"), by.y=c("timestamp", "event_Date"), all=TRUE));
}

#combined tables:
merged.data <- merge.tables.on.time(LocationProbe, FitBitApiFeature);


#------------------------------------------------------------------------
# Interpolate the parsed data to estimate missing values
# (probably want to subset the data to interesting columns... but will try to interp all cols first, great IF it works)
# NB! interpolation will error out if there are not at least 2 non-NA values to interpolate in the column, 
# TODO can automate dropping of columns matching this criteria, then could interp everything, but for now just select columns of interest.
# table: dataframe PR table
# cols: set of columns of interest, defaults to columns which with values
# that are not all NA
#------------------------------------------------------------------------
interp.data <- function(table, cols=NULL)
{
    #some processing req.
       
    if(!is.null(cols))
    {
        #option a) provide a set of columns from the table on which to interpolate
        table <- table[, cols];
    }else
    { 
        #option b) should really validate before interpolate, i.e. only merge columns where there is at least 2 real values, and generate warning
        interpable.cols <- apply(table, 2, function(x){ sum(is.na(x)) >= length(x)-2 } );
        table <- table[, interpable.cols];
    }


    dz <- zoo(table);
    index(dz) <- dz[, 1];

    dz <- as.data.frame(na.approx(dz), stringsAsFactors=FALSE) #somewhere in this process timestamp and others are being turned into factors !!! find and kill!!!!  }
    #fix for wierd factor typing in data.frame conversion of zoo type.... think this is a bug.
    dz <- apply(dz, 2, "[")
    return(as.data.frame(dz, stringsAsFactors=FALSE))
}




#------------------------------------------------------------------------
# Smooth the Interpolated data, window=5
#------------------------------------------------------------------------
noise.filter <- function(table)
{
    return(runmed(na.exclude(table), 5));
}


#==================== END OF PREPROCESSING FUNCs ========================



#------------------------------------------------------------------------
# Preprocess two tables: sort, add time cols, join, join, interpolate 
#------------------------------------------------------------------------

preprocess.tables <- function(table1, table2, interp.on.columns)
{

    table1 <- order.by.timestamp(table1);
    table1 <- add.timestamp.date(table1);
    table1 <- add.split.date(table1);
    table2 <- order.by.timestamp(table2);
    table2 <- add.timestamp.date(table2);
    table2 <- add.split.date(table2);
    tables.m <- merge.tables.on.time(table1, table2);
    tables.mi <- NULL
    if(!is.null(interp.on.columns))
    {
        tables.mi <- interp.data(tables.m, cols=interp.on.columns);
    }else
    {
        tables.mi <- interp.data(tables.m);
    }
    #    tables.mif <- noise.filter(tables.mi);  #TODO
    return(tables.mi);
}



#------------------------------------------------------------------------
# Stub code: Run a selection of preprocessing functions
# running 2 tables at a time, so if you want more than 2 tables recycle 
# the output of this with additional ones recursively
#------------------------------------------------------------------------
#some tables from the list of tables (tables) created in getTable.R

#e.g. ** currently not all the talbes will pass the interpolation step, prob. want to subset out the cols that very sparsely populated.
# cols <- c("timestamp", "LATITUDE", "LIGHTLY_ACTIVE_MINUTES", "ACCURACY", "SPEED", "FAIRLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "SEDENTARY_MINUTES", "VERY_ACTIVE_MINUTES", "VERY_ACTIVE_MINUTES", "SLEEP_MEASUREMENTS_DT_DURATION", "event_Hour.y")
# active.data <- preprocess.tables(tables$FitBitApiFeature, tables$LocationProbe, c);


