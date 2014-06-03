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
#------------------------------------------------------------------------
interp.data <- function(table, cols="")
{
    #some processing req.
    #option a) should really validate before interpolate, i.e. only merge columns where there is at least 2 real values, and generate warning
    interpable.cols <- apply(table, 2, function(x){ sum(is.na(x)) >= 2 } );
    #option b) provide a set of columns from the table on which to interpolate
    if(cols != "")
    {
        table <- table[, cols];
    }

    dz <- zoo(table);
    index(dz) <- dz[, 1];
    return(na.approx(dz));
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

preprocess.tables <- function(table1, table2)
{
    table1 <- order.by.timestamp(table1);
    table1 <- add.timestamp.date(table1);
    table1 <- add.split.date(table1);
    table2 <- order.by.timestamp(table2);
    table2 <- add.timestamp.date(table2);
    table2 <- add.split.date(table2);
    tables.m <- merge.tables.on.time(table1, table2);
    tables.mi <- interp.data(tables.m);
#   tables.mif <- noise.filter(tables.mi);
    return(table.mi);
}



#------------------------------------------------------------------------
# Stub code: Run a selection of preprocessing functions
# running 2 tables at a time, so if you want more than 2 tables recycle 
# the output of this with additional ones recursively
#------------------------------------------------------------------------
#some tables from the list of tables (tables) created in getTable.R

active.data <- preprocess.tables(tables$FitBitApiFeature, tables$LocationProbe);

