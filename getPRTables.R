#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################

#USAGE:
# RScript getPRTables.R db2.cbits.northwestern.edu 5432 prw_database prw_usrname prw_passwoed 1397085335 1398326606 FitBitApiFeature LocationProbe  

#------------------------------------------------------------------------
# Interface with the Purple Robot Warehouse (PRW) database for R
# Extract the required set of data for a chunk of sleep status to 
# forward on to MyHealthLocker/HealthVault
#------------------------------------------------------------------------

#require("RODBC");
require("RPostgreSQL");  #docs https://code.google.com/p/rpostgresql/



#------------------------------------------------------------------------
# global variables
#------------------------------------------------------------------------

dbhost <- NULL;
dbport <- NULL;
dbname <- NULL;
dbuser <- NULL;
dbpass <- NULL;
startpoint  <- NULL;         
endpoint <- NULL; 
tablenames <- NULL; 
startpoint  <- NULL
endpoint <- NULL
tablenames <- NULL
#db connection
con <- NULL

#------------------------------------------------------------------------
#Initialize from Rscript CLI args alternatively init in console
# ARG args: array of args to initialize with or CLI args from RScript (default)
# EXAMPLE:
#   initArgs() # use commandArgs()
#   initArgs(args)  #specify a commandArgs() vector
#------------------------------------------------------------------------
initArgs <- function(args=commandArgs())
{
    #get Args
    #args <- commandArgs();

    #--- db connect info
    dbhost <<- args[2]; 
    dbport <<- args[3];
    dbname <<- args[4];
    dbuser <<- args[5];
    dbpass <<- args[6];

    #--- sql query info
    #all <- TRUE
    startpoint <<- args[7];
    endpoint <<- args[8];
    #tablenames
    tablenames <<- args[9:length(args)];
    #time range
}

#------------------------------------------------------------------------
# Connect and get table(s) from the PR Warehouse user database
#------------------------------------------------------------------------
makeDBConnection <- function()
{
    #tables <- dbListTables(con); # can iterate over these as alt. to tablenames, or specify a list # TODO
    drv <- dbDriver("PostgreSQL");
    con <<- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass);
    dbGetInfo(con);
}

#------------------------------------------------------------------------
# get a table from PRW with for a given SQL query
# returns: a dataframe with all results returned
# ARG1 sql.query: the SQL statement to select the PR table
# ARG2 date2char: convert POSIXct dates columns to character class, default=TRUE
# ARG3 close.con should the connection be closed after running? default=FALSE
# RETURN: dataframe of PR table
#------------------------------------------------------------------------
getTable <- function(sql.query="", date2char=TRUE, close.con=FALSE)
{
    #for the time being just select specific ones:
    rs <- dbSendQuery(con,sql.query);
    table <- fetch(rs, n=-1); #always return full result set 

    #convert POSIXct to char if req. 
    if(date2char==TRUE)
    {
        posixct.cols <- (sapply(sapply(table, class), "[", 1)) %in% "POSIXct"
        table[posixct.cols] <-lapply(table[posixct.cols], as.character)
        table <- as.data.frame(table, stringsAsFactors=FALSE)

    }else
    {
        table <- as.data.frame(table, stringsAsFactors=FALSE)
    }


    #close db connection after query?
    if(close.con == TRUE)
    {
        dbDisconnect(con);
        dbUnloadDriver(drv);
    }

    return(table);
}


#------------------------------------------------------------------------
# Read a set of tables from PRW from a given time range.
# returns: list of dataframes (each table in tablenames) with records 
# from time range
# ARG1 tablenames: a list or character vector of PR tablenames
# ARG2 time.range: specify the range of timestamps which to select rows
#                  a numeric vector: c(from) to latest implied,
#                  or c(from, to), if null then default to all rows
# EXAMPLE:
# initArgs()
# makeDBConnection()
# myTables <- getTables(tablenames)
#------------------------------------------------------------------------
#getTables <- function(tablenames, time.range="all.time")
getTables <- function(tablenames, time.range=NULL)
{
    tables <- list();
    for(i in tablenames)
    {
        sql.query <- "";
        
        if(is.null(time.range))
        {
            #default: return all records in table
            sql.query <- paste('SELECT * FROM "', i, '"', sep="");
        }

        if(length(time.range)==1)
        {
            #only specify start time, i.e. get everything from the startime on...
            sql.query <- paste('SELECT * FROM "', i, '" WHERE timestamp >= ', startpoint,'', sep="");
        }

        if(length(time.range)==2) #from-to
        {
            #build sql query
            sql.query <- paste('SELECT * FROM "', i, '" WHERE timestamp >= ', startpoint, ' AND timestamp <= ', endpoint, '', sep="");
            
        }

        #if (i == tablenames[length(tablenames)]) #on the last table close
        #{
        #    tables[[i]] <- getTable(sql.query, close.con=TRUE);
        #}
        
        tables[[i]] <- getTable(sql.query);
    }
    
    return(tables);
}




#(re below: I've moved a fix for this, you can optionally specify date2char in getTable() )
# The data coming out is getting eventDateTime and insertDateTime listed as POSIXct, this can be fished out and stringified if necessary
# as R can whinge a bit about POSIX types in dataframes
# so
# posixct.cols <- (sapply(sapply(myTables, class), "[", 1)) %in% "POSIXct"
# myTables[posixct.cols] <-lapply(myTables[posixct.ct], as.character) 






