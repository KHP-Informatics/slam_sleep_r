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

#get Args
args <- commandArgs();


#------------------------------------------------------------------------
# Create a connection to a user database in the PR Warehouse RPostgreSQL
#------------------------------------------------------------------------

#--- db connect info
dbhost <- args[1]; 
dbport <- args[2];
dbname <- args[3];
dbuser <- args[4];
dbpass <- args[5];

#--- sql query info
#all <- TRUE
startpoint <- args[6];
endpoint <- args[7];
#tablenames
tablenames <- args[8:length(args)];
#time range


#------------------------------------------------------------------------
# Connect and get table(s) from the PR Warehouse user database
#------------------------------------------------------------------------
#tables <- dbListTables(con); # can iterate over these if neccessary, or specify a list # TODO
drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass);
dbGetInfo(con);


#------------------------------------------------------------------------
# get a table from PRW with for a given SQL query
# returns: a dataframe with all results returned
#------------------------------------------------------------------------
getTable <- function(sql.query="", close.con=FALSE)
{
    #for the time being just select specific ones:
    rs <- dbSendQuery(con,sql.query);
    table <- fetch(rs, n=-1); #always return full result set 

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
#------------------------------------------------------------------------
getTables <- function(tablenames, time.range="all.time")
{
    tables <- list();
    for(i in tablenames)
    {
        sql.query <- "";
        
        if(time.range=="all.time")
        {
            #default: return all records in table
            sql.query <- paste('SELECT * FROM "', i, '"', sep="");
        }

        if(time.range=="from.last")
        {
            #only specify start time, i.e. get everything from the startime on...
            sql.query <- paste('SELECT * FROM "', i, '" WHERE timestamp >= ', startpoint,'', sep="");
        }

        if(time.range=="from.to")
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

#e.g.# myTables <- getTables(tablenames, time.range="from.last")


# The data coming out is getting eventDateTime and insertDateTime listed as POSIXct, this can be fished out and stringified if necessary
# as R can whinge a bit about POSIX types in dataframes
# so
# posixct.cols <- (sapply(sapply(myTables, class), "[", 1)) %in% "POSIXct"
# myTables[posixct.cols] <-lapply(myTables[posixct.ct], as.character) 






