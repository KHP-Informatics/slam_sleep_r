#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


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
port <- args[2];
dbname <- args[3];
dbuser <- args[4];
dbpass <- args[5];

#--- sql query info
#tablenames
tablenames <- args[8:length(args)];
#time range
#all <- TRUE
startpoint <- args[6];
endpoint <- args[7];

#---- a list of tables
tables <- list();

#------------------------------------------------------------------------
# Connect and get table(s) from the PR Warehouse user database
#------------------------------------------------------------------------
#tables <- dbListTables(con); # can iterate over these if neccessary, or specify a list # TODO
drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass);
dbGetInfo(con);



getTables <- function(sql.query, close.con=FALSE)
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
# Stub: read a set of tables from PRW, populating the tables list
#------------------------------------------------------------------------
for(i in tablenames)
{
    #build sql query
    sql.query <- paste('SELECT * FROM "', tablenames[i], '" WHERE timestamp >= ', startpoint, 'AND WHERE timestamp <= ', endpoint, '');
    getTables(sql.query);
    
    if (i == length(tablenames))
    {
         tables[[i]] <- getTables(sql.query, close.con=TRUE);
    }
}


