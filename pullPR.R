#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
# Interface with the PR database for R
#------------------------------------------------------------------------

#require("RODBC");
require("RPostgreSQL");
require("zoo");


#get Args
args <- commandArgs();

#------------------------------------------------------------------------
# Create a connection to a user database in the PR Warehouse with RODBC
# require odbc driver, and dsn (data source name) defined (see docs)
#------------------------------------------------------------------------
#con <- odbcConnect(dsn="PRWarehouse", uid = username, pwd = password);
#tables <- sqlTables(con); # can iterate over these if neccessary, or specify a list # TODO
#
##for the time being just select specific ones:
#AccelerometerProbe <- sqlQuery(sh, "SELECT * FROM AccelerometerProbe");
#FitBitApiFeature <- sqlQuery(sh, "SELECT * FROM FitBitApiFeature");
#LocationProbe <- sqlQuery(sh, "SELECT * FROM LocationProbe");
#ScreenProbe <- sqlQuery(sh, "SELECT * FROM ScreenProbe");
#TemperatureProbe <- sqlQuery(sh, "SELECT * FROM TemperatureProbe");
#DeviceInUseFeature <- sqlQuery(sh, "SELECT * FROM DeviceInUseFeature");
#LightProbe <- sqlQuery(sh, "SELECT * FROM LightProbe");
#RobotHealthProbe <- sqlQuery(sh, "SELECT * FROM RobotHealthProbe");
#SunriseSunsetFeature <- sqlQuery(sh, "SELECT * FROM SunriseSunsetFeature");
#WeatherUndergroundFeature <- sqlQuery(sh, "SELECT * FROM WeatherUndergroundFeature");
#GyroscopeProbe <- sqlQuery(sh, "SELECT * FROM GyroscopeProbe");
#

#------------------------------------------------------------------------
# Create a connection to a user database in the PR Warehouse RPostgreSQL
#------------------------------------------------------------------------

dbhost <- args[1]; 
port <- args[2];
dbname <- args[3];
dbuser <- args[4];
dbpass <- args[5];

drv <- dbDriver("PostgreSQL");
con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass);

#------------------------------------------------------------------------
# Get table(s) from the PR Warehouse user database
#------------------------------------------------------------------------
tables <- sqlTables(con); # can iterate over these if neccessary, or specify a list # TODO

#for the time being just select specific ones:
rs <- dbSendQuery(con, 'SELECT * FROM "AccelerometerProbe"');
AccelerometerProbe <-fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "FitBitApiFeature"');
FitBitApiFeature <- fetch(rs,n=-1); 

rs <- dbSendQuery(con, 'SELECT * FROM "LocationProbe"');
LocationProbe <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "ScreenProbe"');
ScreenProbe <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "TemperatureProbe"');
TemperatureProbe <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "DeviceInUseFeature"');
DeviceInUseFeature <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "LightProbe"');
LightProbe <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "RobotHealthProbe"');
RobotHealthProbe <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "SunriseSunsetFeature"');
SunriseSunsetFeature <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "WeatherUndergroundFeature"');
WeatherUndergroundFeature <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "GyroscopeProbe"');
GyroscopeProbe <- fetch(rs,n=-1);

#close db connection
dbDisconnect(con);


#------------------------------------------------------------------------
# Order by timestamp
#------------------------------------------------------------------------
order.by.timestamp <- function(table)
{
    table <- table[order(table$"timestamp"), ];
    return(table);
}


#order rows by increasing timestamp; 
AccelerometerProbe <- order.by.timestamp(AccelerometerProbe);
FitBitApiFeature <- order.by.timestamp(FitBitApiFeature);
LocationProbe <- order.by.timestamp(LocationProbe);
ScreenProbe <- order.by.timestamp(ScreenProbe);
TemperatureProbe <- order.by.timestamp(TemperatureProbe);
DeviceInUseFeature <- order.by.timestamp(DeviceInUseFeature);
LightProbe <- order.by.timestamp(LightProbe);
RobotHealthProbe <- order.by.timestamp(RobotHealthProbe);
SunriseSunsetFeature <- order.by.timestamp(SunriseSunsetFeature);
WeatherUndergroundFeature <- order.by.timestamp(WeatherUndergroundFeature);
GyroscopeProbe <- order.by.timestamp(GyroscopeProbe);



#------------------------------------------------------------------------
# Append columns for date and time values 
#------------------------------------------------------------------------

# Add separate date/time information and mark the midnight hour timestamp values
add.split.date <- function(table)
{
    table$"event_Date" <- sapply(strsplit(table$eventDateTime, "[ :]"), "[", 1);
    table$"event_Hour" <- sapply(strsplit(table$eventDateTime, "[ :]"), "[", 2); 
    table$"event_Min" <- sapply(strsplit(table$eventDateTime, "[ :]"), "[", 3); 
    table$"event_Sec" <- sapply(strsplit(table$eventDateTime, "[ :]"), "[", 4); 
    table$"midnight_Hour"[table$event_Hour == "00"] <- table$timestamp[table$event_Hour == "00"];

    return(table);
}

# Merge 
AccelerometerProbe <- add.split.date(AccelerometerProbe);
FitBitApiFeature <- add.split.date(FitBitApiFeature);
LocationProbe <- add.split.date(LocationProbe);
ScreenProbe <- add.split.date(ScreenProbe);
TemperatureProbe <- add.split.date(TemperatureProbe);
DeviceInUseFeature <- add.split.date(DeviceInUseFeature);
LightProbe <- add.split.date(LightProbe);
RobotHealthProbe <- add.split.date(RobotHealthProbe);
SunriseSunsetFeature <- add.split.date(SunriseSunsetFeature);
WeatherUndergroundFeature <- add.split.date(WeatherUndergroundFeature);
GyroscopeProbe <- add.split.date(GyroscopeProbe);


#------------------------------------------------------------------------
# Merge the parsed data 
#------------------------------------------------------------------------
merge.tables.on.time <- function(table1, table2)
{
    return(merge(table1, table2, by.x=c("timestamp", "event_Date"), by.y=c("timestamp", "event_Date"), all=TRUE));
}

#combined tables:
merged.data <- merge.tables.on.time(LocationProbe, FitBitApiFeature);
# and similarly other tables can be combined
# merged.data <- merge.tables.on.time(merged.data, AccelerometerProbe);
# merged.data <- merge.tables.on.time(merged.data, TemperatureProbe);
# merged.data <- merge.tables.on.time(merged.data, ScreenProbe);
# merged.data <- merge.tables.on.time(merged.data, DeviceInUseFeature);
# merged.data <- merge.tables.on.time(merged.data, LightProbe);
# merged.data <- merge.tables.on.time(merged.data, SunriseSunsetFeature);
# merged.data <- merge.tables.on.time(merged.data, WeatherUndergroundFeature);
# merged.data <- merge.tables.on.time(merged.data, GyroscopeProbe);

#so probably have a garguantuan table, but likelyhood is we will only want a subset of probes & columns... so economise when this is clearer

#------------------------------------------------------------------------
# Interpolate the parsed data to estimate missing values
# (probably want to subset the data to interesting columns... but will try to interp all cols first, great IF it works)
#------------------------------------------------------------------------
interp.data <- function(table)
{
    dz <- zoo(table);
    index(dz) <- dz[, 1];
    return(na.approx(dz));
}

merged.data.interp <- interp.data(merged.data);

#------------------------------------------------------------------------
# Smooth the Interpolated data
#------------------------------------------------------------------------
merged.data.smo <- runmed(na.exclude(merged.data.interp), 5);






