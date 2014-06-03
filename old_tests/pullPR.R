#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
# Interface with the PR database for R
#------------------------------------------------------------------------

#require("RODBC");
require("RPostgreSQL");  #docs https://code.google.com/p/rpostgresql/
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

dbGetInfo(con);

#------------------------------------------------------------------------
# Get table(s) from the PR Warehouse user database
#------------------------------------------------------------------------
tables <- dbListTables(con); # can iterate over these if neccessary, or specify a list # TODO

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

#rs <- dbSendQuery(con, 'SELECT * FROM "RobotHealthProbe"');
#RobotHealthProbe <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "SunriseSunsetFeature"');
SunriseSunsetFeature <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "WeatherUndergroundFeature"');
WeatherUndergroundFeature <- fetch(rs,n=-1);

rs <- dbSendQuery(con, 'SELECT * FROM "GyroscopeProbe"');
GyroscopeProbe <- fetch(rs,n=-1);

#close db connection
dbDisconnect(con);
dbUnloadDriver(drv);

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
SunriseSunsetFeature <- order.by.timestamp(SunriseSunsetFeature);
WeatherUndergroundFeature <- order.by.timestamp(WeatherUndergroundFeature);
GyroscopeProbe <- order.by.timestamp(GyroscopeProbe);



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


# add new datetime fields
AccelerometerProbe <- add.timestamp.date(AccelerometerProbe);
FitBitApiFeature <- add.timestamp.date(FitBitApiFeature);
LocationProbe <- add.timestamp.date(LocationProbe);
ScreenProbe <- add.timestamp.date(ScreenProbe);
TemperatureProbe <- add.timestamp.date(TemperatureProbe);
DeviceInUseFeature <- add.timestamp.date(DeviceInUseFeature);
LightProbe <- add.timestamp.date(LightProbe);
SunriseSunsetFeature <- add.timestamp.date(SunriseSunsetFeature);
WeatherUndergroundFeature <- add.timestamp.date(WeatherUndergroundFeature);
GyroscopeProbe <- add.timestamp.date(GyroscopeProbe);



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

# add new datetime fields
AccelerometerProbe <- add.split.date(AccelerometerProbe);
FitBitApiFeature <- add.split.date(FitBitApiFeature);
LocationProbe <- add.split.date(LocationProbe);
ScreenProbe <- add.split.date(ScreenProbe);
TemperatureProbe <- add.split.date(TemperatureProbe);
DeviceInUseFeature <- add.split.date(DeviceInUseFeature);
LightProbe <- add.split.date(LightProbe);
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

# NB! interpolation will error out if there are not at least 2 non-NA values to interpolate in the column, 
# TODO can automate dropping of columns matching this criteria, then could interp everything, but for now just select columns of interest.
#------------------------------------------------------------------------
interp.data <- function(table)
{
    dz <- zoo(table);
    index(dz) <- dz[, 1];
    return(na.approx(dz));
}

# for now select columns of interest from each table merged.... then interpolate these see NB above:
fitbit.coi <- c("VERY_ACTIVE_MINUTES", "FAIRLY_ACTIVE_MINUTES", "LIGHTLY_ACTIVE_MINUTES", "SEDENTARY_MINUTES", "ACTIVITY_CALORIES", "SLEEP_MEASUREMENTS_DT_AWAKENINGS_COUNT", "SLEEP_MEASUREMENTS_DT_AWAKE_COUNT", "SLEEP_MEASUREMENTS_DT_DURATION", "SLEEP_MEASUREMENTS_DT_MINUTES_ASLEEP", "SLEEP_MEASUREMENTS_DT_MINUTES_AWAKE", "SLEEP_MEASUREMENTS_DT_MINUTES_IN_BED_AFTER", "SLEEP_MEASUREMENTS_DT_MINUTES_IN_BED_BEFORE", "SLEEP_MEASUREMENTS_DT_RESTLESS_COUNT", "SLEEP_MEASUREMENTS_DT_TIME_IN_BED", "STEPS");
location.coi <- c("ALTITUDE", "BEARING", "SPEED", "LONGITUDE", "LATITUDE", "ACCURACY");

# interpolate 'em
merged.data.interp <- interp.data(merged.data[, c("timestamp", fitbit.coi, location.coi)]);


#------------------------------------------------------------------------
# Smooth the Interpolated data
#------------------------------------------------------------------------
merged.data.smo <- runmed(na.exclude(merged.data.interp), 5);






