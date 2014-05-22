#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
# Interface with the PR database for R
#------------------------------------------------------------------------

require("RODBC");
require("zoo");


#------------------------------------------------------------------------
# Create a connection to a user database in the PR Warehouse 
#------------------------------------------------------------------------
args <- commandArgs();

server_url <- args[1]; 
username <- args[2];
password <- args[3];

con <- odbcConnect(dsn=server_url, uid = username, pwd = password);

#------------------------------------------------------------------------
# Get table(s) from the PR Warehouse user database
#------------------------------------------------------------------------
tables <- sqlTables(con); # can iterate over these if neccessary, or specify a list # TODO

#for the time being just select specific ones:
AccelerometerProbe <- sqlQuery(sh, paste("SELECT * FROM AccelerometerProbe");
FitBitApiFeature <- sqlQuery(sh, paste("SELECT * FROM FitBitApiFeature");
LocationProbe <- sqlQuery(sh, paste("SELECT * FROM LocationProbe");
ScreenProbe <- sqlQuery(sh, paste("SELECT * FROM ScreenProbe");
TemperatureProbe <- sqlQuery(sh, paste("SELECT * FROM TemperatureProbe");
DeviceInUseFeature <- sqlQuery(sh, paste("SELECT * FROM DeviceInUseFeature");
LightProbe <- sqlQuery(sh, paste("SELECT * FROM LightProbe");
RobotHealthProbe <- sqlQuery(sh, paste("SELECT * FROM RobotHealthProbe");
SunriseSunsetFeature <- sqlQuery(sh, paste("SELECT * FROM SunriseSunsetFeature");
WeatherUndergroundFeature <- sqlQuery(sh, paste("SELECT * FROM WeatherUndergroundFeature");
GyroscopeProbe <- sqlQuery(sh, paste("SELECT * FROM GyroscopeProbe");


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






