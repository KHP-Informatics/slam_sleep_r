#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################

source("./getPRTables.R");
source("./preprocessPRTables.R");

#------------------------------------------------------------------------
# ************** MAIN ENTRY POINT FOR SLEEP SERVLET *********************
# 
#------------------------------------------------------------------------



#------------------------------------------------------------------------
# Get a list of dataframes which hold the tables for the current timeseries
# 1) get the relevant tables (as specified on command line args), see
# getPRTAbles.R
#------------------------------------------------------------------------
myTables <- getTables(tablenames, time.range="from.last")



#------------------------------------------------------------------------
# Run preprocessing functions on the tables
#------------------------------------------------------------------------
active.data <- preprocess.tables(myTables$FitBitApiFeature, myTables$LocationProbe);



#------------------------------------------------------------------------
# Extract sleep data 1) sleep/awake classification 2) fitbit restless values
# Parse to XML for injestion by MyHealthLocker
#------------------------------------------------------------------------
lm.fit <- loadRDS("lm_test_mod.rds");

# The current model: SLEEP_MEASUREMENTS_DT_DURATION ~ 0 + LATITUDE + LIGHTLY_ACTIVE_MINUTES
sleep.wake <- predict.lm(fit, data=active.data[, c("LATITUDE", "LIGHTLY_ACTIVE_MINUTES")]);

# Extract the fitbit restless data




#------------------------------------------------------------------------
# convert the predicted sleep datafram + restless results to XML 
#------------------------------------------------------------------------
