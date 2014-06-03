#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
# Extract sleep data 1) sleep/awake classification 2) fitbit restless values
# Parse to XML for injestion by MyHealthLocker
#------------------------------------------------------------------------

# The current model: SLEEP_MEASUREMENTS_DT_DURATION ~ 0 + LATITUDE + LIGHTLY_ACTIVE_MINUTES
sleep.wake <- predict(fit, data=active.data[, c(LATITUDE, LIGHTLY_ACTIVE_MINUTES)]);

# Extract the fitbit restless data




#------------------------------------------------------------------------
# convert the predicted sleep datafram + restless results to XML 
#------------------------------------------------------------------------
