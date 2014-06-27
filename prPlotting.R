#########################################################################
# -- Author: Amos Folarin                                               #
# -- Organisation: KCL/SLaM                                             #
# -- Email: amosfolarin@gmail.com                                       #
#########################################################################


#------------------------------------------------------------------------
#
#------------------------------------------------------------------------


#------------------------------------------------------------------------
# Scale data.
#------------------------------------------------------------------------



#------------------------------------------------------------------------
# Plot all or selected list of variables for all time on one plot
#------------------------------------------------------------------------
# TODO


#------------------------------------------------------------------------
## Plot a single 24hr slice per plot
#loc.ts <- split(location, location$"event_Date");
#plot_loc_slice(loc.ts[[2]]);
#lapply(loc.ts, plot_loc_slice);
#------------------------------------------------------------------------
plot_loc_slice <- function(x)
{
    # latitude only 24hr slice 
    loc.ll <- x["LATITUDE"]; #grab single col dataframe
    #loc.ll <- scale(loc.ll);
    #loc.ll[loc.ll > 51.5] <- NA;  ### blank points above my home latitude... ** need better way to scale!!!! TODO
    #loc.ll[loc.ll < 51.45] <- NA;  ### blank points below my home latitude... ** need better way to scale!!!! TODO
    x11(width=18, height=9);
    loc.nc<- ncol(loc.ll);
    #matplot(location$timestamp, loc.ll, log="y", pch=1:loc.nc, col=1:loc.nc);
    matplot(x$timestamp, loc.ll, pch=1:loc.nc, col=1:loc.nc, lty=2, main="Purple Robot Latitude", sub="24hr Slice", xlab="event timestamp", ylab="latitude");
    legend("topleft", legend=colnames(loc.ll), inset=.05, pch =1:loc.nc, col =1:loc.nc, cex=0.7);
    abline(v=x$midnight_Hour);
    savePlot(paste("location-24hr-slice_", x$event_Date[1], "_.jpg", sep=""));
}


#------------------------------------------------------------------------
#
#------------------------------------------------------------------------

#------------------------------------------------------------------------
#
#------------------------------------------------------------------------

#------------------------------------------------------------------------
#
#------------------------------------------------------------------------

