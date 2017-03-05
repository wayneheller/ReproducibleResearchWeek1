################################################################################
# Reproducible Research - John's Hopkins University | Coursera                 #
# Week 1 Optional Course Project                                               #
# Wayne Heller                                                                 #
# 03/04/2017                                                                   #
# Assignment- to create plots for data science research using inpatient        #
# prospective payment system data                                              #
#                                                                              #
#                                                                              #
# Key Assumption: assumes data file payment.csv is in the                      #
# working directory                                                            #
################################################################################

# Loads the data.frame from the source file
# First checks for the existence of the file and prints an error message if
# not found
readPaymentsData <- function() {
    if(file.exists("payments.csv")){
        df <- read.csv("payments.csv", header=TRUE)
        return(df)
    }
    else {
        print("Set Working Directory to location of payments.csv")
        return(FALSE)
    }
}


# See descriptions of individual plots below
createPlots2 <- function() {
    
    # Read in the dataset
    dfPayments <- readPaymentsData()
    
    # check for error: data file not found
    if(class(dfPayments)!="data.frame") {
        stop()
    }
    
    # Plot 1:
    # Make a plot that answers the question: what is the relationship 
    # between mean covered charges (Average.Covered.Charges) and 
    # mean total payments (Average.Total.Payments) in New York?
    
    # subset on New York
    dfNYPayments <- dfPayments[dfPayments$Provider.State == "NY", ]
    
    # create x, y vectors scaled to $000's
    avgTotalPayments <- log10(dfNYPayments$Average.Total.Payments)
    avgCoveredCharges <- log10(dfNYPayments$Average.Covered.Charges)
    
    # close any open graphics devices
    # this clears out any existing settings
    while(length(dev.list()>1)) {
        dev.off()
    }
    
    # create PNG plotting device
    devPDF <- pdf("plot1_.pdf")
    
    par(las=1, mar=c(4,4,3,3)) # Set Y axis label to print horizontally
    
    # Plot Average.Covered.Charges by Average.Total.Payments
    plot(avgTotalPayments, avgCoveredCharges, 
         pch=20, type = "p", xlab="Log10 Mean Total Payments $'s", ylab="Log10 Mean Covered Charges $'s")
    
    # Plot linear regression line
    abline(lm(avgCoveredCharges  ~ avgTotalPayments ), lwd = 2, col="red")
    
    # Annotate with correlation coefficient
    subtitleText = paste("The correlation coefficient is: " , 
                         as.character(round(cor(avgCoveredCharges, avgTotalPayments),digits=2)))
    text(x=3.8, y=5, labels = subtitleText)
    
    # Add main title
    title(main = "Mean Covered Charges Increase with Mean Total Payments in New York")
    
    # Close the pdf file
    dev.off()
    
    # Plot 2:
    # Make a plot (possibly multi-panel) that answers the question: 
    # How does the relationship between mean covered charges (Average.Covered.Charges)
    # and mean total payments (Average.Total.Payments) vary by 
    # medical condition (DRG.Definition) and the state in which care was received
    # (Provider.State)?"
    
    # Create vectors for the unique states and DRG Defintions
    # There are 6 of each which means we are creating a 6 x 6 matrix of plots
    states = unique(dfPayments$Provider.State)
    DRG = unique(dfPayments$DRG.Definition)
    
    # We will want the axis of each to be uniform to the range of x and y
    xRange = range(log10(dfPayments$Average.Total.Payments))
    yRange = range(log10(dfPayments$Average.Covered.Charges))
    # Setting to 3.4 to make the plots look clean; 
    # otherwise there will be a gap between y and x axis
    xRange[1] <-3.4
    
    # open the pdf device, increase the size given that we are making 36 plots
    devPDF <- pdf("plot2_.pdf", width = 8, height  = 8)
    
    # Set the layout to 6 x 6, set internal and outer margins
    par(mfrow=c(length(states), length(DRG)), mar=c(0,0,0,1), oma=c(11,4,5,3))
    
    # Loop through all 6 states and all 6 DRG Definitions
    for(rowIdx in 1:length(states)){
        for(colIdx in 1:length(DRG)){
            # filter the data based on row and column
            dfPlot <- dfPayments[(dfPayments$Provider.State == states[rowIdx] &
                                      dfPayments$DRG.Definition == DRG[colIdx]), ]
            
            # create vectors for x, y
            avgTotalPayments <- log10(dfPlot$Average.Total.Payments)
            avgCoveredCharges <- log10(dfPlot$Average.Covered.Charges)
            
            # make scatter plot, each DRG Definition will be a different color
            # surpress the printing of the axes, these will be handled individually
            plot(avgTotalPayments, avgCoveredCharges, col=dfPlot$DRG.Definition,
                 pch=20, type = "p", xlab="", ylab="",
                 xlim = xRange, ylim = yRange, axes = FALSE)
            
            # Plot linear regression line
            linreg = lm(avgCoveredCharges  ~ avgTotalPayments )
            abline(a = linreg[1], b= linreg[2], lwd = 1, col="brown")
            
            # annotate each plot with the state abbreviation and the DRG code
            text(x=6, y=190, labels = paste("State: ", states[rowIdx]), cex = .75)
            text(x=6, y=155, labels = paste("DRG: ", substr(DRG[colIdx], 1, 3)), cex = .75)
            
            # The following block of code formats the axes for each plot
            # The default behavior is to print the axes on each plot, but that
            # was suppressed above
            # Column 1 - Print the vertical axis with labels and tick marks on the left
            # Column 6 - Print the vertial axis with labels and tick marks on the right
            # Columns 2-5 Print a vertial axis without labels and tick marks
            if(colIdx != 1) {
                if(colIdx == length(DRG)) {
                    axis(side = 4, labels = TRUE, lwd.ticks =  1)
                }
                
                axis(side = 2, labels = FALSE, lwd.ticks =  0)
            }
            else {
                axis(side = 2, labels = TRUE, lwd.ticks =  1)
            }
            # Row 1 - Print the horizontal axis with labels and tick marks on the top
            # Row 6 - Print the horizontal axis with labels and tick marks on the bottom
            # Rows  2-5 Print a horizontal axis without labels and tick marks
            if(rowIdx != length(states)) {
                if(rowIdx == 1) {
                    axis(side = 3, labels = TRUE, lwd.ticks =  1)
                }
                
                axis(side = 1, labels = FALSE, lwd.ticks =  0)
            }
            else {
                axis(side = 1, labels = TRUE, lwd.ticks =  1)
            }
        }
    }
    # Annotate the Title, Both Axes, Add the Legend
    mtext(text="Mean Covered Charges vs. Mean Total Payments By DRG Definition and Providing State",side=3,cex=1,outer=TRUE,line=3)
    mtext(text="Log10 Mean Total Payments $'s",side=1,cex=1,outer=TRUE,line=3)
    mtext(text="Log10 Mean Covered Charges $'s",side=2,cex=1,outer=TRUE,line=2)
    legend(x=-4.0, y=2.4, legend=DRG, fill=DRG, xpd=NA, bty="n", ncol = 1)
    
    dev.off()
}

