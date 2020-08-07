#########################################################################
##
#    Program created by Lucy Gunn 30/07/2020
#    For the EDGTA project for the JTH conference
#
#    Program contains:
#
#    1) Lnear Regressions and predictions for chronic diseases 
#
##
#
#########################################################################
# read in the data
#########################################################################


library(Hmisc)   # this has the 'describe' function in it
library(dplyr)
library(stats)
library(ape)
library(lme4)
library(forecast)


LucyDir <- "C:/scratch/Research/DoT_RMIT/BelenData"
myDir <- LucyDir
setwd(myDir)


# Load and check data -----------------------------------------------------
# load the treedata and check that it's loaded correctly
mydata<-read.csv("diabetes_death_under.csv",  header = TRUE)
#str(mydata)
#attach(mydata)

# check that the data is being correctly read in
nrow(mydata)
head(mydata)

# Geta  feel for the data - plot it first

plot(mydata$Year, mydata$Males)
plot(mydata$Year, mydata$Females)

#  appears to be very linear for CVD - therfore data adjustments not required

#mydata_COPDnew <- mydata_copd[6:18,]
#mydata_COPDnew
#head(mydata_COPDnew)        # check the data

# replot to check based on the new dataset:

#plot(mydata_COPDnew$YEAR, mydata_COPDnew$Males)
#plot(mydata_COPDnew$YEAR, mydata_COPDnew$Females)


# RUN THE REGRESSIONS LADIES FIRST:
####################################################################


Females<-lm(mydata$Females~mydata$Year, data=mydata)
summary(Females)


Females$coefficients[1]   # gets the coefficients that we want - this is the intercept
Females$coefficients[2]   # gets the slope term (ie on YEAR)


# create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample

new_YEAR=data.frame(seq(1981,2028,1))   # creates a sequence for YEAR but include an additional 10 years
new_YEAR
predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
predictFem

# note there are some negative values, and these need to be set to zero for those years, according AIHW protocols for negative trend

#predictFem<-replace(predictFem, predictFem<0,0) 
#predictFem

# join data into one dataframe for plotting and exporting purposes

FitFore.Fem<-data.frame(new_YEAR,predictFem)  # combines data into one dataframe so it's easier to plot

# check mins and maxs of data for plotting purposes

min(predictFem)
max(predictFem)
min(mydata$Females)
max(mydata$Females)

# plot predicted regression line with it's new forecasts

xrange <- 2029    # sets the range for the plot for the x variable
yrange <- max(predictFem)      # sets the maximum for the y varieable - you can also use max(predictFem) - also good to check the raw data as well as the predicted and forecasts

plot(xrange, yrange,
     xlim=c(1981,2029),
     ylim=c(min(mydata$Females),max(mydata$Females)),
     type = "n",       # this suppressess the plot points (you are are using the x, y to create the correct plot size)
     ylab ="Rate_Diab_deaths_under_Females",
     xlab ="Year", 
     pch=19,
     cex=1.3)
     #xaxt = "YEAR")
#yaxt = "none")

lines(mydata$Year, mydata$Females, 
      type="o", 
      pch = 19,
      cex = 1.1,
      lty = 1,
      lwd = 2,
      col="red")

lines(FitFore.Fem,
      type = "o",
      pch = 19,
      cex = 1.1,
      lty = 1,
      lwd = 2,
      col = "blue")

mtext("Forecasts for Diab_deaths_under_Females", side = 3, line = 1, cex = 1.2, font=2)


# DO THE MALES...
##################################################################


# run the regression, ladies first:

Male<-lm(mydata$Males~mydata$Year, data=mydata)
summary(Male)


Male$coefficients[1]   # gets the coefficients that we want - this is the intercept
Male$coefficients[2]   # gets the slope term (ie on YEAR)


# create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample

#new_YEAR=data.frame(seq(2005,2027,1))   # creates a sequence for YEAR but include an additional 10 years
#new_YEAR
predictMale=Male$coefficients[1]+Male$coefficients[2]*new_YEAR
predictMale

# note there are some negative values, and these need to be set to zero for those years, according AIHW protocols for negative trend

#predictMale<-replace(predictMale, predictMale<0,0) 
#predictMale


FitFore.Male<-data.frame(new_YEAR,predictMale)  # combines data into one dataframe so it's easier to plot

# check mins and maxs of data for plotting purposes

min(predictMale)
max(predictMale)
min(mydata$Male)
max(mydata$Male)

# plot predicted regression line with it's new forecasts

xrange <- 2029    # sets the range for the plot for the x variable
yrange <- max(mydata$Males)    # sets the maximum for the y variable - you can also use max(predictFem) - also good to check the raw data as well as the predicted and forecasts


plot(xrange, yrange,
     xlim=c(1981,2029),
     ylim=c(15, 23),
     type = "n",       # this suppressess the plot points (you are are using the x, y to create the correct plot size)
     ylab ="Rate_Diab_deaths_under_Males",
     xlab ="Year", 
     pch=19,
     cex=1.3)
#xaxt = "YEAR")
#yaxt = "none")

lines(mydata$Year, mydata$Males, 
      type="o", 
      pch = 19,
      cex = 1.1,
      lty = 1,
      lwd = 2,
      col="red")

lines(FitFore.Male,
      type = "o",
      pch = 19,
      cex = 1.1,
      lty = 1,
      lwd = 2,
      col = "blue")

mtext("Forecasts for Diab_deaths_under_Males", side = 3, line = 1, cex = 1.2, font=2)



# MULTIPLY FORECASTS BY THE ERP 
#  ERP figures obtained from ABS
#  Catalogue 3101.0 Australian Demographic Statistics										
#  TABLE 4. Estimated Resident Population, States and Territories (Number)										
#  June quarter used as the reference for each year
#  ERP data stops at 2019, and the value for 2019 of 25364307 (people) is used for forecast years 2020-2028
################################################################


myERP<-read.csv("ERP.csv",  header = TRUE)

# multiply the forecasts by ERP

# first, get the rates by dividing through by 100,000

RatePF<-predictFem/100000
RatePM<-predictMale/100000

# adjust the ERP file to suit the years 1997 - 2028  (not required on this file it runs from 1981 - 2028)

# myERP <- myERP[17:48,]

PredF_ERP <- (RatePF*myERP$ERP)   # vector by vector element by element multiplication
PredM_ERP <- (RatePM*myERP$ERP)   # vector by vector element by element multiplication


# COMBINE DATA FOR EXPORT
#################################################################################################


Forecasts<-cbind.data.frame(new_YEAR, predictFem, predictMale, PredF_ERP, PredM_ERP, myERP$ERP)
Forecasts                                        #print off to check that it's correct

colnames(Forecasts)        # get the current column names 

# replace with sensible names that match the created forecasts 

#Names<-data.frame("Year", "Female_CVD", "Males_CVD")

names(Forecasts)[1]<- "Year"
names(Forecasts)[2]<- "Female_DiabUnder_F"
names(Forecasts)[3]<- "Male_DiabUnder_F"
names(Forecasts)[4]<- "ERP_rateFem_F"
names(Forecasts)[5]<- "ERP_rateMale_F"
names(Forecasts)[6]<- "ERP"


Forecasts  # print to check

#export out as csv file

write.csv(Forecasts, "C:/scratch/Research/DoT_RMIT/BelenData/ForecastDiabDeathUnder.csv")
