#########################################################################
##
#    Program created by Lucy Gunn 30/07/2020
#    For the DOT-RMIT-HIA project 
#
#    Program contains:
#
#    1) Lnear Regressions and predictions for COPD 
#
#
##
#########################################################################
# read in the data
#########################################################################


suppressPackageStartupMessages(library(Hmisc))     # this has the 'describe' function in it
suppressPackageStartupMessages(library(dplyr))     # for manipulating data
suppressPackageStartupMessages(library(stats))     #
suppressPackageStartupMessages(library(ape))       #
suppressPackageStartupMessages(library(lme4))      #
suppressPackageStartupMessages(library(forecast))  #

### for copd use mortlaity grim books
grim_books="Data/aihw/trends/grim_books.csv"


# Load and check data -----------------------------------------------------

mydata <- read.csv(grim_books, as.is=T) %>%
   filter(AGE_GROUP == "Total", SEX != "Persons", YEAR >= 2000,
          cause_of_death == "Chronic obstructive pulmonary disease (COPD) (ICD-10 J40–J44)") %>%
   select(YEAR, SEX, age_standardised_rate) %>%
   pivot_wider(id_cols = c(YEAR, SEX, age_standardised_rate), 
               values_from = c(age_standardised_rate), names_from = c(SEX))

# check that the data is being correctly read in
nrow(mydata)
head(mydata)

# Geta  feel for the data - plot it first

plot(mydata$YEAR, mydata$Males)
plot(mydata$YEAR, mydata$Females)

#  the first 5 years seema  bit wacky, remove first 5 observations (check with Belen on this)

mydatanew <- mydata[6:18,]
mydatanew
head(mydatanew)        # check the data

# replot to check based on the new dataset:

plot(mydatanew$YEAR, mydatanew$Males)
plot(mydatanew$YEAR, mydatanew$Females)


# RUN THE REGRESSIONS LADIES FIRST:
####################################################################


Females<-lm(mydatanew$Females~mydatanew$YEAR, data=mydatanew)
summary(Females)


Females$coefficients[1]   # gets the coefficients that we want - this is the intercept
Females$coefficients[2]   # gets the slope term (ie on YEAR)


# create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample

new_YEAR=data.frame(seq(2005,2027,1))   # creates a sequence for YEAR but include an additional 10 years
new_YEAR
predictFem=Females$coefficients[1]+Females$coefficients[2]*new_YEAR
predictFem

FitFore.Fem<-data.frame(new_YEAR,predictFem)  # combines data into one dataframe so it's easier to plot


# check mins and maxs of data for plotting purposes

min(predictFem)
max(predictFem)
min(mydata$Fem)
max(mydata$Fem)


# plot predicted regression line with it's new forecasts

xrange <- 2029    # sets the range for the plot for the x variable
yrange <- max(predictFem)      # sets the maximum for the y varieable - you can also use max(predictFem) - also good to check the raw data as well as the predicted and forecasts


plot(xrange, yrange,
     xlim=c(2005,2029),
     ylim=c(min(predictFem),max(predictFem)),
     type = "n",       # this suppressess the plot points (you are are using the x, y to create the correct plot size)
     ylab ="Rate_COPD_Females",
     xlab ="Year", 
     pch=19,
     cex=1.3)
     #xaxt = "YEAR")
#yaxt = "none")

lines(mydatanew$YEAR, mydatanew$Females, 
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

mtext("Forecasts for COPD_Females", side = 3, line = 1, cex = 1.2, font=2)


trends_copd_f=(log(predictFem[23,]/predictFem[14,]))/(new_YEAR[23,] - new_YEAR[14,])


# DO THE MALES...
##################################################################


# run the regression, males now:

Male<-lm(mydatanew$Males~mydatanew$YEAR, data=mydatanew)
summary(Male)

Male$coefficients[1]   # gets the coefficients that we want - this is the intercept
Male$coefficients[2]   # gets the slope term (ie on YEAR)


# create the time trend sequence for use in the model to get predicted values in sampel and forecast for out of sample

#new_YEAR=data.frame(seq(2005,2027,1))   # creates a sequence for YEAR but include an additional 10 years
#new_YEAR
predictMale=Male$coefficients[1]+Male$coefficients[2]*new_YEAR
predictMale
FitFore.Male<-data.frame(new_YEAR,predictMale)  # combines data into one dataframe so it's easier to plot

# check mins and maxs of data for plotting purposes

min(predictMale)
max(predictMale)
min(mydata$Males)
max(mydata$Males)

# plot predicted regression line with it's new forecasts

xrange <- 2029    # sets the range for the plot for the x variable
yrange <- max(mydata$Males)    # sets the maximum for the y variable - you can also use max(predictFem) - also good to check the raw data as well as the predicted and forecasts


plot(xrange, yrange,
     xlim=c(2005,2029),
     ylim=c(27,max(mydata$Males)),
     type = "n",       # this suppressess the plot points (you are are using the x, y to create the correct plot size)
     ylab ="Rate_COPD_Males",
     xlab ="Year", 
     pch=19,
     cex=1.3)
#xaxt = "YEAR")
#yaxt = "none")

lines(mydatanew$YEAR, mydatanew$Males, 
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

mtext("Forecasts for COPD_Males", side = 3, line = 1, cex = 1.2, font=2)

trends_copd_m=(log(predictMale[23,]/predictMale[14,]))/(new_YEAR[23,] - new_YEAR[14,])

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

# adjust the ERP file to suit the years  2005 - 2027

myERP <- myERP[25:47,]

PredF_ERP <- (RatePF*myERP$ERP)   # vector by vector element by element multiplication
PredM_ERP <- (RatePM*myERP$ERP)   # vector by vector element by element multiplication


# COMBINE DATA FOR EXPORT
#################################################################################################


Forecasts<-cbind.data.frame(new_YEAR, predictFem, predictMale, PredF_ERP, PredM_ERP, myERP$ERP)
Forecasts                                        #print off to check that it's correct

colnames(Forecasts)        # get the current column names 

# replace with sensible names that match the created forecasts 

# Names<-data.frame("Year", "Female_CVD", "Males_CVD")

names(Forecasts)[1]<- "Year"
names(Forecasts)[2]<- "Female_COPD_F"
names(Forecasts)[3]<- "Male_COPD_F"
names(Forecasts)[4]<- "ERP_rateFem_F"
names(Forecasts)[5]<- "ERP_rateMale_F"
names(Forecasts)[6]<- "ERP"


Forecasts  # print to check

#export out as csv file

write.csv(Forecasts, "C:/scratch/Research/DoT_RMIT/BelenData/ForecastCOPD.csv")


