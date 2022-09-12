#Data generations

Time <- seq.POSIXt(as.POSIXct("2020-01-01 0:00:00", format = "%Y-%m-%d %H:%M:%S"), as.POSIXct("2022-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"), by = "15 min")
Temperature <- ((15 * cos(2 * pi * (as.numeric(Time) / (365.26 * 24 * 60 * 60)) + pi)) + 10) + (5 * cos(2 * pi * (as.numeric(Time) / (24 * 60 * 60)) + pi)) + rnorm(length(Time))
Data_Frame_1 <- data.frame(Time, Temperature)

Date <- seq.Date(as.Date("2020-01-01", format = "%Y-%m-%d"), as.Date("2022-01-01", format = "%Y-%m-%d"), by = "1 day")
Temperature <- ((15 * cos(2 * pi * (as.numeric(Date) / 365.26) + pi)) + 10) + rnorm(length(Date), 0, 5)
Data_Frame_2 <- data.frame(Date, Temperature)

Orientation <- rep((360 / 8) * seq_len(8), each = 5)
Tree_Number <- rep(1:5, length.out = length(Orientation))
Moss_Biomass <- c(abs(rnorm(5, 8, 2)), abs(rnorm(5, 15, 2)), abs(rnorm(5, 19, 2)), abs(rnorm(5, 20, 2)), abs(rnorm(5, 17, 2)), abs(rnorm(5, 14, 2)), abs(rnorm(5, 11, 2)), abs(rnorm(5, 10, 2)))
Data_Frame_5 <- data.frame(Tree_Number, Orientation, Moss_Biomass)
Data_Frame_5 <- Data_Frame_5[order(Data_Frame_5$Tree_Number), ]
rownames(Data_Frame_5) <- NULL

#Uzed in cyclical info i.e. julian day 0 = julian day 365, same with compas orientations

#Fake cyclical functions. Our data won't have period of 2pi or amplitude of 1. Have 4 params we must alter to do transformations:

#Phase shift: How much it shifts right and left. Think cos is a pi/2 phase shift from sin. Add inside trig fxn and add to predictor vairable sin(theta - phase shift). Note: negative phase shift will shift to the right. Positive will shift to the left. 2pi shift results in the same graph

#Periods: Multiply period change inside trig fxn sin(theta * 2) two periods within one of original

#Amplitudes: Change how high/low graph gets. Negative ampliteds relfect across x axis. Multiply outside of trig 2 * sin(theta)

#Intercept: Add or subtract number to say which val oscillates around sin(theta) +1 



#Now how to use
plot(Temperature ~ Date, data = Data_Frame_2, main = "Temperature data")
#We know the period is 365 days, find amp, phase shift, and intercept

#use nls function (non-linear least squares). Another way is to fit the Bayesian way

Data_Frame_2$Date_in_Numeric_Form <- as.numeric(Data_Frame_2$Date)#Need to read in numbers. Other ways to do this

Model_1 <- nls(Temperature ~ Amplitude * sin((2 * pi / 365.26) * (Date_in_Numeric_Form + Phase_Shift)) + Intercept, Data_Frame_2, start = c(Amplitude = 5, Phase_Shift = 10, Intercept = 5))

summary(Data_Frame_2)
coef(Model_1)

Amplitude_1 <- coef(Model_1)["Amplitude"]
Phase_Shift_1 <- coef(Model_1)["Phase_Shift"]
Intercept_1 <- coef(Model_1)["Intercept"]


Data_Frame_2$Predicted_Temperature <- Amplitude_1 * sin((2 * pi/365.26) * (Data_Frame_2$Date_in_Numeric_Form + Phase_Shift_1)) + Intercept_1

plot(Temperature ~ Date, data = Data_Frame_2)
with(Data_Frame_2, lines(Date, Predicted_Temperature, col = 2, lwd = 4))

     
Data_Frame_2$Transformed_Values <- (Data_Frame_2$Temperature - Intercept_1) / Amplitude_1
range(Data_Frame_2$Transformed_Values)


#Dataframe 1 has two different cyclical things happening here: daily temp changes and temp changes over a year

#HAVE TO DO THE BIGGER PERIOD FIRST. I.E. YEARLY TEMP CHANGES

Data_Frame_1$Time_in_Numeric_Form <- as.numeric(Data_Frame_1$Time)
Model_2 <- nls(Temperature ~ Amplitude * sin((2 * pi / (365.26 * 24 * 60 * 60)) * (Time_in_Numeric_Form + Phase_Shift)) + Intercept, Data_Frame_1, start = c(Amplitude = 20, Phase_Shift = 1000, Intercept = 10))     

summary(Model_2)     
coef(Model_2)     
     
     
Amplitude_2 <- coef(Model_2)["Amplitude"]
Phase_Shift_2 <- coef(Model_2)["Phase_Shift"]
Intercept_2 <- coef(Model_2)["Intercept"]    
     
Data_Frame_1$Predicted_Temperature_1_Year_Period <- Amplitude_2 * sin((2 * pi / (365.26 * 24 * 60 * 60)) *
                                                                        (Data_Frame_1$Time_in_Numeric_Form + Phase_Shift_2) + Intercept_2)     
     
#Plot both on top of one another so you can see if good fit

Data_Frame_1$Transformed_Values_1_Year_Period <- (Data_Frame_1$Temperature - Intercept_2) / Amplitude_2
range(Data_Frame_1$Transformed_Values_1_Year_Period)

Data_Frame_1$Model_1_Residuals <- Data_Frame_1$Temperature - Data_Frame_1$Predicted_Temperature_1_Year_Period


#Now for the daily scale
Model_3 <- nls(Model_1_Residuals ~ Amplitude * sin((2 * pi / (24 * 60 * 60)) * (Time_in_Numeric_Form + Phase_Shift)) + Intercept, Data_Frame_1, start = c(Amplitude = 4, Phase_Shift = 24, Intercept = 0))

Model_3
summary(Model_3)

coef(Model_3)     


Amplitude_3 <- coef(Model_3)["Amplitude"]
Phase_Shift_3 <- coef(Model_3)["Phase_Shift"]
Intercept_3 <- coef(Model_3)["Intercept"]    

Data_Frame_1$Transformed_Values_1_Day_Period <- (Data_Frame_1$Model_1_Residuals - Intercept_3) / Amplitude_3


#Next example. What to do if doesn't follow perfect sinwave. i.e. vegitation growth now symmetrical in fall and spring. Another example, moss grows unevenly on east vs west side.

#East/West and North/South. Summer/Winter and Spring Fall. All orthogonal


#Dataframe measuring moss density around a trunk (360 and 0 the same)
     
Model_4 <- nls(Moss_Biomass ~ Amplitude * cos((2 * pi / 360) * (Orientation + Phase_Shift)) - Intercept, 
               Data_Frame_5, start = c(Amplitude = 10, Phase_Shift = 2 * pi / 180, Intercept = 15))

summary(Model_4)
coef(Model_4)

Amplitude_4 <- coef(Model_4)["Amplitude"]
Phase_Shift_4 <- coef(Model_4)["Phase_Shift"]
Intercept_4 <- coef(Model_4)["Intercept"] 


Data_Frame_5$Predicted_Moss_Biomass_North_South_Component <- Amplitude_4 * cos((2 * pi / 360) * (Data_Frame_5$Orientation + Phase_Shift_4)) + Intercept_4

North_South_Component_Trendline_Horizontal_Axis_Values <- seq(45, 360, by = 0.001)
North_South_Component_Trendline_Vertical_Axis_Values <- Amplitude_4 * cos((2 * pi / 360) * (North_South_Component_Trendline_Horizontal_Axis_Values + Phase_Shift_4)) + Intercept_4

North_South_Component_Trendline <- data.frame(
  Horizontal_Axis_Values = North_South_Component_Trendline_Horizontal_Axis_Values,
  Vertical_Axis_Values = North_South_Component_Trendline_Vertical_Axis_Values)

with(North_South_Component_Trendline, lines(Horizontal_Axis_Values, Vertical_Axis_Values, col = 2, lwd = 4))

plot(Moss_Biomass ~ Orientation, data = Data_Frame_5)
with(North_South_Component_Trendline, lines(Horizontal_Axis_Values, Vertical_Axis_Values, col = 2, lwd = 4))

Data_Frame_5$Transformed_Values_North_South_Component <- (Data_Frame_5$Moss_Biomass - Intercept_4) / Amplitude_4

#Here the period is the same. We NEED to be orthogonal so our phase shift has to be 90 degrees. So when we do EW model, we have to set phase shift 


#Get residuals then define phase shift to be 90 degrees from original one
Phase_Shift_5 <- Phase_Shift_4 + (2 / pi)

Data_Frame_5$North_South_Model_Residual_Moss_Biomass <- Data_Frame_5$Moss_Biomass - Data_Frame_5$Predicted_Moss_Biomass_North_South_Component

plot(North_South_Model_Residual_Moss_Biomass ~ Orientation, data = Data_Frame_5)

Model_5 <- nls(North_South_Model_Residual_Moss_Biomass ~ Amplitude * cos((2 * pi / 360) * (Orientation + Phase_Shift_5)) + 0, Data_Frame_5, start = c(Amplitude = 2))

summary(Model_5)
coef(Model_5)

#Generate tranformed values, This will also be a second tranformed data col. EW transformation (as opposed to NW)



