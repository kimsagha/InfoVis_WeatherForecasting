# libraries
library(ggplot2)
library(gghighlight)
library(plotly)
library(zoo)

##############################################################################################################
# LOAD DATA
##############################################################################################################

message("loading data...")

# import complete weather forecasting dataset from csv file
weatherForecasting <- read.table("Bias_correction_ucl.csv", quote="\"", header=T, comment.char="", sep=",")

##############################################################################################################
# DATA MANIPULATION
##############################################################################################################

message("filtering data...")

# remove columns that are not needed
weatherForecasting <- subset(weatherForecasting, select = -c(5,7,8,11,13:15,17:20,25))

# remove rows with null values
weatherForecasting <- na.omit(weatherForecasting)

# Pre-process data for Question 1 ##############################################
# create matrix of average max and min temperatures per station
maxMinTemp <- matrix(1:125, nrow = 25, ncol = 5)
for (i in 1:25) {
  currentMaxTempAvg <- round(mean(data.frame(weatherForecasting[weatherForecasting$station == i, "Present_Tmax"])[, 1]), digits = 0)
  currentMinTempAvg <- round(mean(data.frame(weatherForecasting[weatherForecasting$station == i, "Present_Tmin"])[, 1]), digits = 0)
  currentHeatFlux <- round(mean(data.frame(weatherForecasting[weatherForecasting$station == i, "LDAPS_LH"])[, 1]), digits = 0)
  currentSolarRadiation <- round(mean(data.frame(weatherForecasting[weatherForecasting$station == i, "Solar.radiation"])[, 1]), digits = 0)
  maxMinTemp[i, 1] <- i
  maxMinTemp[i, 2] <- currentMaxTempAvg
  maxMinTemp[i, 3] <- currentMinTempAvg
  if (currentHeatFlux < 40) {
    maxMinTemp[i, 4] = "-20 - 39 W/m2"
  } else if ((currentHeatFlux >= 40) & (currentHeatFlux < 100)) {
    maxMinTemp[i, 4] = "40 - 99 W/m2"
  } else if ((currentHeatFlux >= 100) & (currentHeatFlux < 160)) {
    maxMinTemp[i, 4] = "100 - 159 W/m2"
  } else if ((currentHeatFlux >= 160) & (currentHeatFlux < 220)) {
    maxMinTemp[i, 4] = "160 - 219 W/m2"
  }
  maxMinTemp[i, 5] <- currentSolarRadiation
}

# transform matrix into dataframe
maxMinTemp.df <- as.data.frame(maxMinTemp)

# label the data
names(maxMinTemp.df)[1] <- "Station"
names(maxMinTemp.df)[2] <- "MaxTemp"
names(maxMinTemp.df)[3] <- "MinTemp"
names(maxMinTemp.df)[4] <- "HeatFlux"
names(maxMinTemp.df)[5] <- "SolarRadiation"

slopes <- round(unique(weatherForecasting[["Slope"]]), digits = 3)

# reformat dataframe columns
maxMinTemp.df$Station <- as.numeric(as.character(maxMinTemp.df$Station))
maxMinTemp.df$MaxTemp <- as.numeric(as.character(maxMinTemp.df$MaxTemp))
maxMinTemp.df$MinTemp <- as.numeric(as.character(maxMinTemp.df$MinTemp))
maxMinTemp.df$HeatFlux <- factor(maxMinTemp.df$HeatFlux, levels = c("-20 - 39 W/m2", "40 - 99 W/m2", "100 - 159 W/m2", "160 - 219 W/m2"), ordered = TRUE)
maxMinTemp.df$SolarRadiation <- as.numeric(as.character(maxMinTemp.df$SolarRadiation))
maxMinTemp.df$Station <- reorder(maxMinTemp.df$Station, maxMinTemp.df$MaxTemp)

# Pre-process data for Question 2 ##############################################
# create a vector of all the unique dates in the data set
dates <- unique(weatherForecasting[["Date"]])

# create matrix of average max temp across all stations per day
maxTempDaily <- matrix(1:2456, nrow=614, ncol=4)
for (i in 1:307) { # add measured temps
  maxTempMeasured = round(mean(data.frame(weatherForecasting[weatherForecasting$Date == dates[i], "Present_Tmax"])[, 1]), digits = 0)
  date <- substr(dates[i], 6, 10) # show only month and day of date to be able to plot all years in parallel
  maxTempDaily[i, 1] <- date
  maxTempDaily[i, 2] <- substring(dates[i], 1, 4) # add year in separate column
  maxTempDaily[i, 3] <- maxTempMeasured
  maxTempDaily[i, 4] <- "Measured"
}
j <- 1
for (i in 308:614) { # add forecasted temps
  maxTempForecasted = round(mean(data.frame(weatherForecasting[weatherForecasting$Date == dates[j-1], "Next_Tmax"])[, 1]), digits = 0)
  date <- substr(dates[j], 6, 10) # show only month and day of date to be able to plot all years in parallel
  maxTempDaily[i, 1] <- date
  maxTempDaily[i, 2] <- substring(dates[j], 1, 4) # add year in separate column
  maxTempDaily[i, 3] <- maxTempForecasted
  maxTempDaily[i, 4] <- "Forecasted"
  j <- j + 1
}

# transform matrix into dataframe
maxTempDaily.df <- as.data.frame(maxTempDaily)

# label the data
names(maxTempDaily.df)[1] <- "Date"
names(maxTempDaily.df)[2] <- "Year"
names(maxTempDaily.df)[3] <- "MaxTemp"
names(maxTempDaily.df)[4] <- "Type"

# reformat dataframe columns
maxTempDaily.df$Date = as.factor(maxTempDaily.df$Date) # Year removed from date --> format can't be date anymore --> convert to factor
maxTempDaily.df$MaxTemp <- as.numeric(as.character(maxTempDaily.df$MaxTemp))

# calculate average difference between daily measured and forecasted temperatures
diff <- 0
for(i in 1:307) {
  if(i != 1) {
    diff <- diff + maxTempDaily.df$MaxTemp[i+307] - maxTempDaily.df$MaxTemp[i]
  }
}
avgDiff <- round(diff/307, digits=1)
avgDiff <- paste(avgDiff, "\u00B0C")

##############################################################################################################
# BASIC ANALYSIS AND VISUALIZATION
##############################################################################################################

message("Q1: Which weather station reached on average the highest daily max air temperature and which one reached the lowest daily min air temperature?")

# plot min and max temperature per station
p1 <- ggplot() +
  ylim(c(0, 35)) +
  geom_segment(data = maxMinTemp.df,
               aes(x = Station,
                   xend = Station,
                   y = MinTemp,
                   yend = MaxTemp),
               color = "grey") +
  geom_point(data = maxMinTemp.df,
             aes(x = Station,
                 y = MaxTemp,
                 color = "Max"),
             size = 4) +
  geom_point(data = maxMinTemp.df,
             aes(x = Station,
                 y = MinTemp,
                 color = "Min"),
             size = 4) +
  labs(title = "Average Daily Temperatures for all Weather Stations in Seoul During Summers 2013-2014",
       x = "Weather Station",
       y = "Temperature (\u00B0C)",
       color = "Temp") +
  scale_color_manual(values = c("red3", "blue3")) +
  theme_bw()

# make plot interactive
p1 <- ggplotly(p1)

show(p1)

message("Q1a: What was the average daily incoming solar radiation and heat flux per station and was there a relationship between them and the air temperature? Moreover, did the slope of the stations affect this relationship?")

maxMinTemp.df$Station <- reorder(maxMinTemp.df$Station, slopes)

# plot min and max temperature for all stations using color and size to visualize solar radiation and heat flux
p1a <- ggplot(maxMinTemp.df) +
  labs(title = "Average Daily Temperatures for all Weather Stations in Seoul During Summers 2013-2014, Sorted in Ascending Order by Slope",
       subtitle = "Max temperatures plotted above the min temperatures for each station.",
       x = "Weather Station", y = "Temperature (\u00B0C)") +
  ylim(c(0, 35)) +
  geom_segment(aes(x = Station,
                   xend = Station,
                   y = MinTemp,
                   yend = MaxTemp),
               color = "grey") +
  geom_point(aes(x = Station,
                 y = MinTemp,
                 color = SolarRadiation,
                 size = HeatFlux,
                 group = slopes)) +
  geom_point(aes(x = Station,
                 y = MaxTemp,
                 color = SolarRadiation,
                 size = HeatFlux,
                 group = slopes)) +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  theme_bw()

show(p1a)

##############################################################################################################

message("Q2: How did the temperature change over summer from 2013-2017 and how accurate were the weather forecasts?")

# plot average daily max temperature and forecasted temperature during summers 2013-2017 as a line graph for each year
p2 <- ggplot() +
  labs(title = "Average Daily Max Temperature During Summers 2013-2017 in Seoul",
       x = "Date (mm-dd)",
       y = "Temperature (\u00B0C)") +
  ylim(c(0, 35)) +
  geom_line(data = maxTempDaily.df,
            aes(x = Date,
                y = MaxTemp,
                group = Year,
                color = Type)) +
  geom_text(aes(x="07-07", y=33, color="Average Difference"),
            label = avgDiff,
            size = 3.5) +
  scale_color_manual(values = c("chocolate1", "gray", "darkolivegreen")) +
  scale_x_discrete(breaks = c("06-30", "07-14", "07-28", "08-11", "08-25")) +
  theme_bw() +
  facet_grid(~ Year, scale = "free_y")

# make plot interactive
p2 <- ggplotly(p2)

show(p2)

message("Q2a: When did the biggest changes in temperature over summer in 2013-2017 in Seoul happen and what was the general trend?")

maxTempDaily.df <- maxTempDaily.df[, -4] # remove Type since measured temperature is only needed from now on
maxTempDaily.df <- maxTempDaily.df[1:307, ] # remove the rows with forecasted MaxTemp values

# add annual rolling averages with 1 week (7 days) window size to dataframe
maxTempDaily.df$RollingAverage = c(1:307)
maxTempDaily.df$RollingAverage = NA # set the values to be null (so non-existent values won't show up in the plot)
maxTempDaily.df$RollingAverage[7:61] <- rollmean(maxTempDaily.df[maxTempDaily.df$Year == "2013", "MaxTemp"], 7, align = "right")
maxTempDaily.df$RollingAverage[68:123] <- rollmean(maxTempDaily.df[maxTempDaily.df$Year == "2014", "MaxTemp"], 7, align = "right")
maxTempDaily.df$RollingAverage[130:185] <- rollmean(maxTempDaily.df[maxTempDaily.df$Year == "2015", "MaxTemp"], 7, align = "right")
maxTempDaily.df$RollingAverage[192:245] <- rollmean(maxTempDaily.df[maxTempDaily.df$Year == "2016", "MaxTemp"], 7, align = "right")
maxTempDaily.df$RollingAverage[252:307] <- rollmean(maxTempDaily.df[maxTempDaily.df$Year == "2017", "MaxTemp"], 7, align = "right")
maxTempDaily.df$RollingAverage <- round(maxTempDaily.df$RollingAverage[1:307], digits = 0)

# calculate standard deviation and mean
myStd = function(x) {sd(x) * sqrt((length(x) - 1) / length(x))}
StdTemp <- myStd(maxTempDaily.df$MaxTemp)
MeanTemp <- round(mean(maxTempDaily.df$MaxTemp), digits = 0)
low <- round(MeanTemp - StdTemp, digits = 0) # max point within standard deviation interval
high <- round(MeanTemp + StdTemp, digits = 0) # min point within standard deviation interval

p2a <- ggplot(maxTempDaily.df) +
  geom_rect(aes(xmin = Date[1], xmax = Date[307], ymin = low, ymax = high, color = "Standard Deviation"), fill = "lightgoldenrod1", alpha = 0.5) +
  labs(title = "Average Daily Max Temperature During Summers 2013-2017 in Seoul",
       x = "Date (mm-dd)", y = "Temperature (\u00B0C)") +
  ylim(c(0, 35)) +
  geom_line(aes(x = Date,
                y = MaxTemp,
                group = Year,
                color = "Measured")) +
  geom_line(aes(x = Date,
                y = RollingAverage,
                group = Year,
                color = "Rolling Average"),
            linetype = "dotted") +
  geom_line(aes(x = Date,
                y = MeanTemp,
                group = Year,
                color = "AverageMeasured"),
            linetype = "dotted") +
  scale_color_manual(values = c("chocolate1", "darkolivegreen", "gray50", "lightgoldenrod1")) +
  scale_x_discrete(breaks = c("06-30", "07-14", "07-28", "08-11", "08-25")) +
  theme_bw() +
  facet_grid(~ Year, scale = "free_y")

# make plot interactive
p2a <- ggplotly(p2a)

show(p2a)

##############################################################################################################

message("Q3: Which stations were likely to be windier during summer?")

weatherForecasting$station <- reorder(weatherForecasting$station, weatherForecasting$LDAPS_WS)

p3 <- ggplot(weatherForecasting,
             aes(x = station,
                 y = LDAPS_WS)) +
  labs(title = "Summer-time Wind Speeds in Seoul Weather Stations, 2013-2017",
       x = "Station", y = "Wind Speed (m/s)") +
  geom_violin(color = "darkolivegreen3",
              fill = "darkolivegreen3") +
  theme_bw() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 2, mapping = aes(color = "Average points")) +
  scale_color_manual(values = c("darkolivegreen"))

p3 <- ggplotly(p3)

show(p3)

message("Q3a: Was there a relationship between the wind speeds and the elevation of the stations?")

weatherForecasting$DEM <- round(weatherForecasting$DEM, digits = 2)

p3a <- ggplot(weatherForecasting,
              aes(x = station,
                  y = LDAPS_WS,
                  fill = DEM)) +
  labs(title = "Summer-time Wind Speeds in Seoul Weather Stations, 2013-2017",
       x = "Station", y = "Wind Speed (m/s)") +
  geom_violin() +
  theme_bw() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color = "white", fill = "white")

# make plot interactive
p3a <- ggplotly(p3a)

show(p3a)

##############################################################################################################

message("Q4: Was there a relationship between the forecasted cloud coverage and the forecasted precipitation in the mornings from 6-11am?")

weatherForecasting$LDAPS_CC2 <- round(weatherForecasting$LDAPS_CC2, digits = 2)
weatherForecasting$LDAPS_PPT2 <- round(weatherForecasting$LDAPS_PPT2, digits = 2)

p4 <- ggplot(weatherForecasting) +
  labs(title = "Precipitation Based on Different Levels of Cloud Coverage During Summer Mornings",
       x = "Cloud Coverage (%)", y = "Precipitation (%)") +
  geom_point(aes(x = LDAPS_CC2,
                 y = LDAPS_PPT2),
             color = "gray40",
             size = 2,
             alpha = 0.2) +
  geom_smooth(aes(x = LDAPS_CC2,
                  y = LDAPS_PPT2),
              method = "gam",
              formula = y ~s(x),
              se= FALSE,
              color = "black") +
  theme_bw()

# make plot interactive
p4 <- ggplotly(p4)

show(p4)

message("Q4a: Was the humidity related to the levels of cloud coverage and precipitation in the mornings?")

weatherForecasting$LDAPS_RHmax <- round(weatherForecasting$LDAPS_RHmax, digits = 2)

p4a <- ggplot(weatherForecasting) +
  labs(title = "Precipitation Based on Different Levels of Cloud Coverage During Summer Mornings",
       x = "Cloud Coverage (%)", y = "Precipitation (%)") +
  geom_point(aes(x = LDAPS_CC2,
                 y = LDAPS_PPT2,
                 color = LDAPS_RHmax),
             size = 2) +
  scale_colour_gradient(name = "Max Humidity", low = "aliceblue", high = "blue4", na.value = NA) +
  theme_bw()

# make plot interactive
p4a <- ggplotly(p4a)

show(p4a)