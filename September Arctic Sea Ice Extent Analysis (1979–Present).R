#setting the working directory
setwd("C:\\Users\\KIIT\\OneDrive\\Desktop\\SY. MSC ECONOMICS DA\\EXPLORATORY DATA")
getwd()

#loading libraries
library(ggplot2)
library(dplyr)

#importing data
sept_ice <- read.csv("N_09_extent_v4.0.csv")

#clean column names
colnames(sept_ice) <- trimws(colnames(sept_ice))

#coverting to time series data
ice_ts <- ts(sept_ice$extent, start = min(sept_ice$year), frequency = 1)

#plot full time series
plot(ice_ts,
     main = "September Arctic Sea Ice Extent (1979–Present)",
     xlab = "Year", ylab = "Extent (million sq km)",
     col = "blue", lwd = 2)

#same plot using ggplot2
ggplot(sept_ice, aes(x = year, y = extent)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkblue") +
  labs(title = "September Arctic Sea Ice Extent Over Time",
       x = "Year", y = "Extent (million sq km)") +
  theme_minimal()

#Add a linear line trend
ggplot(sept_ice, aes(x = year, y = extent)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "September Arctic Sea Ice Extent with Trend Line",
       x = "Year", y = "Extent (million sq km)") +
  theme_minimal()

#quantify the trend (linear regression)
model <- lm(extent ~ year, data = sept_ice)
summary(model)

#Looking at the "year" coefficient-this tells us the average
#decline in extent per year (it will be negative)

#period comparison
sept_ice$period <- cut(sept_ice$year,
                       breaks = c(1978, 1990, 2000, 2010, 2025),
                       labels = c("1979-1990", "1991-2000", "2001-2010", "2011-Present"))
aggregate(extent ~ period, data = sept_ice, mean)
ggplot(sept_ice, aes(x = period, y = extent)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Sea Ice Extent by Period",
       x = "Period", y = "Extent (million sq km)") +
  theme_minimal()
