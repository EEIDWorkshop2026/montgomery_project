library(WDI)
library(dplyr)
library(tidyr)
library(e1071)
library(ggplot2)


nyTemps <- data.frame(time = seq(1,nrow(airquality),1),
                      temp = airquality$Temp)

ggplot(data = nyTemps, aes(x = time, y = temp))+
  geom_line()

ld<-as.vector(ldeaths)
plot(ld, xlab="month", ylab="deaths", type="l", 
     col=4, lwd=2)

# Build a data set with the deaths data and the lag-1 response
autoDat <- data.frame(time = seq(1, length(ldeaths),1),
                      deaths = as.vector(ldeaths),
                      lagDeaths = lag(as.vector(ldeaths),1))

# Remove the NA value that is introduced
autoDat <- autoDat %>% na.omit(lagDeaths)

# Calculate the autocorrelation
corr <- round(cor(autoDat$deaths, autoDat$lagDeaths), 2)

# Plot the relationship between deaths this month and last month
ggplot(data = autoDat, aes(x = deaths, y = lagDeaths))+
  geom_point()+
  annotate("text",
           x = 1500, y = 3800, label = paste("corr = ", corr ),
           color = "red")

# Deaths data
acf(ldeaths)

#NY Temps data
acf(airquality$Temp)


# Fit the model with time as a predictor
tempreg <- lm(temp ~ time, data = nyTemps)

# Check out the summary
summary(tempreg)

# Histogram to check for normality - looks great!
hist(tempreg$residuals)

# Predicted vs. Residuals plot to look for patterns - definitely something going on here!
plot(tempreg$residuals, predict(tempreg))

# Add time^2 to the data set
nyTemps$timeSq <- nyTemps$time^2

# Fit the model with time as a predictor
tempreg2 <- lm(temp ~ time + timeSq, data = nyTemps)

# Check out the summary - the squared term is significant!
summary(tempreg2)

# Predicted vs. Residuals plot to look for patterns - better but still weird
plot(tempreg2$residuals, predict(tempreg2))

acf(tempreg2$residuals)

# Add a lag-1 response to the data set
nyTemps$tempLag1 <- lag(nyTemps$temp,1)

# Get rid of the NA that was introduced
nyTemps <- nyTemps %>% na.omit(tempLag1)

# Fit our autoregressive regression model
tempregAuto <- lm(temp ~ time + timeSq + tempLag1, data = nyTemps)

# The lag is significant as expected!
summary(tempregAuto)

# Make an ACF plot of the residuals
acf(tempregAuto$residuals)

# Histogram to check for normality - looks great!
hist(tempregAuto$residuals)

# Predicted vs. Residuals plot to look for patterns - looks a lot better! No clear patterns
plot(tempregAuto$residuals, predict(tempregAuto))


# Create a time variable in the ldeaths data
tmax<-length(ldeaths)
t <- 2:tmax

# Create a data frame with the sine and cosine terms
YX <- data.frame(ld=ld[2:tmax], ldpast=ld[1:(tmax-1)], t=t,
                 sin12=sin(2*pi*t/12), cos12=cos(2*pi*t/12))

# Fit the model
lunglm <- lm(ld ~ t + ldpast + sin12 + cos12, data=YX)

# The seasonal terms are significant!
summary(lunglm)


# Add predicted values
YX$preds <- predict(lunglm)

# Plot
ggplot(data = YX, aes(x = t))+
  geom_line(aes(y = ld), color = "lightblue", linewidth = 1.5)+
  geom_line(aes(y = preds), color = "indianred", linewidth = 1.5, linetype = 2)+
  theme_bw()



# LOESS Model
model <- naiveBayes(deaths ~ ., data = airquality)
lung_loess <- loess(ld ~ t + ldpast, data=YX)


smoothed_values <- predict(lung_loess)

# Plot
plot(YX$t, YX$ld, pch = 19, xlab="t", ylab="ld")
lines(YX$t, smoothed_values, col = "blue", lwd = 2)

summary(lung_loess)

mean(lunglm$residuals^2)
mean(lung_loess$residuals^2)
