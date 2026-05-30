library(tidyverse)
library(rgl)


# Put some data in here
x <- c(1,2,3,4)
y <- c(9.5, 11, 19.6, 20)

# Plot it - looks kind of like a line
plot(x, y)


#Let's look through a few parameter values for the slope and see what the result looks like

# Make a search grid of parameter values
beta0 <- seq(0,10,0.1)
beta1 <- seq(0,10,0.1)
betas <- expand.grid(beta0 = beta0, 
                     beta1 = beta1)

# Get SSR values for each beta set
for (i in 1:nrow(betas)){
  
  beta0 <- betas$beta0[i]
  beta1 <- betas$beta1[i]
  SSR <- sum((y - (beta0 + beta1*x))^2)
  
  betas$SSR[i] <- SSR
  
}

# Plot the SSR for each pair of values
plot3d( 
  x=betas$beta0, y=betas$beta1, z=betas$SSR, 
  type = 's', 
  radius = 5)
rglwidget()


# Surfaces are difficult to visualize, so let's break it down into   # the components for ease

betas_0 <- betas %>%group_by(beta0) %>% summarize(SSRmin = min(SSR))
betas_1 <- betas %>%group_by(beta1) %>% summarize(SSRmin = min(SSR))

# Notice that the minimum SSR happens when beta0 = 5
plot(betas_0$beta0, betas_0$SSRmin)

# Notice that the minimum SSR happens when beta1 = 4
plot(betas_1$beta1, betas_1$SSRmin)

# We get the same result using the lm() function that uses OLS to 
# find the parameter values
summary(lm(y ~ x))





#----------------------------------------------------------------#



source("VecTraits_Dataset_Access.R")

mosquito_df <- getDataset(578) #this returns a list of data frames in case we ask for several data sets.
df <- mosquito_df[[1]] #we can extract our data frame like this


# Make a data set of the aggregated mean values
development_rate_mean <- df %>% 
  filter(SecondStressorValue == 165) %>%
  group_by(Interactor1Temp) %>%
  summarise(Trait = mean(1 / OriginalTraitValue), .groups = "drop") %>%
  mutate(curve_ID = factor(1), Temp = Interactor1Temp)



# Make a dataset of the individual-level values
development_rate_individuals <- df %>%
  filter(SecondStressorValue == 165) %>%
  mutate(curve_ID = factor(2),
         Temp = Interactor1Temp,
         Trait = 1 / OriginalTraitValue)


ggplot() +
  geom_jitter(data = development_rate_individuals,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  theme_bw()


briere <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
              start = list(a = 1, tmin = 22, tmax = 35),
              data = development_rate_individuals)

# Full summary
summary(briere)


# Generate predicted values to graph
tempDat <- data.frame(Temp = 
                        seq(min(development_rate_individuals$Temp),
                            max(development_rate_individuals$Temp),
                            length.out = 100))

d_preds <- predict(briere, newdata = tempDat)
tempDat$preds <- d_preds


tempDat <- data.frame(Temp = 
                        seq(min(development_rate_individuals$Temp),
                            max(development_rate_individuals$Temp),
                            length.out = 100))

d_preds <- predict(briere, newdata = tempDat)
tempDat$preds <- d_preds

# Graph
ggplot() +
  geom_jitter(data = development_rate_individuals,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  geom_line(data = tempDat,
            aes(x = Temp, y = preds), color = "blue")+
  theme_bw()

# Extract confidence intervals
confint(briere)






#----------------------------------------------------------------#




source("VecTraits_Dataset_Access.R")

tick2_df <- getDataset(640) #this returns a list of data frames in case we ask for several data sets.
df <- tick2_df[[1]] #we can extract our data frame like this


# Make a data set of the aggregated mean values
development_rate_mean <- df %>% 
  group_by(Interactor1Temp) %>%
  summarise(Trait = mean(1 / OriginalTraitValue), .groups = "drop") %>%
  mutate(curve_ID = factor(1), Temp = Interactor1Temp)


# Make a dataset of the individual-level values
development_rate_individuals <- df %>%
  mutate(curve_ID = factor(2),
         Temp = Interactor1Temp,
         Trait = 1 / OriginalTraitValue)


ggplot() +
  geom_jitter(data = development_rate_individuals,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  theme_bw()


briere <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
              data = development_rate_individuals)

# Full summary
summary(briere)


# Generate predicted values to graph
tempDat <- data.frame(Temp = 
                        seq(min(development_rate_individuals$Temp),
                            max(development_rate_individuals$Temp),
                            length.out = 100))

d_preds <- predict(briere, newdata = tempDat)
tempDat$preds <- d_preds


tempDat <- data.frame(Temp = 
                        seq(min(development_rate_individuals$Temp),
                            max(development_rate_individuals$Temp),
                            length.out = 100))

d_preds <- predict(briere, newdata = tempDat)
tempDat$preds <- d_preds

# Graph
ggplot() +
  geom_jitter(data = development_rate_individuals,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  geom_line(data = tempDat,
            aes(x = Temp, y = preds), color = "blue")+
  theme_bw()

# Extract confidence intervals
confint(briere)


tick2_df$results$OriginalTraitValue

plot(tick2_df$results$Interactor1Size, tick2_df$results$OriginalTraitValue)





